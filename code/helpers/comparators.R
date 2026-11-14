# One shared split and evaluation population. Both team perspectives are used
# for each first phase; Win=0.5 for a draw. No held-out outcomes select models.
team_rows <- function(d) {
  do.call(rbind,lapply(c('Home','Away'),function(side) {
    poss <- d$Team_In_Poss==side
    margin <- ifelse(poss,d$Points_Difference,-d$Points_Difference)
    data.frame(ID=d$ID,game_id=d$game_id,side=side,
      actual=ifelse(poss,d$Win,1-d$Win),
      meter_line=ifelse(poss,d$meter_line,100-d$meter_line),
      margin=margin,seconds=pmax(d$Seconds_Remaining,0),
      cards=ifelse(poss,d$Card_Diff,-d$Card_Diff),possession=as.integer(poss),
      adjusted_diff=margin/sqrt(pmax(d$Seconds_Remaining,0)+1))
  }))
}

fit_comparators <- function(train, test, seed=123) {
  predictors <- c('meter_line','margin','seconds','cards','possession','adjusted_diff')
  logistic <- glm(actual ~ meter_line + margin + cards + possession + adjusted_diff,
                   data=train,family=quasibinomial())
  forest <- ranger::ranger(actual ~ .,data=train[c('actual',predictors)],
    num.trees=250,mtry=3,min.node.size=25,seed=seed,num.threads=2)
  game_order <- unique(train$game_id)
  inner <- train$game_id %in% head(game_order,floor(.8*length(game_order)))
  x <- as.matrix(train[predictors]); xt <- as.matrix(test[predictors])
  params <- list(objective='reg:logistic',eval_metric='rmse',max_depth=3,eta=.1,
    min_child_weight=20,gamma=5,seed=seed,nthread=2,
    monotone_constraints=c(0,1,0,1,0,1))
  din <- xgboost::xgb.DMatrix(x[inner,,drop=FALSE],label=train$actual[inner])
  dv <- xgboost::xgb.DMatrix(x[!inner,,drop=FALSE],label=train$actual[!inner])
  selected <- xgboost::xgb.train(params=params,data=din,nrounds=2000,
    evals=list(validation=dv),early_stopping_rounds=30,verbose=0)
  best_iteration <- xgboost::xgb.attr(selected, 'best_iteration')
  if (!length(best_iteration)) stop('XGBoost did not return a selected iteration')
  rounds <- as.integer(best_iteration) + 1L # core attribute uses zero-based indexing
  fit <- xgboost::xgb.train(params=params,
    data=xgboost::xgb.DMatrix(x,label=train$actual),nrounds=rounds,verbose=0)
  result <- test[c('ID','game_id','side','actual')]
  result$Logistic <- predict(logistic,test,type='response')
  result$RandomForest <- predict(forest,test)$predictions
  result$XGBoost <- predict(fit,xgboost::xgb.DMatrix(xt))
  result$Naive <- .5
  # Training prevalence is 0.5 under symmetric team expansion. Retain the
  # separately estimated home/away result rates as the nontrivial constant.
  home_rate <- mean(train$actual[train$side=='Home'])
  result$HomePrevalence <- ifelse(test$side=='Home',home_rate,1-home_rate)
  list(predictions=result,xgb_rounds=rounds,home_prevalence=home_rate,
       fit=list(logistic=logistic,forest=forest,xgboost=fit))
}
