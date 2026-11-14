# All reported errors target E[1(win) + 0.5*1(draw)], with common observations.
# Bootstrap intervals below resample held-out games, conditional on fitted models.
prepare_calibration_data <- function(phase_data, n_samples = Inf) {
  d <- phase_data
  if (is.finite(n_samples) && nrow(d) > n_samples) {
    set.seed(123); d <- d[sample.int(nrow(d), n_samples), ]
  }
  d$team_poss <- d$Team_In_Poss
  d$poss_team_won <- d$Win
  d
}

performance_summary <- function(predictions, n_boot = 2000L, seed = 123) {
  models <- setdiff(names(predictions), c('ID','game_id','side','actual'))
  stopifnot(!anyNA(predictions), all(as.matrix(predictions[models]) >= 0),
            all(as.matrix(predictions[models]) <= 1))
  errors <- sapply(models,function(m) (predictions[[m]] - predictions$actual)^2)
  sums <- rowsum(errors, predictions$game_id)
  counts <- as.numeric(table(factor(predictions$game_id,levels=rownames(sums))))
  set.seed(seed)
  draws <- replicate(n_boot, sample.int(nrow(sums), nrow(sums), replace=TRUE))
  boot <- apply(draws,2,function(i) colSums(sums[i,,drop=FALSE])/sum(counts[i]))
  ci <- t(apply(boot,1,quantile,c(.025,.975)))
  mse <- colMeans(errors)
  result <- data.frame(model=models,mse=mse,rmse=sqrt(mse),lower=ci[,1],upper=ci[,2],row.names=NULL)
  baseline <- mse[match('Naive',models)]
  result$error_reduction <- 1-result$mse/baseline
  differences <- t(apply(sweep(boot,2,boot[match('Markov',models),],'-'),1,quantile,c(.025,.975)))
  result$mse_minus_markov <- mse - mse[match('Markov',models)]
  result$difference_lower <- differences[,1]; result$difference_upper <- differences[,2]
  result
}

calibration_summary <- function(predictions, model='Markov', n_boot=2000L, seed=124) {
  p <- predictions[[model]]; y <- predictions$actual
  stopifnot(!anyNA(p), all(p >= -1e-12 & p <= 1+1e-12))
  p <- pmin(pmax(p,0),1)
  bins <- cut(p,seq(0,1,.1),include.lowest=TRUE,labels=FALSE)
  games <- unique(predictions$game_id)
  set.seed(seed)
  draws <- replicate(n_boot,sample(games,length(games),replace=TRUE))
  do.call(rbind,lapply(sort(unique(bins)),function(b) {
    take <- bins==b
    totals <- rowsum(cbind(y=y[take],n=1), predictions$game_id[take])
    rates <- apply(draws,2,function(g) {
      z <- totals[match(g,rownames(totals)),,drop=FALSE]
      sum(z[,1],na.rm=TRUE)/sum(z[,2],na.rm=TRUE)
    })
    limits <- quantile(rates,c(.025,.975),na.rm=TRUE)
    data.frame(bin=b,predicted=mean(p[take]),observed=mean(y[take]),n=sum(take),
               games=length(unique(predictions$game_id[take])),lower=limits[1],upper=limits[2])
  }))
}
