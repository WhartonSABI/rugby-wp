# Corrected analysis. Run from the repository root:
# Rscript code/markov/run_sim.R [output-directory]
# The historical entry-point name is retained; predictions now use absorption
# equations. Simulation supplies only illustrative trajectories and checks.
suppressPackageStartupMessages({
  source('code/helpers/data-prep.R'); source('code/helpers/state-space.R')
  source('code/helpers/absorption.R'); source('code/helpers/markov-sim.R')
  source('code/helpers/calibration.R'); source('code/helpers/comparators.R')
  library(ggplot2)
})
args <- commandArgs(trailingOnly=TRUE)
out <- if(length(args)) args[1] else 'output/markov-revision-20261113'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
set.seed(123)
d <- prepare_transition_data(prepare_phase_data())
games <- unique(d$game_id); training <- head(games,floor(.8*length(games)))
train <- d[d$game_id %in% training,]; test <- d[!d$game_id %in% training,]
model <- build_state_space(train)
write.csv(data.frame(game_id=games,split=ifelse(games %in% training,'train','test')),
          file.path(out,'split.csv'),row.names=FALSE)
write.csv(model$support,file.path(out,'state_support.csv'),row.names=FALSE)
write.csv(d[d$reward!=0 & d$reward!=d$points,c('ID','game_id','Outcome','reward')],
          file.path(out,'source_label_disagreements.csv'),row.names=FALSE)
train_rows <- team_rows(train[train$first_phase==1,])
test_rows <- team_rows(test[test$first_phase==1,])
queries <- rbind(train[train$first_phase==1,], test[test$first_phase==1,])
# Choose a numerical grid based on unresolved probability mass, never labels.
for (bound in c(100L,150L,200L,300L)) {
  cat('Absorption grid +/-',bound,'\n')
  chain <- compile_absorbing_chain(model,bound)
  index <- chain$index(as.integer(queries$time_state),queries$meter_line,queries$home_margin,
                       ifelse(queries$Team_In_Poss=='Home',1L,-1L))
  max_unresolved <- max(chain$probabilities[index,'unresolved'])
  cat('Maximum unresolved mass:',max_unresolved,'\n')
  if (max_unresolved<1e-8) break
}
if(max_unresolved>=1e-8) stop('Grid did not meet numerical tolerance')
p <- predict_absorption(chain,test$time_state[test$first_phase==1],test$meter_line[test$first_phase==1],
                        test$home_margin[test$first_phase==1],test$Team_In_Poss[test$first_phase==1])
cat('Fitting comparators using training games and an inner validation split\n')
comparison <- fit_comparators(train_rows,test_rows)
pred <- comparison$predictions
pred$Markov <- c(p$expected_result_home,p$away_win+.5*p$draw)
write.csv(pred,file.path(out,'predictions.csv'),row.names=FALSE)
write.csv(cbind(test[test$first_phase==1,c('ID','game_id')],p),file.path(out,'markov_probabilities.csv'),row.names=FALSE)
metrics <- performance_summary(pred)
write.csv(metrics,file.path(out,'performance.csv'),row.names=FALSE); print(metrics)
cal <- calibration_summary(pred)
write.csv(cal,file.path(out,'calibration.csv'),row.names=FALSE)
ggsave(file.path(out,'calibration.png'),
  ggplot(cal,aes(predicted,observed))+geom_abline(slope=1,intercept=0,linetype='dashed')+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=.015)+geom_point(aes(size=n))+
  coord_equal(xlim=c(0,1),ylim=c(0,1))+theme_minimal(base_size=13)+
  labs(x='Predicted win probability + half draw probability',y='Observed result (win = 1, draw = 0.5)',
       size='Team-state rows',subtitle='Held-out games; 95% match-bootstrap intervals'),width=7,height=6,dpi=180)
for(horizon in c('half','match')) {
  ep <- next_score_probabilities(model,horizon=horizon)
  write.csv(ep,file.path(out,paste0('next_score_',horizon,'.csv')),row.names=FALSE)
  # Display each time bucket separately; these are pooled phase states, not lineouts.
  display <- ep[ep$poss==1,];display$time_state <- factor(TIME_STATES[display$time],levels=TIME_STATES)
  ggsave(file.path(out,paste0('expected_points_',horizon,'.png')),
    ggplot(display,aes(field,ep_home))+geom_hline(yintercept=0,linetype='dashed')+
    geom_line()+geom_point()+facet_wrap(~time_state,ncol=3)+theme_minimal(base_size=12)+
    labs(x='Distance from opposition try line (m)',y='Expected next-score points',
         subtitle=paste('Tied score, home possession; horizon:',horizon)),width=10,height=6,dpi=180)
}
for(time in c('1h_middle','2h_middle')) {
  margins <- final_margin_distribution(chain,time,45,-3,'Home')
  write.csv(margins,file.path(out,paste0('margin_',time,'.csv')),row.names=FALSE)
  ggsave(file.path(out,paste0('margin_',time,'.png')),
    ggplot(margins,aes(margin,probability))+geom_col()+geom_vline(xintercept=0,linetype='dashed')+
    coord_cartesian(xlim=c(-60,60))+theme_minimal(base_size=13)+
    labs(x='Final home-minus-away margin',y='Probability',
         subtitle=paste(time,'; home down 3, home possession at 45 m')),width=7,height=4,dpi=180)
}
set.seed(123)
pbp <- simulate_game_play_by_play('1h_start',69,0,0,'Home',model)
write.csv(pbp,file.path(out,'example_trajectory.csv'),row.names=FALSE)
write.csv(predict_absorption(chain,'2h_middle',45,7,'Home'),
          file.path(out,'example_21_14.csv'),row.names=FALSE)
jsonlite::write_json(list(training_games=length(training),test_games=length(games)-length(training),
  training_phases=nrow(train),test_phases=nrow(test),test_first_phases=sum(test$first_phase==1),
  evaluation_team_rows=nrow(test_rows),margin_limit=bound,max_unresolved=max_unresolved,
  linear_system_residual=chain$residual,xgb_rounds=comparison$xgb_rounds,
  source_md5=unname(tools::md5sum('data/raw/phase_2018-19.csv')),
  heldout_bootstrap='2000 resamples of games; fitted models held fixed',
  kernel='joint signed score, possession switch, next field/time, terminal; empirical rows; unseen-row training backoff'),
  file.path(out,'diagnostics.json'),pretty=TRUE,auto_unbox=TRUE)
saveRDS(model,file.path(out,'transition_model_v2.rds'))
capture.output(sessionInfo(),file=file.path(out,'session-info.txt'))
cat('Saved corrected analysis to',out,'\n')
