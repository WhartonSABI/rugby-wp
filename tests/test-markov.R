# Run from project root: Rscript tests/test-markov.R
suppressPackageStartupMessages({
  source('code/helpers/data-prep.R'); source('code/helpers/state-space.R')
  source('code/helpers/markov-sim.R'); source('code/helpers/absorption.R')
})
stopifnot(identical(score_bucket_from_diff(c(-15,-14,-13,-7,-6,-1,0,1,6,7,13,14)),
  SCORE_BUCKETS[c(1,1,2,2,3,3,4,5,5,6,6,7)]))
d <- prepare_phase_data()
s <- prepare_transition_data(d)
stopifnot(nrow(s) == 35199, length(unique(s$game_id)) == 132,
          all(s$score_bucket == score_bucket_from_diff(s$Points_Difference)),
          all(s$home_score - s$away_score == s$home_margin),
          sum(s$terminal) == 132, sum(s$terminal & s$reward != 0) == 32)
# Replaying all transitions must reproduce the next observed scoreboard AND
# possession, including scoring for the opponent and retaining possession.
for (rows in split(seq_len(nrow(s)), s$game_id)) {
  z <- s[rows, ]; last <- nrow(z)
  home <- z$home_score[1]; away <- z$away_score[1]
  for (i in seq_len(last)) {
    r <- z$reward[i] * ifelse(z$Team_In_Poss[i] == 'Home', 1, -1)
    home <- home + max(r, 0); away <- away + max(-r, 0)
    if (i < last) {
      stopifnot(home == z$home_score[i+1], away == z$away_score[i+1],
        (z$Team_In_Poss[i] != z$Team_In_Poss[i+1]) == z$flip[i])
    }
  }
  expected <- if (home == away) 0.5 else as.numeric(if (z$Team_In_Poss[last] == 'Home') home > away else away > home)
  stopifnot(z$Win[last] == expected)
}
model <- build_state_space(d)
stopifnot(all(vapply(model$kernels, function(x) abs(sum(x$probability)-1)<1e-12, TRUE)))
# Analytic toy: self-loop with probability 1/2; terminal +5, -5, 0 with
# probabilities 1/5, 1/5, 1/10. Eventual probabilities are .4, .4, .2.
toy <- model
for (key in names(toy$kernels)) {
  parts <- strsplit(key, '_', fixed=TRUE)[[1]]
  time <- match(paste(parts[1:2],collapse='_'),TIME_STATES); field <- as.numeric(parts[3])
  toy$kernels[[key]] <- data.frame(reward=c(0,5,-5,0), next_time=c(time,0,0,0),
    next_field=c(field,0,0,0), flip=FALSE, terminal=c(FALSE,TRUE,TRUE,TRUE),
    half_end=c(FALSE,TRUE,TRUE,TRUE), probability=c(.5,.2,.2,.1))
}
chain <- compile_absorbing_chain(toy, 10)
p <- predict_absorption(chain,'1h_middle',45,0,'Home')
stopifnot(max(abs(unlist(p[1,c('home_win','draw','away_win')])-c(.4,.2,.4)))<1e-10)
ep <- next_score_probabilities(toy)
stopifnot(max(abs(ep$ep_home)) < 1e-10, max(abs(ep[['score_5']]-.4)) < 1e-10)
set.seed(876)
mc <- simulate_game('1h_middle',45,0,0,'Home',toy,n_sims=2000)
stopifnot(abs(mc$home_win_prob-.4)<.04, abs(mc$tie_prob-.2)<.04)
# A loop that cannot terminate must fail, never generate a spurious result.
loop <- toy
loop$kernels <- lapply(loop$kernels,function(x) {x<-x[1,];x$probability<-1;x})
stopifnot(inherits(try(simulate_game_play_by_play('1h_middle',45,0,0,'Home',loop,10),silent=TRUE),'try-error'))
# A deterministic score for the opponent must change the correct scoreboard.
neg <- toy; neg$kernels <- lapply(neg$kernels,function(x){x<-x[3,];x$probability<-1;x})
z <- simulate_game_play_by_play('1h_middle',45,0,0,'Away',neg)
stopifnot(z$home_score==5,z$away_score==0,z$terminal)
cat('PASS: all observed matches replay; score/possession invariants; terminal scores;\n')
cat('analytic absorption and +/-5 EP; Monte Carlo agreement; cutoff rejection.\n')
source('code/helpers/calibration.R')
# Paired uncertainty must be exactly zero for identical prediction columns.
small <- data.frame(ID=1:8,game_id=rep(letters[1:4],each=2),side=rep(c('Home','Away'),4),
  actual=c(1,0,0,1,.5,.5,1,0),Markov=c(.8,.2,.4,.6,.55,.45,.9,.1),Naive=.5)
small$Copy <- small$Markov
m <- performance_summary(small,n_boot=100)
stopifnot(m$difference_lower[m$model=='Copy']==0,m$difference_upper[m$model=='Copy']==0)
small$Markov[c(1,8)] <- c(1+1e-15,0)
cal <- calibration_summary(small,n_boot=100)
stopifnot(!anyNA(cal),sum(cal$n)==nrow(small))
cat('PASS: paired bootstrap and calibration endpoints.\n')
