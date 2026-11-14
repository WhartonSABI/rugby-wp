# Optional slower numerical check against Monte Carlo and a wider margin grid.
# Run after the analysis: Rscript tests/check-absorption.R
suppressPackageStartupMessages({
  source('code/helpers/data-prep.R'); source('code/helpers/state-space.R')
  source('code/helpers/absorption.R'); source('code/helpers/markov-sim.R')
})
out <- 'output/markov-revision-20261113'
m <- readRDS(file.path(out,'transition_model_v2.rds'))
chain <- compile_absorbing_chain(m,200)
truth <- predict_absorption(chain,'2h_middle',45,7,'Home')
set.seed(20261113)
mc <- simulate_game('2h_middle',45,21,14,'Home',m,n_sims=1000)
stopifnot(abs(mc$home_win_prob-truth$home_win)<.05,abs(mc$tie_prob-truth$draw)<.03)
d <- prepare_transition_data(prepare_phase_data())
g <- unique(d$game_id)
test <- d[!d$game_id %in% head(g,105) & d$first_phase==1,]
wide <- compile_absorbing_chain(m,250)
p1 <- predict_absorption(chain,test$time_state,test$meter_line,test$home_margin,test$Team_In_Poss)
p2 <- predict_absorption(wide,test$time_state,test$meter_line,test$home_margin,test$Team_In_Poss)
change <- max(abs(p1$expected_result_poss-p2$expected_result_poss))
stopifnot(change<1e-9)
print(truth); print(mc)
cat('PASS: Monte Carlo agrees; max probability change after widening grid:',change,'\n')
