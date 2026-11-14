# Refresh approved manuscript figures from the corrected direct calculations.
# Run after run_sim.R, from the repository root.
suppressPackageStartupMessages({
  source('code/helpers/state-space.R')
  source('code/helpers/absorption.R')
  source('code/helpers/calibration.R')
  source('code/helpers/comparators.R')
  library(ggplot2)
})
args <- commandArgs(trailingOnly=TRUE)
out <- if (length(args)) args[1] else 'output/markov-revision-20261113'
model <- readRDS(file.path(out, 'transition_model_v2.rds'))
diagnostics <- jsonlite::read_json(file.path(out, 'diagnostics.json'), simplifyVector=TRUE)
chain <- compile_absorbing_chain(model, diagnostics$margin_limit)
train <- model$phase_data[model$phase_data$first_phase == 1, ]
p <- predict_absorption(chain, train$time_state, train$meter_line,
                        train$home_margin, train$Team_In_Poss)
train_rows <- team_rows(train)[c('ID', 'game_id', 'side', 'actual')]
train_rows$Markov <- c(p$expected_result_home, p$away_win + .5*p$draw)
write.csv(train_rows, file.path(out, 'markov_training_predictions.csv'), row.names=FALSE)
cal_train <- calibration_summary(train_rows)
write.csv(cal_train, file.path(out, 'calibration_training.csv'), row.names=FALSE)
cal_test <- read.csv(file.path(out, 'calibration.csv'))

calibration_plot <- function(d, intervals=FALSE) {
  g <- ggplot(d, aes(predicted, observed)) +
    geom_abline(slope=1, intercept=0, linetype='dashed', color='grey50')
  if (intervals) g <- g + geom_errorbar(aes(ymin=lower, ymax=upper), width=.025)
  g + geom_point(size=2) +
    coord_equal(xlim=c(0,1), ylim=c(0,1)) +
    scale_x_continuous(breaks=seq(0,1,.25)) +
    scale_y_continuous(breaks=seq(0,1,.25)) +
    theme_minimal(base_size=14) +
    labs(x='Predicted expected result', y='Observed average result')
}
ggsave('paper/calibration_in_sample.png', calibration_plot(cal_train),
       width=4.4, height=4.2, dpi=300)
ggsave('paper/calibration_out_sample.png', calibration_plot(cal_test, TRUE),
       width=4.4, height=4.2, dpi=300)

for (half in c('first', 'second')) {
  bucket <- if (half == 'first') '1h_middle' else '2h_middle'
  d <- read.csv(file.path(out, paste0('margin_', bucket, '.csv')))
  g <- ggplot(d, aes(margin, probability)) + geom_col() +
    geom_vline(xintercept=0, linetype='dashed', color='grey50') +
    coord_cartesian(xlim=c(-60,60)) + theme_minimal(base_size=14) +
    labs(x='Final home-minus-away margin', y='Probability')
  ggsave(paste0('paper/final_margin_plot_', half, '_half.png'), g,
         width=4.4, height=3.6, dpi=300)
  cat(half, 'half: modal final margin', d$margin[which.max(d$probability)],
      '; mass outside displayed range', sum(d$probability[abs(d$margin)>60]), '\n')
}

# EP illustration: tied score, first-half middle bucket, possession
# team perspective, next score before half-time. Preserve all seven outcomes.
ep <- read.csv(file.path(out, 'next_score_half.csv'), check.names=FALSE)
ep <- ep[ep$time == 2 & ep$poss == 1, ]
values <- c(-7,-5,-3,0,3,5,7)
long <- do.call(rbind, lapply(values, function(k) {
  data.frame(field=ep$field, score=factor(k, levels=values),
             probability=ep[[paste0('score_', k)]])
}))
g <- ggplot(long, aes(field, probability, color=score, group=score)) +
  geom_line(linewidth=.7) + geom_point(size=1.6) +
  scale_color_manual(values=c('#99000d','#ef6548','#fc8d59','#636363',
                              '#74a9cf','#2b8cbe','#045a8d')) +
  theme_minimal(base_size=14) +
  theme(legend.position='bottom') +
  guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  labs(x='Distance from opposition try line (m)', y='Probability', color='Next score')
ggsave(file.path(out, 'paper_next_score_proposed.png'), g, width=4.8, height=4.2, dpi=300)
g <- ggplot(ep, aes(field, ep_home)) + geom_line() + geom_point(size=2) +
  geom_hline(yintercept=0, linetype='dashed', color='grey50') +
  theme_minimal(base_size=14) +
  labs(x='Distance from opposition try line (m)', y='Expected next-score points')
ggsave(file.path(out, 'paper_expected_points_proposed.png'), g, width=4.8, height=4.2, dpi=300)
file.copy(file.path(out, 'paper_next_score_proposed.png'), 'paper/next_score_plot.png', overwrite=TRUE)
file.copy(file.path(out, 'paper_expected_points_proposed.png'), 'paper/expected_points_plot.png', overwrite=TRUE)
cat('Updated calibration, margin, next-score, and EP figures.\n')
