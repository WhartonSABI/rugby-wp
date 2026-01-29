#############
### SETUP ###
#############

# Markov Chain Simulation — Win probability via Monte Carlo simulation.
# Run from project root: Rscript code/markov/run_sim.R
if (!file.exists("code/helpers/data-prep.R")) {
  stop("Run from project root: setwd(...) or Rscript code/markov/run_sim.R")
}

# Load shared helpers for data prep, state space, and calibration.
source("code/helpers/data-prep.R")
source("code/helpers/state-space.R")
source("code/helpers/calibration.R")
source("code/helpers/markov-sim.R")

# Build phase-level data and initialize state space.
transition_path <- "data/results/transition_probs.rds"
if (file.exists(transition_path)) {
  state_space <- readRDS(transition_path)
} else {
  phase_data <- prepare_phase_data(save_processed = TRUE)
  state_space <- build_state_space(phase_data, save_transition = TRUE)
}
phase_data <- state_space$phase_data
phase_data <- state_space$phase_data
transition_matrix <- state_space$transition_matrix
transition_df <- state_space$transition_df
scoring_probs <- state_space$scoring_probs
score_next_states <- state_space$score_next_states
poss_change_matrix <- state_space$poss_change_matrix
time_state_levels <- state_space$time_state_levels

# Hold out 20% of games for out-of-sample calibration.
set.seed(123)
unique_games <- unique(phase_data$game_id)
holdout_games <- sample(unique_games, size = floor(0.2 * length(unique_games)))
train_data <- phase_data[!phase_data$game_id %in% holdout_games, ]
holdout_data <- phase_data[phase_data$game_id %in% holdout_games, ]

state_space_train <- build_state_space(train_data)
state_space_holdout <- build_state_space(holdout_data)

#############
### PLOTS ###
#############

PLOT_DIR <- "plots/markov/sim"
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)
###################
### CALIBRATION ###
###################

calibration_in_sample <- prepare_calibration_data(phase_data, n_samples = 1000)
# In-sample simulation using full transition estimates.
results_in_sample <- run_calibration_sims_sim(calibration_in_sample, transition_matrix,
  score_next_states, poss_change_matrix, n_sims = 100)

invisible(plot_calibration(results_in_sample, PLOT_DIR,
  "Markov Chain Simulation — Calibration Curve (In Sample)",
  "Markov Chain Simulation — Distribution of Predictions (In Sample)",
  "Using Monte Carlo simulation. Perfect calibration = points on diagonal line",
  "Using Monte Carlo simulation",
  calibration_filename = "calibration_in_sample.png",
  distribution_filename = "prediction_distribution_in_sample.png"))
print_error_reduction(results_in_sample, "State Space Model (Markov Chain Simulation, In Sample)")

# Out-of-sample evaluation with transitions trained on training games only.
calibration_out_sample <- prepare_calibration_data(state_space_holdout$phase_data, n_samples = 1000)
results_out_sample <- run_calibration_sims_sim(calibration_out_sample,
  state_space_train$transition_matrix,
  state_space_train$score_next_states,
  state_space_train$poss_change_matrix,
  n_sims = 100)

invisible(plot_calibration(results_out_sample, PLOT_DIR,
  "Markov Chain Simulation — Calibration Curve (Out of Sample)",
  "Markov Chain Simulation — Distribution of Predictions (Out of Sample)",
  "Holdout matches, transitions fit on train set",
  "Holdout matches, transitions fit on train set",
  calibration_filename = "calibration_out_sample.png",
  distribution_filename = "prediction_distribution_out_sample.png"))
print_error_reduction(results_out_sample, "State Space Model (Markov Chain Simulation, Out of Sample)")

####################
### EXAMPLE GAME ###
####################

start_time_state <- time_state_levels[1]
# Example: away possession near the goal with a 21-14 scoreline.
start_state <- build_state(time_state = start_time_state, meter_line = 2.5, team_poss = "Away",
  score_home = 21, score_away = 14)
result <- simulate_game(start_state, 21, 14, "Away", transition_matrix,
  score_next_states, poss_change_matrix, n_sims = 1000)
cat("\n=== Example: 21-14, Away in possession at 2.5m ===\n")
print(result)

# Play-by-play example from kickoff state.
pbp_start_state <- build_state(time_state = start_time_state, meter_line = 69, team_poss = "Home",
  score_home = 0, score_away = 0)
pbp <- simulate_game_play_by_play(
  start_state = pbp_start_state,
  score_home = 0,
  score_away = 0,
  team_poss = "Home",
  transition_matrix = transition_matrix,
  score_next_states = score_next_states,
  poss_change_matrix = poss_change_matrix
)