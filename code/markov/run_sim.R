#############
### SETUP ###
#############

# Markov Chain Simulation — Win probability via Monte Carlo simulation.
# Run from project root: Rscript code/markov/run_sim.R
if (!file.exists("code/helpers/data-prep.R")) {
  stop("Run from project root: setwd(...) or Rscript code/markov/run_sim.R")
}

source("code/helpers/data-prep.R")
source("code/helpers/state-space.R")
source("code/helpers/calibration.R")
source("code/helpers/markov-sim.R")

phase_data <- prepare_phase_data()
state_space <- build_state_space(phase_data)
phase_data <- state_space$phase_data
transition_matrix <- state_space$transition_matrix
transition_df <- state_space$transition_df
score_agg <- state_space$score_agg
score_next_states <- state_space$score_next_states
poss_change_matrix <- state_space$poss_change_matrix
time_state_levels <- state_space$time_state_levels

#############
### PLOTS ###
#############

PLOT_DIR <- "plots/markov/sim"
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)
save_scoring_heatmap(score_agg, file.path(PLOT_DIR, "scoring_probability_by_state.png"),
  "Markov Sim — Probability of Scoring by State")

##################
### SIMULATION ###
##################

start_time_state <- time_state_levels[length(time_state_levels)]
start_state <- build_state(time_state = start_time_state, meter_line = 2.5, team_poss = "Away",
  score_home = 21, score_away = 14)
result <- simulate_game(start_state, 21, 14, "Away", transition_matrix,
  score_next_states, poss_change_matrix, n_sims = 1000)
cat("\n=== Example: 21-14, Away in possession at 2.5m ===\n")
print(result)

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

###################
### CALIBRATION ###
###################

calibration_data <- prepare_calibration_data(phase_data, n_samples = 1000)

calibration_results <- run_calibration_sims_sim(calibration_data, transition_matrix,
  score_next_states, poss_change_matrix, n_sims = 100)

plot_calibration(calibration_results, PLOT_DIR,
  "Markov Chain Simulation — Calibration Curve",
  "Markov Chain Simulation — Distribution of Predictions",
  "Using Monte Carlo simulation. Perfect calibration = points on diagonal line",
  "Using Monte Carlo simulation")
print_error_reduction(calibration_results, "State Space Model (Markov Chain Simulation)")
