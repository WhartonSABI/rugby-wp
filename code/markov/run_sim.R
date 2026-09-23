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

phase_data <- phase_data[order(phase_data$ID), ]

unique_games <- unique(phase_data$game_id)
n_train <- floor(0.80 * length(unique_games))
train_games <- unique_games[1:n_train]

train_data   <- phase_data[phase_data$game_id %in% train_games, ]
holdout_data <- phase_data[!phase_data$game_id %in% train_games, ]

# Trying to match with other model test set
holdout_data <- holdout_data %>%
  filter(first_phase == 1)

# Gam

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
start_state <- build_state(time_state = start_time_state, meter_line = 45, team_poss = "Home",
  score_home = 0, score_away = 0)
result <- simulate_game(start_state, 0, 0, "Home", transition_matrix,
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


# Simulating next scoring event

get_outcome <- function(meter_line, time_state, team_poss = "Home",
                        transition_matrix, score_next_states, poss_change_matrix) {
  start_state <- build_state(time_state = time_state, meter_line = meter_line,
                             team_poss = team_poss, score_home = 0, score_away = 0)
  pbp <- simulate_game_play_by_play(
    start_state = start_state,
    score_home = 0, score_away = 0, team_poss = team_poss,
    transition_matrix = transition_matrix,
    score_next_states = score_next_states,
    poss_change_matrix = poss_change_matrix
  )
  
  idx <- which(pbp$event %in% c("Score_3", "Score_5", "Score_7", "End_of_Game"))[1]
  if (is.na(idx)) return(NA_character_)
  if (pbp$event[idx] == "End_of_Game") return("End_of_Game")
  
  # Determine who scored by comparing score columns to the prior row
  # (or to 0/0 if it's the very first play)
  prev_home <- if (idx == 1) 0 else pbp$home_score[idx - 1]
  prev_away <- if (idx == 1) 0 else pbp$away_score[idx - 1]
  
  scoring_team <- if (pbp$home_score[idx] > prev_home) "Home" else "Away"
  points <- case_when(
    pbp$event[idx] == "Score_7" ~ "7",
    pbp$event[idx] == "Score_5" ~ "5",
    pbp$event[idx] == "Score_3" ~ "3"
  )
  
  if (scoring_team == team_poss) {
    paste0("Starting_Team_Score_", points)
  } else {
    paste0("Opponent_Score_", points)
  }
}

n_sims <- 1000
meter_lines <- c(2.5, 13.5, 31, 45, 55, 69, 86.5, 97.5)

outcome_probs <- map_dfr(meter_lines, function(ml) {
  outcomes <- map_chr(seq_len(n_sims), ~ get_outcome(
    meter_line = ml, time_state = "1h_start", team_poss = "Home",
    transition_matrix = transition_matrix,
    score_next_states = score_next_states,
    poss_change_matrix = poss_change_matrix
  ))
  tibble(meter_line = ml, outcome = outcomes)
}) %>%
  filter(!is.na(outcome)) %>%
  count(meter_line, outcome) %>%
  group_by(meter_line) %>%
  mutate(probability = n / sum(n)) %>%
  ungroup()

outcome_colors <- c(
  "Starting_Team_Score_3" = "#9ecae1",  # light blue
  "Starting_Team_Score_5" = "#4292c6",  # medium blue
  "Starting_Team_Score_7" = "#08519c",  # dark blue
  "Opponent_Score_3"      = "#fcae91",  # light red
  "Opponent_Score_5"      = "#fb6a4a",  # medium red
  "Opponent_Score_7"      = "#a50f15",  # dark red
  "End_of_Game"           = "gray50"
)

next_score_plot <- ggplot(outcome_probs, aes(x = meter_line, y = probability, color = outcome)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = outcome_colors) +
  labs(title = "Probability of Game Outcome by Starting Field Position",
       x = "Meter Line", y = "Probability", color = "Outcome") +
  theme_minimal()

ggsave(
  filename = file.path(PLOT_DIR, "next_score_plot.png"),
  plot = next_score_plot,
  width = 8, height = 5, dpi = 300
)

# Smoothed

next_score_plot_smooth <-ggplot(outcome_probs, aes(x = meter_line, y = probability, color = outcome)) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1) +
  scale_color_manual(values = outcome_colors) +
  labs(title = "Probability of Game Outcome by Starting Field Position (Smoothed)",
       x = "Meter Line", y = "Probability", color = "Outcome") +
  theme_minimal()

ggsave(
  filename = file.path(PLOT_DIR, "next_score_plot_smooth.png"),
  plot = next_score_plot_smooth,
  width = 8, height = 5, dpi = 300
)

# Expected Points

expected_points <- outcome_probs %>%
  mutate(
    points = case_when(
      outcome == "Starting_Team_Score_7" ~ 7,
      outcome == "Starting_Team_Score_3" ~ 3,
      outcome == "Opponent_Score_7"      ~ -7,
      outcome == "Opponent_Score_3"      ~ -3,
      outcome == "End_of_Game"           ~ 0,
      TRUE ~ 0
    ),
    weighted_points = probability * points
  ) %>%
  group_by(meter_line) %>%
  summarise(expected_points = sum(weighted_points), .groups = "drop")

expected_points_plot <- ggplot(expected_points, aes(x = meter_line, y = expected_points)) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Expected Points by Starting Field Position (With Possession)",
       x = "Meter Line", y = "Expected Points") +
  theme_minimal()

ggsave(
  filename = file.path(PLOT_DIR, "expected_points_plot.png"),
  plot = expected_points_plot,
  width = 8, height = 5, dpi = 300
)


# Simulating final scores from a given state

# Middle of First Half

time_state_initial <- time_state_levels[2]
score_home <- 7
score_away <- 10
poss       <- "Home"

start_state <- build_state(time_state = time_state_initial,
                           meter_line = 45, team_poss = poss,
                           score_home = score_home, score_away = score_away)

# Getting Final Score of Game

sim_final <- function() {
  pbp <- simulate_game_play_by_play(
    start_state        = start_state,
    score_home         = score_home,
    score_away         = score_away,
    team_poss          = poss,
    transition_matrix  = transition_matrix,
    score_next_states  = score_next_states,
    poss_change_matrix = poss_change_matrix
  )
  last <- pbp[nrow(pbp), ]
  c(home = last$home_score, away = last$away_score, plays = nrow(pbp))
}

n_sims <- 1000
finals <- as.data.frame(t(replicate(n_sims, sim_final()))) |>
  mutate(margin = home - away,
         result = case_when(margin > 0 ~ "Home win",
                            margin < 0 ~ "Away win",
                            TRUE       ~ "Tie"))

final_margin_plot_first_half <- ggplot(finals, aes(margin, fill = margin > 0)) +
  geom_histogram(binwidth = 1, color = "white", show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("TRUE" = "#2c7fb8", "FALSE" = "#d95f0e")) +
  labs(title = "Final margin (Home - Away), Home down 3 in Middle of First Half",
       x = "Final margin", y = "Simulations")

ggsave(
  filename = file.path(PLOT_DIR, "final_margin_plot_first_half.png"),
  plot = final_margin_plot_first_half,
  width = 8, height = 5, dpi = 300
)

# Middle of Second Half

time_state_initial <- time_state_levels[5]
score_home <- 7
score_away <- 10
poss       <- "Home"

start_state <- build_state(time_state = time_state_initial,
                           meter_line = 45, team_poss = poss,
                           score_home = score_home, score_away = score_away)

# Getting Final Score of Game

sim_final <- function() {
  pbp <- simulate_game_play_by_play(
    start_state        = start_state,
    score_home         = score_home,
    score_away         = score_away,
    team_poss          = poss,
    transition_matrix  = transition_matrix,
    score_next_states  = score_next_states,
    poss_change_matrix = poss_change_matrix
  )
  last <- pbp[nrow(pbp), ]
  c(home = last$home_score, away = last$away_score, plays = nrow(pbp))
}

n_sims <- 1000
finals <- as.data.frame(t(replicate(n_sims, sim_final()))) |>
  mutate(margin = home - away,
         result = case_when(margin > 0 ~ "Home win",
                            margin < 0 ~ "Away win",
                            TRUE       ~ "Tie"))

final_margin_plot_second_half <- ggplot(finals, aes(margin, fill = margin > 0)) +
  geom_histogram(binwidth = 1, color = "white", show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("TRUE" = "#2c7fb8", "FALSE" = "#d95f0e")) +
  labs(title = "Final margin (Home - Away), Home down 3 in Middle of First Half",
       x = "Final margin", y = "Simulations")

ggsave(
  filename = file.path(PLOT_DIR, "final_margin_plot_second_half.png"),
  plot = final_margin_plot_second_half,
  width = 8, height = 5, dpi = 300
)

