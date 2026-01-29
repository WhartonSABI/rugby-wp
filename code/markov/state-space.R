#############
### SETUP ###
#############

# Markov Chain Simulation — Win probability via Monte Carlo simulation.
# Run from project root: Rscript code/markov/sim-runner.R
if (!file.exists("code/helpers/data-prep.R")) {
  stop("Run from project root: setwd(...) or Rscript code/markov/sim-runner.R")
}

source("code/helpers/data-prep.R")
source("code/helpers/state-space.R")
source("code/helpers/calibration.R")

#############
### PLOTS ###
#############

PLOT_DIR <- "plots/markov/sim"
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)
save_scoring_heatmap(score_agg, file.path(PLOT_DIR, "scoring_probability_by_state.png"),
  "Markov Sim — Probability of Scoring by State")

build_state <- function(time_state, meter_line, team_poss, score_home, score_away) {
  score_diff_poss <- if (team_poss == "Home") score_home - score_away else score_away - score_home
  score_bucket <- score_bucket_from_diff(score_diff_poss) # nolint
  paste(time_state, meter_line, score_bucket, sep = "_")
}

##################
### SIMULATION ###
##################

simulate_game <- function(start_state, score_home, score_away, team_poss,
                         transition_matrix, score_next_states, poss_change_matrix, n_sims = 1000) {
  results <- data.frame(home_wins = 0, away_wins = 0, ties = 0)
  for (sim in 1:n_sims) {
    current_state <- as.character(start_state)
    current_score_home <- score_home
    current_score_away <- score_away
    current_team <- team_poss
    max_plays <- 400
    plays <- 0
    while (plays < max_plays) {
      plays <- plays + 1
      if (!current_state %in% rownames(transition_matrix)) break
      probs <- transition_matrix[current_state, ]
      probs <- probs[probs > 0]
      if (length(probs) == 0) break
      next_event <- sample(names(probs), 1, prob = probs)
      if (next_event == "End_of_Game") break
      if (grepl("^Score_", next_event)) {
        pts <- as.numeric(sub("Score_", "", next_event))
        if (current_team == "Home") {
          current_score_home <- current_score_home + pts
          current_team <- "Away"
        } else {
          current_score_away <- current_score_away + pts
          current_team <- "Home"
        }
        next_state_df <- score_next_states[[current_state]]
        if (!is.null(next_state_df) && nrow(next_state_df) > 0) {
          next_state_df$probability <- next_state_df$probability / sum(next_state_df$probability)
          current_state <- as.character(sample(next_state_df$next_state, 1, prob = next_state_df$probability))
        }
      } else if (next_event == "Possession_Change") {
        current_team <- ifelse(current_team == "Home", "Away", "Home")
        if (current_state %in% rownames(poss_change_matrix)) {
          row <- poss_change_matrix[current_state, , drop = FALSE]
          probs_row <- as.numeric(row)
          names(probs_row) <- colnames(poss_change_matrix)
          probs_row <- probs_row[probs_row > 0]
          if (length(probs_row) > 0) current_state <- as.character(sample(names(probs_row), 1, prob = probs_row))
        }
      } else {
        current_state <- as.character(next_event)
      }
    }
    if (current_score_home > current_score_away) results$home_wins <- results$home_wins + 1
    else if (current_score_away > current_score_home) results$away_wins <- results$away_wins + 1
    else results$ties <- results$ties + 1
  }
  results$home_win_prob <- results$home_wins / n_sims
  results$away_win_prob <- results$away_wins / n_sims
  results$tie_prob <- results$ties / n_sims
  results
}

start_state <- build_state(time_state = 5, meter_line = 2.5, team_poss = "Away", score_home = 21, score_away = 14)
result <- simulate_game(start_state, 21, 14, "Away", transition_matrix,
  score_next_states, poss_change_matrix, n_sims = 1000)
cat("\n=== Example: 21-14, Away in possession at 5_2.5 ===\n")
print(result)

simulate_game_play_by_play <- function(start_state, score_home, score_away, team_poss,
                                      transition_matrix, score_next_states, poss_change_matrix, max_plays = 400) {
  current_state <- as.character(start_state)
  current_score_home <- score_home
  current_score_away <- score_away
  current_team <- team_poss
  plays <- 0
  log <- data.frame(play = integer(), state_before = character(), event = character(),
    team = character(), home_score = integer(), away_score = integer(), state_after = character(), stringsAsFactors = FALSE)
  while (plays < max_plays) {
    plays <- plays + 1
    if (!current_state %in% rownames(transition_matrix)) break
    probs <- transition_matrix[current_state, ]
    probs <- probs[probs > 0]
    if (length(probs) == 0) break
    next_event <- sample(names(probs), 1, prob = probs)
    state_before <- current_state
    if (next_event == "End_of_Game") {
      log <- rbind(log, data.frame(play = plays, state_before = state_before, event = "End_of_Game",
        team = current_team, home_score = current_score_home, away_score = current_score_away, state_after = NA))
      break
    }
    if (grepl("^Score_", next_event)) {
      pts <- as.numeric(sub("Score_", "", next_event))
      if (current_team == "Home") {
        current_score_home <- current_score_home + pts
        current_team <- "Away"
      } else {
        current_score_away <- current_score_away + pts
        current_team <- "Home"
      }
      next_state_df <- score_next_states[[current_state]]
      if (!is.null(next_state_df) && nrow(next_state_df) > 0) {
        next_state_df$probability <- next_state_df$probability / sum(next_state_df$probability)
        current_state <- as.character(sample(next_state_df$next_state, 1, prob = next_state_df$probability))
      }
    } else if (next_event == "Possession_Change") {
      current_team <- ifelse(current_team == "Home", "Away", "Home")
      if (current_state %in% rownames(poss_change_matrix)) {
        row <- poss_change_matrix[current_state, , drop = FALSE]
        probs_row <- as.numeric(row)
        names(probs_row) <- colnames(poss_change_matrix)
        probs_row <- probs_row[probs_row > 0]
        if (length(probs_row) > 0) current_state <- as.character(sample(names(probs_row), 1, prob = probs_row))
      }
    } else current_state <- as.character(next_event)
    log <- rbind(log, data.frame(play = plays, state_before = state_before, event = next_event,
      team = current_team, home_score = current_score_home, away_score = current_score_away, state_after = current_state))
  }
  log
}

###################
### CALIBRATION ###
###################

calibration_data <- prepare_calibration_data(phase_data, n_samples = 1000)

pbp_start_state <- build_state(time_state = 8, meter_line = 69, team_poss = "Home", score_home = 0, score_away = 0)
pbp <- simulate_game_play_by_play(
  start_state = pbp_start_state,
  score_home = 0,
  score_away = 0,
  team_poss = "Home",
  transition_matrix = transition_matrix,
  score_next_states = score_next_states,
  poss_change_matrix = poss_change_matrix
)

run_calibration_sims <- function(calibration_data, transition_matrix,
                                score_next_states, poss_change_matrix, n_sims = 100) {
  results <- data.frame(state = character(), actual_win = numeric(), predicted_win_prob = numeric(),
    home_score = numeric(), away_score = numeric(), time_state = character(), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(calibration_data))) {
    if (i %% 50 == 0) cat("Processing state", i, "of", nrow(calibration_data), "\n")
    row <- calibration_data[i, ]
    sim_result <- simulate_game(row$state, row$home_score, row$away_score, row$team_poss,
      transition_matrix, score_next_states, poss_change_matrix, n_sims = n_sims)
    pred_win_prob <- if (row$team_poss == "Home") sim_result$home_win_prob else sim_result$away_win_prob
    results <- rbind(results, data.frame(state = row$state, actual_win = row$poss_team_won,
      predicted_win_prob = pred_win_prob, home_score = row$home_score, away_score = row$away_score,
      time_state = sub("_.*", "", row$state), stringsAsFactors = FALSE))
  }
  results
}

calibration_results <- run_calibration_sims(calibration_data, transition_matrix,
  score_next_states, poss_change_matrix, n_sims = 100)

plot_calibration(calibration_results, PLOT_DIR,
  "Markov Chain Simulation — Calibration Curve",
  "Markov Chain Simulation — Distribution of Predictions",
  "Using Monte Carlo simulation. Perfect calibration = points on diagonal line",
  "Using Monte Carlo simulation")
print_error_reduction(calibration_results, "State Space Model (Markov Chain Simulation)")
