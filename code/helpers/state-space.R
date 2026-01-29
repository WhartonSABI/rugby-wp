#############
### SETUP ###
#############

# Shared state-space model: time x meter_line states, transition matrix,
# post-score and possession-change transitions.

library(dplyr)
library(ggplot2)
library(rlang)

score_bucket_from_diff <- function(score_diff_poss) {
  # Map score differential (from possession team view) into coarse buckets.
  case_when(
    score_diff_poss <= -14 ~ "down_14p",
    score_diff_poss <= -7  ~ "down_7_13",
    score_diff_poss <= -1  ~ "down_1_6",
    score_diff_poss == 0   ~ "tied",
    score_diff_poss <= 6   ~ "up_1_6",
    score_diff_poss <= 13  ~ "up_7_13",
    TRUE                   ~ "up_14p"
  )
}

build_time_state <- function(seconds_remaining) {
  # Bucket time by half: start, middle, end of each half.
  half <- ifelse(seconds_remaining > 40 * 60, "1h", "2h")
  half_time <- ifelse(half == "1h", seconds_remaining - 40 * 60, seconds_remaining)
  time_bucket <- case_when(
    half_time > 40 * 60 - 2 * 60 ~ "start",
    half_time <= 2 * 60 ~ "end",
    TRUE ~ "middle"
  )
  time_state <- paste(half, time_bucket, sep = "_")
  factor(time_state, levels = c("1h_start", "1h_middle", "1h_end",
                                "2h_start", "2h_middle", "2h_end"))
}

build_state <- function(time_state, meter_line, team_poss, score_home, score_away) {
  # Build a composite state from time, field position, and score.
  score_diff_poss <- if (team_poss == "Home") score_home - score_away else score_away - score_home
  score_bucket <- score_bucket_from_diff(score_diff_poss)
  paste(time_state, meter_line, score_bucket, sep = "_")
}

view_state_transitions <- function(state, transition_df) {
  # Convenience helper to inspect transition probabilities from a state.
  transition_df[transition_df$from_state == state, ]
}

build_possession_change_matrix <- function(phase_data) {
  # Empirical probabilities for next state conditional on possession change.
  poss_change_indices <- which(phase_data$state_result == "Possession_Change")
  next_indices <- poss_change_indices + 1
  valid_next <- next_indices <= nrow(phase_data)
  current_states <- phase_data$state[poss_change_indices[valid_next]]
  next_states <- phase_data$state[next_indices[valid_next]]
  prop.table(table(current_states, next_states), margin = 1)
}

build_state_space <- function(phase_data,
                              save_transition = FALSE,
                              transition_path = "data/results/transition_probs.rds") {
  # Construct time/position/score states and transition probabilities.
  phase_data$time_state <- build_time_state(phase_data$Seconds_Remaining)
  phase_data$score_diff_poss <- ifelse(phase_data$Team_In_Poss == "Home",
                                       phase_data$Points_Difference,
                                       -phase_data$Points_Difference)
  phase_data$score_bucket <- score_bucket_from_diff(phase_data$score_diff_poss)
  phase_data$state <- paste(phase_data$time_state, phase_data$meter_line, phase_data$score_bucket, sep = "_")
  phase_data$game_id <- paste(phase_data$Home, phase_data$Away, phase_data$Round, sep = "_")
  phase_data <- phase_data[order(phase_data$game_id, -phase_data$Seconds_Remaining), ]

  # Build look-ahead columns to identify transitions and outcomes.
  phase_data$next_team <- ave(phase_data$Team_In_Poss, phase_data$game_id,
                              FUN = function(x) c(as.character(x[-1]), NA))
  phase_data$next_state_result <- ave(phase_data$state, phase_data$game_id,
                                      FUN = function(x) c(as.character(x[-1]), NA))
  phase_data$next_points_change <- ave(phase_data$points_change, phase_data$game_id,
                                       FUN = function(x) c(x[-1], NA))

  # Define state_result as score, possession change, or next state.
  phase_data$state_result <- ifelse(
    !is.na(phase_data$next_points_change) & phase_data$next_points_change > 0,
    paste0("Score_", phase_data$next_points_change),
    ifelse(phase_data$Team_In_Poss != phase_data$next_team & !is.na(phase_data$next_team),
           "Possession_Change",
           ifelse(phase_data$state != phase_data$next_state_result & !is.na(phase_data$next_state_result),
                  phase_data$next_state_result,
                  ifelse(is.na(phase_data$next_team), "End_of_Game", phase_data$next_state_result)))
  )

  # Transition matrix and tidy transition table.
  transition_matrix <- prop.table(table(phase_data$state, phase_data$state_result), margin = 1)
  transition_df <- as.data.frame.table(transition_matrix)
  names(transition_df) <- c("from_state", "to_result", "probability")
  transition_df <- transition_df[transition_df$probability > 0, ]
  transition_df <- transition_df[order(transition_df$from_state, -transition_df$probability), ]

  # Summaries for scoring and possession changes by state.
  state_summary <- aggregate(cbind(prob_score = probability) ~ from_state,
                             data = transition_df[grepl("^Score_", transition_df$to_result), ],
                             FUN = sum)
  poss_change_summary <- aggregate(probability ~ from_state,
                                   data = transition_df[transition_df$to_result == "Possession_Change", ],
                                   FUN = sum)
  names(poss_change_summary)[2] <- "prob_poss_change"
  state_summary <- merge(state_summary, poss_change_summary, by = "from_state", all = TRUE)
  state_summary[is.na(state_summary)] <- 0
  scoring_trans <- transition_df[grepl("^Score_", transition_df$to_result), ]
  scoring_trans$points_scored <- as.numeric(sub("Score_", "", scoring_trans$to_result))
  scoring_trans$exp_pts <- scoring_trans$probability * scoring_trans$points_scored
  exp_pts_by_state <- aggregate(exp_pts ~ from_state, data = scoring_trans, FUN = sum)
  names(exp_pts_by_state)[2] <- "exp_points_from_state"
  state_summary <- merge(state_summary, exp_pts_by_state, by = "from_state", all.x = TRUE)
  state_summary[is.na(state_summary)] <- 0

  # Aggregated scoring probability by time and field position.
  scoring_by_state <- transition_df[grepl("^Score_", transition_df$to_result), ]
  scoring_by_state$from_state <- as.character(scoring_by_state$from_state)
  state_lookup <- unique(phase_data[, c("state", "time_state", "meter_line", "score_bucket")])
  state_score_prob <- aggregate(probability ~ from_state, data = scoring_by_state, FUN = sum)
  names(state_score_prob)[2] <- "score_prob"
  state_score_prob <- merge(state_score_prob, state_lookup,
                            by.x = "from_state", by.y = "state", all.x = TRUE)
  state_score_prob$time_state <- as.character(state_score_prob$time_state)
  state_counts <- as.data.frame(table(phase_data$state), stringsAsFactors = FALSE)
  names(state_counts) <- c("state", "n")
  state_score_prob <- merge(state_score_prob, state_counts,
                            by.x = "from_state", by.y = "state", all.x = TRUE)
  state_score_prob$n[is.na(state_score_prob$n)] <- 0
  state_score_prob$weighted_score_prob <- state_score_prob$score_prob * state_score_prob$n
  scoring_probs <- aggregate(cbind(weighted_score_prob, n) ~ time_state + meter_line,
                             data = state_score_prob, FUN = sum)
  scoring_probs$probability <- ifelse(scoring_probs$n > 0,
                                      scoring_probs$weighted_score_prob / scoring_probs$n,
                                      0)
  scoring_probs <- scoring_probs[, c("time_state", "meter_line", "probability")]
  scoring_probs$time_state <- factor(scoring_probs$time_state,
                                     levels = c("first_2", "middle", "last_2"))
  scoring_probs$meter_line <- factor(scoring_probs$meter_line,
                                     levels = as.character(sort(unique(as.numeric(scoring_probs$meter_line)), decreasing = TRUE)))

  # Score-specific next-state probabilities used by simulation/DP.
  score_next_states <- phase_data %>%
    filter(grepl("^Score_", state_result), !is.na(next_state_result)) %>%
    count(state, next_state_result, name = "n") %>%
    group_by(state) %>%
    mutate(probability = n / sum(n)) %>%
    ungroup() %>%
    rename(next_state = next_state_result)

  # Possession change probabilities by current state.
  score_next_states <- split(score_next_states, score_next_states$state)
  poss_change_matrix <- build_possession_change_matrix(phase_data)

  transition_outputs <- list(
    phase_data = phase_data,
    transition_matrix = transition_matrix,
    transition_df = transition_df,
    state_summary = state_summary,
    scoring_probs = scoring_probs,
    score_next_states = score_next_states,
    poss_change_matrix = poss_change_matrix,
    time_state_levels = levels(phase_data$time_state)
  )

  # Optionally persist transition probabilities for reuse.
  if (save_transition) {
    dir.create(dirname(transition_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(transition_outputs, transition_path)
  }

  transition_outputs
}
