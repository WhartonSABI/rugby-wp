#############
### SETUP ###
#############

# Shared state-space model: time x meter_line states, transition matrix,
# post-score and possession-change transitions. Run after data_prep.R.

library(dplyr)
library(ggplot2)
library(rlang)

###################
### STATE SPACE ###
###################

phase_data$time_state <- cut(phase_data$Seconds_Remaining,
                             breaks = seq(0, 4800, by = 600),
                             labels = 1:8,
                             include.lowest = TRUE)
score_bucket_from_diff <- function(score_diff_poss) {
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

phase_data$score_diff_poss <- ifelse(phase_data$Team_In_Poss == "Home",
                                     phase_data$Points_Difference,
                                     -phase_data$Points_Difference)
phase_data$score_bucket <- score_bucket_from_diff(phase_data$score_diff_poss)
phase_data$state <- paste(phase_data$time_state, phase_data$meter_line, phase_data$score_bucket, sep = "_")
phase_data$game_id <- paste(phase_data$Home, phase_data$Away, phase_data$Round, sep = "_")
phase_data <- phase_data[order(phase_data$game_id, -phase_data$Seconds_Remaining), ]

phase_data$next_team <- ave(phase_data$Team_In_Poss, phase_data$game_id,
                            FUN = function(x) c(as.character(x[-1]), NA))
phase_data$next_state_result <- ave(phase_data$state, phase_data$game_id,
                                    FUN = function(x) c(as.character(x[-1]), NA))
phase_data$next_points_change <- ave(phase_data$points_change, phase_data$game_id,
                                     FUN = function(x) c(x[-1], NA))

phase_data$state_result <- ifelse(
  !is.na(phase_data$next_points_change) & phase_data$next_points_change > 0,
  paste0("Score_", phase_data$next_points_change),
  ifelse(phase_data$Team_In_Poss != phase_data$next_team & !is.na(phase_data$next_team),
         "Possession_Change",
         ifelse(phase_data$state != phase_data$next_state_result & !is.na(phase_data$next_state_result),
                phase_data$next_state_result,
                ifelse(is.na(phase_data$next_team), "End_of_Game", phase_data$next_state_result)))
)

###################
### TRANSITIONS ###
###################

transition_matrix <- prop.table(table(phase_data$state, phase_data$state_result), margin = 1)
transition_df <- as.data.frame.table(transition_matrix)
names(transition_df) <- c("from_state", "to_result", "probability")
transition_df <- transition_df[transition_df$probability > 0, ]
transition_df <- transition_df[order(transition_df$from_state, -transition_df$probability), ]

view_state_transitions <- function(state, transition_df) {
  transition_df[transition_df$from_state == state, ]
}

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

###############
### HEATMAP ###
###############

scoring_by_state <- transition_df[grepl("^Score_", transition_df$to_result), ]
scoring_by_state$from_state <- as.character(scoring_by_state$from_state)
state_parts <- strsplit(scoring_by_state$from_state, "_")
scoring_by_state$time_state <- vapply(state_parts, `[[`, "", 1)
scoring_by_state$meter_line <- vapply(state_parts, `[[`, "", 2)
scoring_by_state$score_bucket <- vapply(state_parts, `[[`, "", 3)
score_agg <- aggregate(probability ~ time_state + meter_line, data = scoring_by_state, FUN = sum)
score_agg$meter_line <- factor(score_agg$meter_line,
                               levels = as.character(sort(unique(as.numeric(score_agg$meter_line)), decreasing = TRUE)))

save_scoring_heatmap <- function(score_agg, path, title) {
  p <- ggplot(score_agg, aes(x = .data$time_state, y = .data$meter_line, fill = .data$probability)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(.data$probability, 2)), size = 3) +
    scale_fill_gradient(low = "white", high = "darkred") +
    labs(title = title, subtitle = "Using next_points_change for accurate scoring attribution",
         x = "Time State", y = "Meter Line") +
    theme_minimal()
  ggsave(path, p, width = 7, height = 5, dpi = 300)
}

##################
### POST SCORE ###
##################

score_next_states <- phase_data %>%
  filter(grepl("^Score_", state_result), !is.na(next_state_result)) %>%
  count(state, next_state_result, name = "n") %>%
  group_by(state) %>%
  mutate(probability = n / sum(n)) %>%
  ungroup() %>%
  rename(next_state = next_state_result)

score_next_states <- split(score_next_states, score_next_states$state)

#########################
### POSSESSION CHANGE ###
#########################

build_possession_change_matrix <- function(phase_data) {
  poss_change_indices <- which(phase_data$state_result == "Possession_Change")
  next_indices <- poss_change_indices + 1
  valid_next <- next_indices <= nrow(phase_data)
  current_states <- phase_data$state[poss_change_indices[valid_next]]
  next_states <- phase_data$state[next_indices[valid_next]]
  prop.table(table(current_states, next_states), margin = 1)
}

poss_change_matrix <- build_possession_change_matrix(phase_data)
