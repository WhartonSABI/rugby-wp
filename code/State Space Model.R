
# setwd("/Users/kennywatts/Documents/Github/Rugby_wp")

# load data
phase_data = read.csv("data/phase_2018-19.csv")

################################
### Organising training data ###
################################

head(phase_data)

phase_data <- phase_data %>%
  mutate(
    # extract signed numbers inside parentheses: (+3), (-3), etc.
    points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
    
    # handle cases with no number (e.g. "no score", turnovers)
    points = ifelse(is.na(points), 0, points)
  )

# Finding total points

phase_data <- phase_data %>%
  group_by(Round, Home, Away) %>%
  mutate(
    prev_diff = lag(Points_Difference, default = first(Points_Difference)),  # First row: no change
    scoring_event = abs(Points_Difference) != abs(prev_diff) & row_number() > 1,  # Skip first
    points_change = if_else(scoring_event, abs(lag(points)), 0),
    total_points_both = cumsum(points_change)
  ) %>%
  ungroup()

# Getting final score of the match
last_play <- phase_data %>%
  group_by(Round, Home, Away) %>%
  filter(ID == max(ID)) %>%
  ungroup() %>%
  mutate(
    last_play_points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
    last_play_points = ifelse(is.na(last_play_points), 0, last_play_points),
    
    Score_Change = last_play_points,
    
    Final_Points_Difference = Points_Difference + Score_Change,
    Final_Points_Diff_Home = if_else(Team_In_Poss == "Home",
                                     Final_Points_Difference,
                                     -Final_Points_Difference)
  )

# making win percentage column

last_play <- last_play %>%
  dplyr::select(Round, Home, Away, Final_Points_Diff_Home) %>%
  distinct()

last_play <- last_play %>%
  pivot_longer(
    cols = c(Home, Away),
    names_to = "Side",
    values_to = "Team"
  ) %>%
  mutate(
    Win = case_when(
      Side == "Home" & Final_Points_Diff_Home > 0  ~ 1,
      Side == "Home" & Final_Points_Diff_Home == 0 ~ 0.5,   # draw
      Side == "Home" & Final_Points_Diff_Home < 0  ~ 0,
      Side == "Away" & Final_Points_Diff_Home < 0  ~ 1,
      Side == "Away" & Final_Points_Diff_Home == 0 ~ 0.5,   # draw
      Side == "Away" & Final_Points_Diff_Home > 0  ~ 0
    ),
    Round = as.numeric(Round)
  ) %>%
  arrange(Team, Round)

# Get running win percentages
last_play <- last_play %>%
  group_by(Team) %>%
  arrange(Round) %>%
  mutate(
    Games_Played = lag(row_number(), default = 0),
    Wins_Before = lag(cumsum(Win), default = 0),
    WinPct_Before = if_else(Games_Played == 0, 0.5, Wins_Before / Games_Played)
  ) %>%
  ungroup()

# Get teams in possession
phase_data <- phase_data %>%
  mutate(
    Team_for_join = case_when(
      Team_In_Poss == "Home" ~ Home,
      Team_In_Poss == "Away" ~ Away
    )
  ) %>%
  # Add in win percentage for team in possession and whether they win
  left_join(
    last_play %>% dplyr::select(Team, Round, WinPct_Before, Win),
    by = c("Team_for_join" = "Team", "Round" = "Round")
  )

# Define which team is in possession
phase_data <- phase_data %>%
  mutate(
    Home_Attack = if_else(Team_In_Poss == "Home", 1, 0)
  )

# Get opponent data
phase_data <- phase_data %>%
  mutate(
    Opponent = if_else(Team_In_Poss == "Home", Away, Home)
  ) %>%
  left_join(
    last_play %>% dplyr::select(Team, Round, WinPct_Before) %>%
      rename(Opponent = Team,
             Opponent_WinPct = WinPct_Before),
    by = c("Opponent", "Round")
  ) %>%
  mutate(
    # Win percent differential
    WinPct_Diff = WinPct_Before - Opponent_WinPct,
    # Card differential
    Card_Diff = (Yellow_Cards_Opp + Red_Cards_Opp) - (Yellow_Cards_Own + Red_Cards_Own)
  )


# First phase or open play

phase_data <- phase_data %>%
  mutate(
    first_phase = if_else(
      Phase == 1,
      1,
      0
    ))

# Filter for first phase

phase_data <- phase_data %>%
  filter(first_phase == 1)

# Adding meter line of play start
location_names <- c("5m-Goal (opp)", "22m-5m (opp)", "10m-22m (opp)",
                    "Half-10m (opp)","10m-Half (own)", "22m-10m (own)",
                    "5m-22m (own)", "Goal-5m (own)")
location_meters <- c(2.5, 13.5, 31, 45, 55, 69, 86.5, 97.5)

lookup <- setNames(location_meters, location_names)

phase_data$meter_line <- lookup[phase_data$Location]

# Adding scores ahead variable

phase_data <- phase_data %>%
  mutate(
    Scores_Ahead = case_when(
      Points_Difference == 0 ~ 0L,
      TRUE ~ sign(Points_Difference) *
        ceiling(abs(Points_Difference) / 7)
    )
  )

###############################################
### Transforming Data to State Spaced Model ###
###############################################

# Create time interval states (8 x 10-minute intervals)
phase_data$time_state <- cut(phase_data$Seconds_Remaining, 
                             breaks = seq(0, 4800, by = 600),
                             labels = 1:8,
                             include.lowest = TRUE)

# Create state variable combining time and meter_line
phase_data$state <- paste(phase_data$time_state, phase_data$meter_line, sep = "_")

# Calculate expected points for each state
ep_model <- aggregate(points_change ~ state, data = phase_data, FUN = mean)
names(ep_model)[2] <- "expected_points"

# Merge expected points back to original data
phase_data <- merge(phase_data, ep_model, by = "state", all.x = TRUE)

# View the expected points by state
print(ep_model)

# Plotting

library(ggplot2)

# Split state back into time_state and meter_line components
ep_model$time_state <- sub("_.*", "", ep_model$state)
ep_model$meter_line <- sub(".*_", "", ep_model$state)

# Order meter_line by numeric value (largest to smallest)
ep_model$meter_line <- factor(ep_model$meter_line, 
                              levels = sort(unique(as.numeric(ep_model$meter_line)), 
                                            decreasing = TRUE))

# Create heatmap
ggplot(ep_model, aes(x = time_state, y = meter_line, fill = expected_points)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(expected_points, 2)), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Expected Points by Field Position and Time Remaining",
       x = "Time State (10-min intervals)",
       y = "Meter Line",
       fill = "Expected\nPoints") +
  theme_minimal()

# Transforming Data to State Appropriate Structure

# 1. Filter for unique games
unique_games <- unique(phase_data[, c("Home", "Away", "Round")])
phase_data$game_id <- paste(phase_data$Home, phase_data$Away, phase_data$Round, sep = "_")

# 2. Order by game and time (descending seconds = forward in time)
phase_data <- phase_data[order(phase_data$game_id, -phase_data$Seconds_Remaining), ]

# 3. Create next row variables within each game
phase_data$next_team <- ave(phase_data$Team_In_Poss, phase_data$game_id, 
                            FUN = function(x) c(as.character(x[-1]), NA))
phase_data$next_state_result <- ave(phase_data$state, phase_data$game_id, 
                                    FUN = function(x) c(as.character(x[-1]), NA))

phase_data$next_points_change <- ave(phase_data$points_change, phase_data$game_id,
                                     FUN = function(x) c(x[-1], NA))

# Determine the result of current state using NEXT row's points_change
phase_data$state_result <- ifelse(!is.na(phase_data$next_points_change) & phase_data$next_points_change > 0, 
                                  paste0("Score_", phase_data$next_points_change),
                                  ifelse(phase_data$Team_In_Poss != phase_data$next_team & !is.na(phase_data$next_team),
                                         "Possession_Change",
                                         ifelse(phase_data$state != phase_data$next_state_result & !is.na(phase_data$next_state_result),
                                                phase_data$next_state_result,
                                                ifelse(is.na(phase_data$next_team),
                                                       "End_of_Game",
                                                       phase_data$next_state_result))))

# View the results
table(phase_data$state_result)
head(phase_data[, c("state", "points_change", "next_points_change", "Team_In_Poss", "next_team", "state_result")], 30)


#####################################
### Transition Probability Matrix ###
#####################################

transition_matrix <- prop.table(table(phase_data$state, phase_data$state_result), margin = 1)

# Convert to data frame
transition_df <- as.data.frame.table(transition_matrix)
names(transition_df) <- c("from_state", "to_result", "probability")
transition_df <- transition_df[transition_df$probability > 0, ]
transition_df <- transition_df[order(transition_df$from_state, -transition_df$probability), ]

# View transitions from a specific state
view_state_transitions <- function(state, transition_df) {
  subset(transition_df, from_state == state)
}

cat("\n=== Example transitions from state 5_45 ===\n")
print(view_state_transitions("5_45", transition_df))

# Summary statistics by state
state_summary <- aggregate(cbind(prob_score = probability) ~ from_state, 
                           data = transition_df[grepl("^Score_", transition_df$to_result), ],
                           FUN = sum)

poss_change_summary <- aggregate(probability ~ from_state,
                                 data = transition_df[transition_df$to_result == "Possession_Change", ],
                                 FUN = sum)
names(poss_change_summary)[2] <- "prob_poss_change"

state_summary <- merge(state_summary, poss_change_summary, by = "from_state", all = TRUE)
state_summary[is.na(state_summary)] <- 0

# Expected points when scoring
scoring_trans <- transition_df[grepl("^Score_", transition_df$to_result), ]
scoring_trans$points_scored <- as.numeric(sub("Score_", "", scoring_trans$to_result))
scoring_trans$exp_pts <- scoring_trans$probability * scoring_trans$points_scored

exp_pts_by_state <- aggregate(exp_pts ~ from_state, data = scoring_trans, FUN = sum)
names(exp_pts_by_state)[2] <- "exp_points_from_state"

state_summary <- merge(state_summary, exp_pts_by_state, by = "from_state", all.x = TRUE)
state_summary[is.na(state_summary)] <- 0

cat("\n=== State Summary (Corrected) ===\n")
print(head(state_summary, 20))

# Heatmap of scoring probability
library(ggplot2)
scoring_by_state <- transition_df[grepl("^Score_", transition_df$to_result), ]
scoring_by_state$time_state <- sub("_.*", "", scoring_by_state$from_state)
scoring_by_state$meter_line <- sub(".*_", "", scoring_by_state$from_state)

score_agg <- aggregate(probability ~ time_state + meter_line, 
                       data = scoring_by_state, FUN = sum)

score_agg$meter_line <- factor(score_agg$meter_line, 
                               levels = as.character(sort(unique(as.numeric(score_agg$meter_line)), 
                                                          decreasing = TRUE)))

ggplot(score_agg, aes(x = time_state, y = meter_line, fill = probability)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(probability, 2)), size = 3) +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(title = "Probability of Scoring by State",
       subtitle = "Using next_points_change for accurate scoring attribution",
       x = "Time State",
       y = "Meter Line") +
  theme_minimal()

# Testing what happens after a score

analyze_next_state_after_score <- function(phase_data) {
  
  scored_indices <- which(grepl("^Score_", phase_data$state_result))
  
  next_indices <- scored_indices + 1
  valid_next <- next_indices <= nrow(phase_data)
  
  scored_indices <- scored_indices[valid_next]
  next_indices <- next_indices[valid_next]
  
  scoring_team <- phase_data$Team_In_Poss[scored_indices]
  next_states <- phase_data$state[next_indices]
  next_team_poss <- phase_data$Team_In_Poss[next_indices]
  
  cat("\n=== NEXT STATE AFTER SCORING (ALL) ===\n")
  cat("Total scoring events:", length(next_states), "\n\n")
  
  if (length(next_states) == 0) {
    cat("No scoring events found\n")
    return(NULL)
  }
  
  scoring_team_receives <- scoring_team == next_team_poss
  
  cat("\n--- FROM SCORING TEAM PERSPECTIVE (team that receives kickoff) ---\n")
  scoring_team_states <- next_states[scoring_team_receives]
  cat("Events where scoring team gets possession:", sum(scoring_team_receives), "\n")
  
  if (sum(scoring_team_receives) > 0) {
    st_probs <- prop.table(table(scoring_team_states))
    st_df <- as.data.frame(st_probs)
    names(st_df) <- c("next_state", "probability")
    st_df <- st_df[order(-st_df$probability), ]
    print(head(st_df, 20))
  }
  
  cat("\n--- FROM CONCEDING TEAM PERSPECTIVE (team that kicks off) ---\n")
  conceding_team_states <- next_states[!scoring_team_receives]
  cat("Events where conceding team gets possession:", sum(!scoring_team_receives), "\n")
  
  if (sum(!scoring_team_receives) > 0) {
    ct_probs <- prop.table(table(conceding_team_states))
    ct_df <- as.data.frame(ct_probs)
    names(ct_df) <- c("next_state", "probability")
    ct_df <- ct_df[order(-ct_df$probability), ]
    print(head(ct_df, 20))
  }
  
  return(list(
    scoring_team_next = if(sum(scoring_team_receives) > 0) st_df else NULL,
    conceding_team_next = if(sum(!scoring_team_receives) > 0) ct_df else NULL
  ))
}

score_next_states <- analyze_next_state_after_score(phase_data)

# Testing what happens after a possession change

analyze_next_state_after_possession_change <- function(phase_data) {
  
  poss_change_indices <- which(phase_data$state_result == "Possession_Change")
  
  next_indices <- poss_change_indices + 1
  valid_next <- next_indices <= nrow(phase_data)
  
  poss_change_indices <- poss_change_indices[valid_next]
  next_indices <- next_indices[valid_next]
  
  current_states <- phase_data$state[poss_change_indices]
  losing_team <- phase_data$Team_In_Poss[poss_change_indices]
  next_states <- phase_data$state[next_indices]
  next_team_poss <- phase_data$Team_In_Poss[next_indices]
  
  cat("\n=== POSSESSION CHANGE: CURRENT vs NEXT STATE ===\n")
  cat("Total possession changes:", length(next_states), "\n\n")
  
  if (length(next_states) == 0) {
    cat("No possession changes found\n")
    return(NULL)
  }
  
  losing_team_keeps <- losing_team == next_team_poss
  
  cat("--- TEAM THAT LOST POSSESSION ---\n")
  cat("State when they lost it -> State from their perspective after:\n")
  losing_comparison <- data.frame(
    current_state = current_states[!losing_team_keeps],
    next_state = next_states[!losing_team_keeps]
  )
  if (nrow(losing_comparison) > 0) {
    losing_table <- table(losing_comparison$current_state, losing_comparison$next_state)
    print(head(as.data.frame.table(losing_table)[as.data.frame.table(losing_table)$Freq > 0, ], 20))
  }
  
  cat("\n--- TEAM THAT GAINED POSSESSION ---\n")
  cat("State when they gained it -> State from their perspective:\n")
  gaining_comparison <- data.frame(
    current_state = current_states[!losing_team_keeps],
    next_state = next_states[!losing_team_keeps]
  )
  if (nrow(gaining_comparison) > 0) {
    gaining_table <- table(gaining_comparison$current_state, gaining_comparison$next_state)
    print(head(as.data.frame.table(gaining_table)[as.data.frame.table(gaining_table)$Freq > 0, ], 20))
  }
  
  return(list(
    losing_team_transitions = losing_comparison,
    gaining_team_transitions = gaining_comparison
  ))
}

poss_next_states <- analyze_next_state_after_possession_change(phase_data)

# Build possession change transition matrix

build_possession_change_matrix <- function(phase_data) {
  
  poss_change_indices <- which(phase_data$state_result == "Possession_Change")
  next_indices <- poss_change_indices + 1
  valid_next <- next_indices <= nrow(phase_data)
  
  poss_change_indices <- poss_change_indices[valid_next]
  next_indices <- next_indices[valid_next]
  
  current_states <- phase_data$state[poss_change_indices]
  next_states <- phase_data$state[next_indices]
  
  poss_change_matrix <- prop.table(table(current_states, next_states), margin = 1)
  
  return(poss_change_matrix)
}

poss_change_matrix <- build_possession_change_matrix(phase_data)

###########################
### Simulation function ###
###########################

simulate_game <- function(start_state, score_home, score_away, team_poss, 
                          transition_matrix, 
                          post_score_states_scoring_team,
                          post_score_states_conceding_team,
                          poss_change_matrix,
                          n_sims = 1000) {
  
  results <- data.frame(home_wins = 0, away_wins = 0, ties = 0)
  
  for (sim in 1:n_sims) {
    current_state <- start_state
    current_score_home <- score_home
    current_score_away <- score_away
    current_team <- team_poss
    max_plays <- 200
    plays <- 0
    
    while (plays < max_plays) {
      plays <- plays + 1
      
      if (!current_state %in% rownames(transition_matrix)) {
        break
      }
      
      probs <- transition_matrix[current_state, ]
      probs <- probs[probs > 0]
      
      if (length(probs) == 0) break
      
      next_event <- sample(names(probs), 1, prob = probs)
      
      if (next_event == "End_of_Game") {
        break
        
      } else if (grepl("^Score_", next_event)) {
        pts <- as.numeric(sub("Score_", "", next_event))
        if (current_team == "Home") {
          current_score_home <- current_score_home + pts
          current_team <- "Away"
          current_state <- sample(post_score_states_scoring_team$next_state, 1, 
                                  prob = post_score_states_scoring_team$probability)
        } else {
          current_score_away <- current_score_away + pts
          current_team <- "Home"
          current_state <- sample(post_score_states_scoring_team$next_state, 1, 
                                  prob = post_score_states_scoring_team$probability)
        }
        
      } else if (next_event == "Possession_Change") {
        current_team <- ifelse(current_team == "Home", "Away", "Home")
        
        # Sample next state based on where possession was lost
        if (current_state %in% rownames(poss_change_matrix)) {
          next_state_probs <- poss_change_matrix[current_state, ]
          next_state_probs <- next_state_probs[next_state_probs > 0]
          
          if (length(next_state_probs) > 0) {
            current_state <- sample(names(next_state_probs), 1, prob = next_state_probs)
          }
        }
        
      } else {
        current_state <- next_event
      }
    }
    
    if (current_score_home > current_score_away) {
      results$home_wins <- results$home_wins + 1
    } else if (current_score_away > current_score_home) {
      results$away_wins <- results$away_wins + 1
    } else {
      results$ties <- results$ties + 1
    }
  }
  
  results$home_win_prob <- results$home_wins / n_sims
  results$away_win_prob <- results$away_wins / n_sims
  results$tie_prob <- results$ties / n_sims
  
  return(results)
}

# Run with new possession change matrix

result <- simulate_game(
  start_state = "5_2.5", 
  score_home = 21, 
  score_away = 14, 
  team_poss = "Away",
  transition_matrix = transition_matrix,
  post_score_states_scoring_team = score_next_states$scoring_team_next,
  post_score_states_conceding_team = score_next_states$conceding_team_next,
  poss_change_matrix = poss_change_matrix,
  n_sims = 1000
)

result

# Calibrating Model

prepare_calibration_data <- function(phase_data, n_samples = 2500) {
  
  phase_data <- phase_data[phase_data$state_result != "End_of_Game", ]
  
  set.seed(123)
  sample_indices <- sample(1:nrow(phase_data), min(n_samples, nrow(phase_data)))
  sampled_states <- phase_data[sample_indices, ]
  
  sampled_states$home_score <- ifelse(sampled_states$Team_In_Poss == "Home",
                                      (sampled_states$total_points_both + sampled_states$Points_Difference) / 2,
                                      (sampled_states$total_points_both - sampled_states$Points_Difference) / 2)
  
  sampled_states$away_score <- sampled_states$total_points_both - sampled_states$home_score
  
  sampled_states$team_poss <- ifelse(sampled_states$Team_In_Poss == "Home",
                                     "Home", "Away")
  
  sampled_states$poss_team_won <- sampled_states$Win
  
  return(sampled_states)
}

calibration_data <- prepare_calibration_data(phase_data, n_samples = 2500)

cat("=== Sample of Calibration Data ===\n")
print(head(calibration_data[, c("state", "home_score", "away_score", "team_poss", "poss_team_won")], 10))

run_calibration_sims <- function(calibration_data, transition_matrix, 
                                 post_score_states_scoring_team,
                                 post_score_states_conceding_team,
                                 poss_change_matrix,
                                 n_sims = 100) {
  
  results <- data.frame(
    state = character(),
    actual_win = numeric(),
    predicted_win_prob = numeric(),
    home_score = numeric(),
    away_score = numeric(),
    time_state = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(calibration_data)) {
    
    if (i %% 50 == 0) cat("Processing state", i, "of", nrow(calibration_data), "\n")
    
    row <- calibration_data[i, ]
    
    sim_result <- simulate_game(
      start_state = row$state,
      score_home = row$home_score,
      score_away = row$away_score,
      team_poss = row$team_poss,
      transition_matrix = transition_matrix,
      post_score_states_scoring_team = post_score_states_scoring_team,
      post_score_states_conceding_team = post_score_states_conceding_team,
      poss_change_matrix = poss_change_matrix,
      n_sims = n_sims
    )
    
    if (row$team_poss == "Home") {
      pred_win_prob <- sim_result$home_win_prob
    } else {
      pred_win_prob <- sim_result$away_win_prob
    }
    
    results <- rbind(results, data.frame(
      state = row$state,
      actual_win = row$poss_team_won,
      predicted_win_prob = pred_win_prob,
      home_score = row$home_score,
      away_score = row$away_score,
      time_state = sub("_.*", "", row$state),
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

calibration_results <- run_calibration_sims(
  calibration_data, 
  transition_matrix, 
  post_score_states_scoring_team = score_next_states$scoring_team_next,
  post_score_states_conceding_team = score_next_states$conceding_team_next,
  poss_change_matrix = poss_change_matrix,
  n_sims = 100
)

library(ggplot2)

plot_calibration <- function(calibration_results) {
  
  calibration_results$pred_bin_num <- cut(calibration_results$predicted_win_prob,
                                          breaks = seq(0, 1, 0.1),
                                          include.lowest = TRUE)
  
  bin_summary <- aggregate(cbind(actual_win, predicted_win_prob) ~ pred_bin_num,
                           data = calibration_results,
                           FUN = function(x) c(mean = mean(x), n = length(x)))
  
  bin_summary$pred_mean <- bin_summary$predicted_win_prob[, 1]
  bin_summary$actual_mean <- bin_summary$actual_win[, 1]
  bin_summary$n <- bin_summary$actual_win[, 2]
  
  p1 <- ggplot(bin_summary, aes(x = pred_mean, y = actual_mean)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Calibration Curve",
         subtitle = "Perfect calibration = points on diagonal line",
         x = "Predicted Win Probability",
         y = "Actual Win Rate",
         size = "N") +
    xlim(0, 1) + ylim(0, 1) +
    theme_minimal()
  
  p2 <- ggplot(calibration_results, aes(x = predicted_win_prob, fill = factor(actual_win))) +
    geom_histogram(bins = 20, alpha = 0.6, position = "identity") +
    labs(title = "Distribution of Predictions",
         x = "Predicted Win Probability",
         y = "Count",
         fill = "Actual Win") +
    theme_minimal()
  
  print(p1)
  print(p2)
  
  return(list(calibration = p1, distribution = p2))
}

plots <- plot_calibration(calibration_results)

# Reduction in Error

rmse <- function(y, p) sqrt(mean((y - p)^2))
mse <- function(y, p) mean((y - p)^2)

# Extract actual outcomes and predictions from calibration results
y_actual <- calibration_results$actual_win
y_pred <- calibration_results$predicted_win_prob

# Compute RMSE and MSE for state space model
state_space_rmse <- rmse(y_actual, y_pred)
state_space_mse <- mse(y_actual, y_pred)

# Baselines for error reduction
naive_rmse <- sqrt(mean((y_actual - 0.5)^2))        
prop_rmse <- sqrt(mean((y_actual - mean(y_actual))^2)) 
naive_mse <- mean((y_actual - 0.5)^2)        
prop_mse <- mean((y_actual - mean(y_actual))^2)

# Error reduction table
error_reduction <- data.frame(
  Model = "State Space Model",
  RMSE = state_space_rmse,
  MSE = state_space_mse,
  RMSE_vs_Naive = paste0(round(100 * (naive_rmse - state_space_rmse) / naive_rmse, 2), "%"),
  MSE_vs_Naive = paste0(round(100 * (naive_mse - state_space_mse) / naive_mse, 2), "%"),
  RMSE_vs_Prop = paste0(round(100 * (prop_rmse - state_space_rmse) / prop_rmse, 2), "%"),
  MSE_vs_Prop = paste0(round(100 * (prop_mse - state_space_mse) / prop_mse, 2), "%")
)

cat("\n=== Error Reduction vs Baselines ===\n")
print(error_reduction)

cat("\n=== Baseline Performance ===\n")
cat("Naive (0.5) RMSE:", naive_rmse, "\n")
cat("Naive (0.5) MSE:", naive_mse, "\n")
cat("Proportion RMSE:", prop_rmse, "\n")
cat("Proportion MSE:", prop_mse, "\n")





