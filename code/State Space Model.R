
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
ep_model <- aggregate(points ~ state, data = phase_data, FUN = mean)
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

# 4. Determine the result of current state
phase_data$state_result <- ifelse(phase_data$points_change > 0, 
                                  paste0("Score_", phase_data$points_change),
                                  ifelse(phase_data$Team_In_Poss != phase_data$next_team & !is.na(phase_data$next_team),
                                         "Possession_Change",
                                         ifelse(phase_data$state != phase_data$next_state_result & !is.na(phase_data$next_state_result),
                                                phase_data$next_state_result,
                                                ifelse(is.na(phase_data$next_team) & phase_data$points_change == 0,
                                                       "End_of_Game",
                                                       phase_data$next_state_result))))

# View the results
table(phase_data$state_result)
head(phase_data[, c("state", "points", "Team_In_Poss", "next_team", "state_result")], 20)

#####################################
### Transition Probability Matrix ###
#####################################

transition_matrix <- prop.table(table(phase_data$state, phase_data$state_result), margin = 1)

# Inspecting Matrix

# Convert to data frame for easier viewing
transition_df <- as.data.frame.table(transition_matrix)
names(transition_df) <- c("from_state", "to_result", "probability")
transition_df <- transition_df[transition_df$probability > 0, ]  # Remove zeros
transition_df <- transition_df[order(transition_df$from_state, -transition_df$probability), ]

# View top transitions from a specific state
view_state_transitions <- function(state, transition_df) {
  subset(transition_df, from_state == state)
}

# Example: See all transitions from state "5_45"
print(view_state_transitions("5_45", transition_df))

# Summary statistics by state
state_summary <- aggregate(cbind(prob_score = probability) ~ from_state, 
                           data = transition_df[grepl("^Score_", transition_df$to_result), ],
                           FUN = sum)

poss_change_summary <- aggregate(probability ~ from_state,
                                 data = transition_df[transition_df$to_result == "Possession_Change", ],
                                 FUN = sum)
names(poss_change_summary)[2] <- "prob_poss_change"

# Merge the two summaries
state_summary <- merge(state_summary, poss_change_summary, by = "from_state", all = TRUE)

# Replace NAs with 0 (states with no scoring or no possession changes)
state_summary[is.na(state_summary)] <- 0

# Add expected points when scoring
scoring_trans <- transition_df[grepl("^Score_", transition_df$to_result), ]
scoring_trans$points_scored <- as.numeric(sub("Score_", "", scoring_trans$to_result))
scoring_trans$exp_pts <- scoring_trans$probability * scoring_trans$points_scored

exp_pts_by_state <- aggregate(exp_pts ~ from_state, data = scoring_trans, FUN = sum)
names(exp_pts_by_state)[2] <- "exp_points_from_state"

state_summary <- merge(state_summary, exp_pts_by_state, by = "from_state", all.x = TRUE)
state_summary[is.na(state_summary)] <- 0

print(state_summary)

# Heatmap of scoring probability by state
library(ggplot2)
scoring_by_state <- transition_df[grepl("^Score_", transition_df$to_result), ]
scoring_by_state$time_state <- sub("_.*", "", scoring_by_state$from_state)
scoring_by_state$meter_line <- sub(".*_", "", scoring_by_state$from_state)

score_agg <- aggregate(probability ~ time_state + meter_line, 
                       data = scoring_by_state, FUN = sum)

ggplot(score_agg, aes(x = time_state, y = meter_line, fill = probability)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(probability, 2)), size = 3) +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(title = "Probability of Scoring by State") +
  theme_minimal()


###########################
### Simulation function ###
###########################


simulate_game <- function(start_state, score_home, score_away, team_poss, 
                          transition_matrix, n_sims = 1000) {
  
  results <- data.frame(home_wins = 0, away_wins = 0, ties = 0)
  
  for (sim in 1:n_sims) {
    current_state <- start_state
    current_score_home <- score_home
    current_score_away <- score_away
    current_team <- team_poss
    max_plays <- 200  # Safety limit
    plays <- 0
    
    while (plays < max_plays) {
      plays <- plays + 1
      
      # Get possible transitions from current state
      if (!current_state %in% rownames(transition_matrix)) {
        break  # State not in matrix, end simulation
      }
      
      probs <- transition_matrix[current_state, ]
      probs <- probs[probs > 0]  # Only non-zero probabilities
      
      if (length(probs) == 0) break
      
      # Sample next event
      next_event <- sample(names(probs), 1, prob = probs)
      
      # Handle the result
      if (next_event == "End_of_Game") {
        break
        
      } else if (grepl("^Score_", next_event)) {
        # Extract points scored
        pts <- as.numeric(sub("Score_", "", next_event))
        if (current_team == "Home") {
          current_score_home <- current_score_home + pts
        } else {
          current_score_away <- current_score_away + pts
        }
        # Possession changes after score
        current_team <- ifelse(current_team == "Home", "Away", "Home")
        
      } else if (next_event == "Possession_Change") {
        current_team <- ifelse(current_team == "Home", "Away", "Home")
        
      } else {
        # Transition to new state
        current_state <- next_event
      }
    }
    
    # Record result
    if (current_score_home > current_score_away) {
      results$home_wins <- results$home_wins + 1
    } else if (current_score_away > current_score_home) {
      results$away_wins <- results$away_wins + 1
    } else {
      results$ties <- results$ties + 1
    }
  }
  
  # Calculate win probabilities
  results$home_win_prob <- results$home_wins / n_sims
  results$away_win_prob <- results$away_wins / n_sims
  results$tie_prob <- results$ties / n_sims
  
  return(results)
}

# Example usage:
# Starting at state "5_45", Home team has ball, score is Home 14 - Away 10
result <- simulate_game(start_state = "5_45", 
                        score_home = 14, 
                        score_away = 10, 
                        team_poss = "Home",
                        transition_matrix = transition_matrix,
                        n_sims = 1000)

print(result)








