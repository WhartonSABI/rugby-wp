#############
### SETUP ###
#############

# Shared data preparation for Markov/DP win probability models.
# Returns phase_data with engineered features.

library(dplyr)
library(stringr)
library(tidyr)

prepare_phase_data <- function(file_path = "data/raw/phase_2018-19.csv",
                               save_processed = FALSE,
                               processed_path = "data/processed/phase_data.csv") {
  # Load raw phase-level data for the season.
  phase_data <- read.csv(file_path)
  
  # Parse scoring outcomes from the string and fill missing scores with zeros.
  phase_data <- phase_data %>%
    mutate(
      points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
      points = ifelse(is.na(points), 0, points)
    )
  
  # Points_Difference is already possession-relative. Use a fixed home
  # perspective before differencing; a possession change alone is not a score.
  phase_data <- phase_data %>%
    arrange(ID) %>%
    group_by(Round, Home, Away) %>%
    mutate(
      home_margin = if_else(Team_In_Poss == "Home", Points_Difference, -Points_Difference),
      home_margin_change = home_margin - lag(home_margin, default = 0),
      points_change = abs(home_margin_change),
      scoring_event = points_change > 0,
      total_points_both = cumsum(points_change),
      home_score = (total_points_both + home_margin) / 2,
      away_score = (total_points_both - home_margin) / 2
    ) %>%
    ungroup()
  stopifnot(all(phase_data$points_change %in% c(0, 3, 5, 7)),
            all(phase_data$home_score >= 0), all(phase_data$away_score >= 0))

  # Compute rolling win percentage before each match (Elo-style features below).
  matches <- phase_data %>%
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
    ) %>%
    select(Round, Home, Away, Final_Points_Diff_Home) %>%
    distinct() %>%
    arrange(Round)
  
  # Simple Elo-style ratings derived from match outcomes.
  initial_elo <- 1500
  teams <- unique(c(matches$Home, matches$Away))
  team_current_elo <- setNames(rep(initial_elo, length(teams)), teams)
  matches$Home_Elo_Before <- NA
  matches$Away_Elo_Before <- NA
  
  for (i in seq_len(nrow(matches))) {
    home <- matches$Home[i]
    away <- matches$Away[i]
    matches$Home_Elo_Before[i] <- team_current_elo[home]
    matches$Away_Elo_Before[i] <- team_current_elo[away]
    home_result <- if (matches$Final_Points_Diff_Home[i] == 0) 0.5 else
      as.numeric(matches$Final_Points_Diff_Home[i] > 0)
    expected <- 1 / (1 + 10^((team_current_elo[away] - team_current_elo[home]) / 400))
    elo_change <- 32 * (home_result - expected)
    team_current_elo[home] <- team_current_elo[home] + elo_change
    team_current_elo[away] <- team_current_elo[away] - elo_change
  }
  
  # Attach win percentage and Elo features to each team/round.
  last_play <- matches %>%
    pivot_longer(cols = c(Home, Away), names_to = "Side", values_to = "Team") %>%
    mutate(
      Win = case_when(
        Side == "Home" & Final_Points_Diff_Home > 0  ~ 1,
        Side == "Home" & Final_Points_Diff_Home == 0 ~ 0.5,
        Side == "Home" & Final_Points_Diff_Home < 0  ~ 0,
        Side == "Away" & Final_Points_Diff_Home < 0  ~ 1,
        Side == "Away" & Final_Points_Diff_Home == 0 ~ 0.5,
        Side == "Away" & Final_Points_Diff_Home > 0  ~ 0
      ),
      Elo_Before = if_else(Side == "Home", Home_Elo_Before, Away_Elo_Before),
      Round = as.numeric(Round)
    ) %>%
    arrange(Team, Round) %>%
    group_by(Team) %>%
    mutate(
      Games_Played = lag(row_number(), default = 0),
      Wins_Before = lag(cumsum(Win), default = 0),
      WinPct_Before = if_else(Games_Played == 0, 0.5, Wins_Before / Games_Played)
    ) %>%
    ungroup() %>%
    select(-Home_Elo_Before, -Away_Elo_Before)
  
  # Join team strength features onto each phase based on possession team.
  phase_data <- phase_data %>%
    mutate(
      Team_for_join = if_else(Team_In_Poss == "Home", Home, Away)
    ) %>%
    left_join(
      last_play %>% select(Team, Round, WinPct_Before, Win, Elo_Before),
      by = c("Team_for_join" = "Team", "Round" = "Round")
    ) %>%
    mutate(Home_Attack = if_else(Team_In_Poss == "Home", 1L, 0L)) %>%
    mutate(Opponent = if_else(Team_In_Poss == "Home", Away, Home)) %>%
    left_join(
      last_play %>% select(Team, Round, WinPct_Before) %>%
        rename(Opponent = Team, Opponent_WinPct = WinPct_Before),
      by = c("Opponent", "Round")
    ) %>%
    # Add Home and Away Elo from matches table
    left_join(
      matches %>% select(Round, Home, Away, Home_Elo_Before, Away_Elo_Before),
      by = c("Round", "Home", "Away")
    ) %>%
    mutate(
      WinPct_Diff = WinPct_Before - Opponent_WinPct,
      Card_Diff = (Yellow_Cards_Opp + Red_Cards_Opp) - (Yellow_Cards_Own + Red_Cards_Own),
      # Add Elo_Diff based on who has possession (similar to WinPct_Diff)
      Elo_Diff = if_else(Team_In_Poss == "Home", 
                         Home_Elo_Before - Away_Elo_Before, 
                         Away_Elo_Before - Home_Elo_Before)
    ) %>%
    mutate(first_phase = if_else(Phase == 1, 1L, 0L))
  
  # Map field location buckets to numeric meter line.
  location_names <- c("5m-Goal (opp)", "22m-5m (opp)", "10m-22m (opp)",
                      "Half-10m (opp)", "10m-Half (own)", "22m-10m (own)",
                      "5m-22m (own)", "Goal-5m (own)")
  location_meters <- c(2.5, 13.5, 31, 45, 55, 69, 86.5, 97.5)
  lookup <- setNames(location_meters, location_names)
  phase_data$meter_line <- lookup[phase_data$Location]
  
  # Discretize score into "scores ahead" buckets for modeling.
  phase_data <- phase_data %>%
    mutate(
      Scores_Ahead = case_when(
        Points_Difference == 0 ~ 0L,
        TRUE ~ as.integer(sign(Points_Difference) * ceiling(abs(Points_Difference) / 7))
      )
    )
  
  # Optionally persist the processed dataset for reuse.
  if (save_processed) {
    dir.create(dirname(processed_path), recursive = TRUE, showWarnings = FALSE)
    write.csv(phase_data, processed_path, row.names = FALSE)
  }
  
  # Return enriched phase data for downstream modeling.
  phase_data
}
