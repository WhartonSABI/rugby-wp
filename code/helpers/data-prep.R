#############
### SETUP ###
#############

# Shared data preparation for Markov/DP win probability models.
# Loads phase data, builds last_play (win %, Elo), joins to phase_data.
# Expects: run from project root; data at data/raw/phase_2018-19.csv

library(dplyr)
library(stringr)
library(tidyr)

#################
### DATA LOAD ###
#################

phase_data <- read.csv("data/raw/phase_2018-19.csv")

#####################
### MATCH CONTEXT ###
#####################

phase_data <- phase_data %>%
  mutate(
    points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
    points = ifelse(is.na(points), 0, points)
  )

phase_data <- phase_data %>%
  group_by(Round, Home, Away) %>%
  mutate(
    prev_diff = lag(Points_Difference, default = first(Points_Difference)),
    scoring_event = abs(Points_Difference) != abs(prev_diff) & row_number() > 1,
    points_change = if_else(scoring_event, abs(lag(points)), 0),
    total_points_both = cumsum(points_change)
  ) %>%
  ungroup()

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
  ) %>%
  dplyr::select(Round, Home, Away, Final_Points_Diff_Home) %>%
  distinct()

last_play <- last_play %>%
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
    Round = as.numeric(Round)
  ) %>%
  arrange(Team, Round)

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
  home_won <- matches$Final_Points_Diff_Home[i] > 0
  if (home_won) {
    expected <- 1 / (1 + 10^((team_current_elo[away] - team_current_elo[home]) / 400))
    elo_change <- 32 * (1 - expected)
    team_current_elo[home] <- team_current_elo[home] + elo_change
    team_current_elo[away] <- team_current_elo[away] - elo_change
  } else {
    expected <- 1 / (1 + 10^((team_current_elo[home] - team_current_elo[away]) / 400))
    elo_change <- 32 * (1 - expected)
    team_current_elo[away] <- team_current_elo[away] + elo_change
    team_current_elo[home] <- team_current_elo[home] - elo_change
  }
}

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

################
### FEATURES ###
################

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
  mutate(
    WinPct_Diff = WinPct_Before - Opponent_WinPct,
    Card_Diff = (Yellow_Cards_Opp + Red_Cards_Opp) - (Yellow_Cards_Own + Red_Cards_Own)
  ) %>%
  mutate(first_phase = if_else(Phase == 1, 1L, 0L))

location_names <- c("5m-Goal (opp)", "22m-5m (opp)", "10m-22m (opp)",
                   "Half-10m (opp)", "10m-Half (own)", "22m-10m (own)",
                   "5m-22m (own)", "Goal-5m (own)")
location_meters <- c(2.5, 13.5, 31, 45, 55, 69, 86.5, 97.5)
lookup <- setNames(location_meters, location_names)
phase_data$meter_line <- lookup[phase_data$Location]

phase_data <- phase_data %>%
  mutate(
    Scores_Ahead = case_when(
      Points_Difference == 0 ~ 0L,
      TRUE ~ as.integer(sign(Points_Difference) * ceiling(abs(Points_Difference) / 7))
    )
  )
