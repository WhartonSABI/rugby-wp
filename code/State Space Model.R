
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



