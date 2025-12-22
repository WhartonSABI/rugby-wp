library(tidyverse)
library(stringr)
library(patchwork)
library(ggplot2)


phase_data = read.csv("/Users/kennywatts/Documents/Github/Rugby-Expected-Points/data/phase_2018-19.csv")

# preview data
head(phase_data)

phase_data <- phase_data %>%
  mutate(
    # extract signed numbers inside parentheses: (+3), (-3), etc.
    points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
    
    # handle cases with no number (e.g. "no score", turnovers)
    points = ifelse(is.na(points), 0, points)
  )

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
  pivot_longer(cols = c(Home, Away),
               names_to = "Side",
               values_to = "Team") %>%
  mutate(
    Win = case_when(
      Side == "Home" & Final_Points_Diff_Home > 0 ~ 1,
      Side == "Home" & Final_Points_Diff_Home <= 0 ~ 0,
      Side == "Away" & Final_Points_Diff_Home < 0 ~ 1,
      Side == "Away" & Final_Points_Diff_Home >= 0 ~ 0
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

# Seconds remaining in half
phase_data <- phase_data %>%
  mutate(
    Seconds_Remaining_Half = if_else(
      Seconds_Remaining > 2400,             # first half
      Seconds_Remaining - 2400,             # seconds remaining in first half
      Seconds_Remaining                     # second half: already less than 2400
    )
  )

# First phase or open play

phase_data <- phase_data %>%
  mutate(
    first_phase = if_else(
    Phase == 1,
    1,
    0
    ))

# Adding whether you have possession or not

phase_team <- phase_data %>%
  rowwise() %>%
  do({
    home_row <- .
    away_row <- .
    
    # Home perspective
    home_row$Team <- "Home"
    home_row$Possession <- as.integer(home_row$Team_In_Poss == home_row$Team)
    home_row$Win <- if_else(home_row$Possession == 1, home_row$Win,
                            1 - home_row$Win)
    
    home_row$Points_Diff_Team <- if_else(home_row$Possession == 1,
                                         home_row$Points_Difference,
                                         -home_row$Points_Difference)
    home_row$WinPct_Diff_Team <- if_else(home_row$Possession == 1,
                                         home_row$WinPct_Diff,
                                         -home_row$WinPct_Diff)
    home_row$Card_Diff_Team <- if_else(home_row$Possession == 1,
                                       home_row$Card_Diff,
                                       -home_row$Card_Diff)
    
    # Away perspective
    away_row$Team <- "Away"
    away_row$Possession <- as.integer(away_row$Team_In_Poss == away_row$Team)
    away_row$Win <- if_else(away_row$Possession == 1, away_row$Win,
                            1 - away_row$Win)
    away_row$Points_Diff_Team <- if_else(away_row$Possession == 1,
                                         away_row$Points_Difference,
                                         -away_row$Points_Difference)
    away_row$WinPct_Diff_Team <- if_else(away_row$Possession == 1,
                                         away_row$WinPct_Diff,
                                         -away_row$WinPct_Diff)
    away_row$Card_Diff_Team <- if_else(away_row$Possession == 1,
                                         away_row$Card_Diff,
                                         -away_row$Card_Diff)
    
    bind_rows(home_row, away_row)
  }) %>%
  ungroup()

# Adding adjusted score (like Lock & Nettleton (2014))

phase_team <- phase_team %>%
  mutate(adjusted_diff = Points_Diff_Team/((Seconds_Remaining+1)^0.5))


###########################
### Random Forest Model ###
###########################

# Selecting predictors
predictors <- c(
  "Location",
  "Points_Diff_Team",
  "Seconds_Remaining",
  "WinPct_Diff_Team",
  "Card_Diff_Team",
  "Possession",
  "adjusted_diff"
)

# Not using: first_phase

# Converting categorical variables to factors
wp_model <- phase_team %>%
  dplyr::select(Win, Round, all_of(predictors)) %>%
  mutate(
    across(where(is.character), as.factor),
    Win = as.factor(Win)
  ) %>%
  na.omit()

# Splitting and shuffling data

wp_shuffled <- wp_model %>%
  dplyr::select(-Round) %>%
  dplyr::slice_sample(prop = 1)

n_train <- floor(0.8 * nrow(wp_shuffled))

train_data <- wp_shuffled[1:n_train, ]
test_data  <- wp_shuffled[(n_train + 1):nrow(wp_shuffled), ]

# Fitting
library(randomForest)

rf_fit <- randomForest(
  Win ~ .,
  data = train_data,
  ntree = 250,
  mtry = 2,
  importance = TRUE
)

print(rf_fit)

# Performance
pred_class <- predict(rf_fit, test_data)
pred_prob  <- predict(rf_fit, test_data, type = "prob")[, "1"]

# Confusion Matrix
table(
  Predicted = pred_class,
  Actual   = test_data$Win
)

# Accuracy
mean(pred_class == test_data$Win)

# Variable importance
varImpPlot(rf_fit, type = 2)

# Calibration

rf_prob <- predict(rf_fit, train_data, type = "prob")[, "1"]

calib_fit <- glm(
  Win ~ rf_prob,
  data = train_data,
  family = binomial
)

calib_fit

# Win Prob of a test game (Exeter Perspective)

example_game <- phase_team %>%
  filter(
    Round == 22,
    Home == "Exeter",
    Away == "Northampton",
    Home_Attack == 1,
    Team == "Home"
  )

#example_game <- phase_team %>%
#  filter(
#    Round == 22,
#    Home == "Worcester",
#    Away == "Saracens",
#    Home_Attack == 1,
#    Team == "Home"
#  )

example_game <- example_game %>%
  dplyr::select(all_of(predictors)) %>%
  mutate(
    across(where(is.character), as.factor)
  )

# Aligning factors
example_game <- example_game %>%
  mutate(
    across(
      where(is.factor),
      ~ factor(.x, levels = levels(wp_model[[cur_column()]]))
    )
  )

example_game$Win_Prob <- predict(
  rf_fit,
  newdata = example_game,
  type = "prob"
)[, "1"]

# Plotting win probs
ggplot(example_game, aes(
  x = Seconds_Remaining,
  y = Win_Prob
)) +
  geom_line(linewidth = 1) +
  scale_x_reverse() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Win Probability: Exeter vs Northampton",
    x = "Seconds Remaining",
    y = "Exeter Win Probability"
  ) +
  theme_minimal()

###############
### XGBoost ###
###############

library(xgboost)
library(Matrix)

# Response
y_train <- train_data$Win
y_test  <- test_data$Win

# Design matrices (drop intercept)
X_train <- model.matrix(Win ~ . - 1, data = train_data)
X_test  <- model.matrix(Win ~ . - 1, data = test_data)

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test, label = y_test)

feature_names <- colnames(X_train)

# Monotinicity Constraints

mono_constraints <- rep(0, length(feature_names))
names(mono_constraints) <- feature_names

mono_constraints["Points_Diff_Team"] <- 1
mono_constraints["WinPct_Diff_Team"] <- 1
mono_constraints["Card_Diff_Team"]   <- 1
mono_constraints["Possession"]       <- 1
mono_constraints["adjusted_diff"]    <- 1

mono_string <- paste(mono_constraints, collapse = ",")

xgb_fit <- xgb.train(
  data = dtrain,
  nrounds = 500,
  objective = "binary:logistic",
  monotone_constraints = mono_string,
  eval_metric = "logloss",
  max_depth = 4,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Probabilities
xgb_prob <- predict(xgb_fit, dtest)

# Classes
xgb_class <- ifelse(xgb_prob > 0.5, 1, 0)

# Accuracy
mean(xgb_class == y_test)

# Confusion matrix
table(
  Predicted = xgb_class,
  Actual   = y_test
)

xgb_prob_train <- predict(xgb_fit, dtrain)

calib_fit <- glm(
  Win ~ xgb_prob_train,
  family = binomial
)

###########################
### Logistic Regression ###
###########################

glm_train <- train_data %>%
  mutate(Win = as.numeric(as.character(Win)))

glm_fit <- glm(
  Win ~ .,
  data = glm_train,
  family = binomial(link = "logit")
)

summary(glm_fit)

# Predicting on test data

glm_test <- test_data %>%
  mutate(Win = as.numeric(as.character(Win)))

test_data$Win_Prob_Logit <- predict(
  glm_fit,
  newdata = glm_test,
  type = "response"   # probabilities
)

# Model performance

# Classification

test_data$Pred_Class <- ifelse(test_data$Win_Prob_Logit > 0.5, 1, 0)

table(
  Predicted = test_data$Pred_Class,
  Actual   = glm_test$Win
)

# Accuracy

mean(test_data$Pred_Class == glm_test$Win)

# Comparing to Random forest

test_data$Win_Prob_RF <- predict(
  rf_fit,
  newdata = test_data,
  type = "prob"
)[, "1"]

cor(test_data$Win_Prob_Logit, test_data$Win_Prob_RF)

# Callibration

test_data %>%
  mutate(
    Win_num = as.numeric(as.character(Win)),
    bin = ntile(Win_Prob_Logit, 10)
  ) %>%
  group_by(bin) %>%
  summarise(
    mean_pred = mean(Win_Prob_Logit),
    win_rate  = mean(Win_num),
    n = n(),
    .groups = "drop"
  )

# Example Game

example_game$Win_Prob_Logit <- predict(
  glm_fit,
  newdata = example_game,
  type = "response"
)

ggplot(example_game, aes(
  x = Seconds_Remaining,
  y = Win_Prob_Logit
)) +
  geom_line() +
  scale_x_reverse() +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent) +
  labs(
    title = "Logistic Win Probability: Exeter vs Northampton",
    y = "Exeter Win Probability"
  ) +
  theme_minimal()
