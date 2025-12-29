library(tidyverse)
library(stringr)
library(patchwork)
library(ggplot2)
library(xgboost)
library(Matrix)
library(randomForest)
library(tidyr)
library(isotone)

setwd("/Users/kennywatts/Documents/Github/Rugby_wp")

phase_data = read.csv("/Users/kennywatts/Documents/Github/Rugby-Expected-Points/data/phase_2018-19.csv")

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
    home_row$Scores_Ahead_Team <- if_else(home_row$Possession == 1,
                                        home_row$Scores_Ahead,
                                        -home_row$Scores_Ahead)
    home_row$Card_Diff_Team <- if_else(home_row$Possession == 1,
                                       home_row$Card_Diff,
                                       -home_row$Card_Diff)
    home_row$meter_line_Team <- if_else(home_row$Possession == 1,
                                   home_row$meter_line,
                                   100 - home_row$meter_line)
    
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
    away_row$Scores_Ahead_Team <- if_else(away_row$Possession == 1,
                                          away_row$Scores_Ahead,
                                          -away_row$Scores_Ahead)
    away_row$Card_Diff_Team <- if_else(away_row$Possession == 1,
                                         away_row$Card_Diff,
                                         -away_row$Card_Diff)
    away_row$meter_line_Team <- if_else(away_row$Possession == 1,
                                        away_row$meter_line,
                                        100 - away_row$meter_line)
    
    
    bind_rows(home_row, away_row)
  }) %>%
  ungroup()

# Adding adjusted score (like Lock & Nettleton (2014))

phase_team <- phase_team %>%
  mutate(adjusted_diff = Points_Diff_Team/((Seconds_Remaining+1)^0.5))

######################
### Model Training ###
######################

# Selecting predictors
predictors <- c(
  "meter_line_Team",
  "Points_Diff_Team",
  "Seconds_Remaining",
  "WinPct_Diff_Team",
  "Scores_Ahead_Team",
  "Card_Diff_Team",
  "Possession",
  "adjusted_diff"
)

# Converting categorical variables to factors and recoding draw as 0
wp_model <- phase_team %>%
  dplyr::select(Win, Round, all_of(predictors)) %>%
  mutate(
    across(where(is.character), as.factor),
    # Recode Win: 1 = win, 0 = loss or draw
    Win = ifelse(Win == 1, 1, 0)
  ) %>%
  na.omit()

# Splitting and shuffling data
wp_shuffled <- wp_model %>%
  dplyr::select(-Round) %>%
  dplyr::slice_sample(prop = 1)

n_train <- floor(0.8 * nrow(wp_shuffled))
train_data <- wp_shuffled[1:n_train, ]
test_data  <- wp_shuffled[(n_train + 1):nrow(wp_shuffled), ]

logit_train <- train_data %>%
  mutate(Win = as.numeric(as.character(Win)))

logit_test <- test_data %>%
  mutate(Win = as.numeric(as.character(Win)))

train_data <- train_data %>%
  mutate(Win = as.factor(Win))

test_data <- test_data %>%
  mutate(Win = as.factor(Win))

# Random Forest

library(randomForest)

rf_fit <- randomForest(
  Win ~ .,
  data = train_data,
  ntree = 500,
  mtry = 3,
  nodesize = 50,
  importance = TRUE
)

print(rf_fit)

# Variable importance plot
var_imp <- as.data.frame(rf_fit$importance) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(MeanDecreaseAccuracy))

var_imp_plot <- ggplot(var_imp, aes(x = reorder(Variable, MeanDecreaseAccuracy), 
                                    y = MeanDecreaseAccuracy)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(x = "Variable", y = "Mean Decrease Accuracy", 
       title = "Random Forest Variable Importance") +
  theme_minimal()

ggsave("rf_variable_importance.png", var_imp_plot, width = 7, height = 5, dpi = 300)

rf_prob <- predict(
  rf_fit,
  newdata = test_data,
  type = "prob"
)[, "1"]

# Logistic Regression

logit_fit <- glm(
  Win ~ . - Seconds_Remaining,
  data = logit_train,
  family = binomial(link = "logit")
)

logit_prob <- predict(logit_fit, 
                      newdata = logit_test, 
                      type = "response")


# XGBoost

library(xgboost)

y_train <- as.numeric(as.character(train_data$Win))
y_test  <- as.numeric(as.character(test_data$Win))

X_train <- model.matrix(Win ~ . - 1, data = train_data)
X_test  <- model.matrix(Win ~ . - 1, data = test_data)

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test, label = y_test)

mono_constraints <- rep(0, ncol(train_data) - 1)
names(mono_constraints) <- colnames(train_data)[-1]

mono_constraints["Points_Diff_Team"] <- 1
mono_constraints["WinPct_Diff_Team"] <- 1
mono_constraints["Scores_Ahead_Team"] <- 1
mono_constraints["Card_Diff_Team"]   <- 1
mono_constraints["Possession"]       <- 1
mono_constraints["adjusted_diff"]    <- 1
mono_constraints["meter_line_Team"]  <- -1

xgb_fit <- xgb.train(
  data = dtrain,
  nrounds = 2000,
  watchlist = list(train = dtrain, test = dtest),
  objective = "binary:logistic",
  monotone_constraints = mono_constraints,
  eval_metric = "logloss",
  max_depth = 3,
  eta = 0.03,
  subsample = 0.7,
  colsample_bytree = 0.7,
  min_child_weight = 50,
  gamma = 1,
  early_stopping_rounds = 30
)

xgb_prob <- predict(xgb_fit, dtest)


# Model Performance

rmse <- function(y, p) sqrt(mean((y - p)^2))

y_true <- as.numeric(as.character(test_data$Win))

# Compute RMSE
rf_rmse <- rmse(y_true, rf_prob)
logit_rmse <- rmse(y_true, logit_prob)
xgb_rmse <- rmse(y_true, xgb_prob)

results <- tibble(
  Model = c("Logistic", "Random Forest", "XGBoost"),
  RMSE  = c(logit_rmse, rf_rmse, xgb_rmse)
)

results


#########################
### Model Calibration ###
#########################

# Calibration

# Random Forest
train_data$Win_Prob_RF <- predict(
  rf_fit,
  newdata = train_data,
  type = "prob"
)[, "1"]


# Logistic
logit_train$Win_Prob_Logit <- predict(
  logit_fit,
  newdata = logit_train,
  type = "response"
)

# XGBoost
train_data$Win_Prob_XGB <- predict(
  xgb_fit,
  newdata = dtrain
)

calibration_df <- function(prob, y, n_bins = 20) {
  tibble(prob = prob, y = y) %>%
    # Fixed bins along the predicted probability axis (0 to 1)
    mutate(
      bin = cut(
        prob,
        breaks = seq(0, 1, length.out = n_bins + 1),
        include.lowest = TRUE,
        labels = FALSE
      )
    ) %>%
    group_by(bin) %>%
    summarise(
      # Mean predicted probability in the bin
      mean_pred = mean(prob),
      # Observed proportion of wins in the bin
      win_rate  = mean(y),
      n = n(),
      .groups = "drop"
    )
}


# Calibration tables

cal_logit <- calibration_df(
  logit_train$Win_Prob_Logit,
  logit_train$Win
)

cal_rf <- calibration_df(
  train_data$Win_Prob_RF,
  as.numeric(as.character(train_data$Win))
)

cal_xgb <- calibration_df(
  train_data$Win_Prob_XGB,
  as.numeric(as.character(train_data$Win))
)

cal_all <- bind_rows(
  cal_logit %>% mutate(model = "Logistic"),
  cal_rf    %>% mutate(model = "Random Forest"),
  cal_xgb   %>% mutate(model = "XGBoost")
)

# Calibration plot

plot_calibration <- function(cal_df, model_name) {
  ggplot(cal_df, aes(mean_pred, win_rate)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", colour = "grey50"
    ) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(
      title = paste("Calibration Curve —", model_name),
      x = "Predicted Win Probability",
      y = "Observed Win Rate"
    ) +
    theme_minimal()
}

cal_plot_logit <- plot_calibration(cal_logit, "Logistic Regression")
cal_plot_rf    <- plot_calibration(cal_rf, "Random Forest")
cal_plot_xgb   <- plot_calibration(cal_xgb, "XGBoost")

ggsave("calibration_logistic.png", cal_plot_logit, width = 6, height = 5, dpi = 300)
ggsave("calibration_rf.png",        cal_plot_rf,    width = 6, height = 5, dpi = 300)
ggsave("calibration_xgboost.png",   cal_plot_xgb,   width = 6, height = 5, dpi = 300)


# R Squared

r2_calibration <- cal_all %>%
  group_by(model) %>%
  summarise(
    r2 = 1 - sum((win_rate - mean_pred)^2) / sum((win_rate - mean(win_rate))^2),
    .groups = "drop"
  )

r2_calibration

######################
### Game example #####
######################

# Example Game Random Forest

example_game <- phase_team %>%
  filter(
    Round == 22,
    Home == "Exeter",
    Away == "Northampton",
    Home_Attack == 1,
    Team == "Home"
  )

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

example_game$Win_Prob_RF <- predict(
  rf_fit,
  newdata = example_game,
  type = "prob"
)[, "1"]

example_game$Win_Prob_Logit <- predict(
  logit_fit,
  newdata = example_game,
  type = "response"
)

xgb_matrix_example <- xgb.DMatrix(
  data = as.matrix(example_game[, predictors])
)

example_game$Win_Prob_XGB <- predict(
  xgb_fit,
  xgb_matrix_example
)

example_long <- example_game %>%
  select(
    Seconds_Remaining,
    Win_Prob_RF,
    Win_Prob_Logit,
    Win_Prob_XGB
  ) %>%
  pivot_longer(
    cols = starts_with("Win_Prob"),
    names_to = "Model",
    values_to = "Win_Prob"
  ) %>%
  mutate(
    Model = recode(
      Model,
      Win_Prob_RF    = "Random Forest",
      Win_Prob_Logit = "Logistic",
      Win_Prob_XGB   = "XGBoost"
    )
  )

# Scoring events

score_changes <- example_game %>%
  arrange(desc(Seconds_Remaining)) %>%
  mutate(
    score_change = Points_Diff_Team != lag(Points_Diff_Team)
  ) %>%
  filter(score_change) %>%
  select(Seconds_Remaining, Points_Diff_Team)

# Plotting Game

example_plot <- ggplot(example_long, aes(
  x = Seconds_Remaining,
  y = Win_Prob
)) +
  geom_line(linewidth = 1) +
  geom_vline(
    data = score_changes,
    aes(xintercept = Seconds_Remaining),
    linetype = "dotted",
    colour = "grey40"
  ) +
  scale_x_reverse() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  facet_wrap(~ Model, nrow = 1) +
  labs(
    title = "Win Probability — Exeter vs Northampton (Round 22)",
    x = "Seconds Remaining",
    y = "Exeter Win Probability"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold")
  )

ggsave("example_game_plot.png", example_plot, width = 10, height = 4, dpi = 300)


# Plotting Random Forest Partials

# Baseline values
baseline <- train_data %>%
  summarise(
    across(
      all_of(predictors),
      ~ if (is.numeric(.)) median(., na.rm = TRUE) else {
        names(sort(table(.), decreasing = TRUE))[1]
      }
    )
  )

partial_rf <- function(var, grid_length = 50) {
  
  grid_vals <- seq(
    min(train_data[[var]], na.rm = TRUE),
    max(train_data[[var]], na.rm = TRUE),
    length.out = grid_length
  )
  
  newdata <- baseline[rep(1, grid_length), ]
  newdata[[var]] <- grid_vals
  
  newdata$Win_Prob_RF <- predict(
    rf_fit,
    newdata = newdata,
    type = "prob"
  )[,"1"]
  
  data.frame(
    value = grid_vals,
    win_prob = newdata$Win_Prob_RF
  )
}

pd_list <- list(
  "Field_Position" = partial_rf("meter_line_Team"),
  "Points_Differential" = partial_rf("Points_Diff_Team"),
  "Time_Remaining" = partial_rf("Seconds_Remaining"),
  "Team_Strength" = partial_rf("WinPct_Diff_Team"),
  "Scores_Ahead" = partial_rf("Scores_Ahead_Team"),
  "Card_Differential" = partial_rf("Card_Diff_Team")
)

for (name in names(pd_list)) {
  p <- ggplot(pd_list[[name]], aes(value, win_prob)) +
    geom_line(linewidth = 1) +
    labs(
      x = name,
      y = "Win Probability",
      title = paste("Partial Dependence:", name)
    ) +
    theme_minimal()
  
  ggsave(paste0("partial_", tolower(gsub(" ", "_", name)), ".png"),
         p, width = 7, height = 5, dpi = 300)
}

# Plotting xgboost partials

partial_xgb <- function(var, grid_length = 50) {
  
  grid_vals <- seq(
    min(train_data[[var]], na.rm = TRUE),
    max(train_data[[var]], na.rm = TRUE),
    length.out = grid_length
  )
  
  newdata <- baseline[rep(1, grid_length), ]
  newdata[[var]] <- grid_vals
  
  # XGBoost requires a matrix / DMatrix
  xgb_mat <- xgb.DMatrix(
    data = as.matrix(newdata[, predictors])
  )
  
  newdata$Win_Prob_XGB <- predict(
    xgb_fit,
    xgb_mat
  )
  
  data.frame(
    value = grid_vals,
    win_prob = newdata$Win_Prob_XGB
  )
}

pd_list_xgb <- list(
  "Field_Position"      = partial_xgb("meter_line_Team"),
  "Points_Differential" = partial_xgb("Points_Diff_Team"),
  "Time_Remaining"      = partial_xgb("Seconds_Remaining"),
  "Team_Strength"       = partial_xgb("WinPct_Diff_Team"),
  "Scores_Ahead"        = partial_xgb("Scores_Ahead_Team"),
  "Card_Differential"   = partial_xgb("Card_Diff_Team")
)

for (name in names(pd_list_xgb)) {
  p <- ggplot(pd_list_xgb[[name]], aes(value, win_prob)) +
    geom_line(linewidth = 1) +
    labs(
      x = name,
      y = "Win Probability",
      title = paste("Partial Dependence (XGBoost):", name)
    ) +
    theme_minimal()
  
  ggsave(
    paste0("partial_xgb_", tolower(gsub(" ", "_", name)), ".png"),
    p, width = 7, height = 5, dpi = 300
  )
}

