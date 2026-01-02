library(tidyverse)
library(stringr)
library(patchwork)
library(ggplot2)
library(xgboost)
library(Matrix)
library(randomForest)
library(tidyr)
library(isotone)

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

# Seconds remaining in half
phase_data <- phase_data %>%
  mutate(
    Seconds_Remaining_Half = if_else(
      Seconds_Remaining > 2400,             # first half
      Seconds_Remaining - 2400,             # seconds remaining in first half
      Seconds_Remaining                     # second half: already less than 2400
    )
  )



# Adding final game state if not present

if(nrow(games_missing_final) > 0) {
  phase_data <- phase_data %>%
    filter(Seconds_Remaining > 0) %>%
    group_by(Home, Away, Round) %>%
    slice_tail(n = 1) %>%
    mutate(Seconds_Remaining = 0, first_phase = 1) %>%
    bind_rows(phase_data, .) %>%
    arrange(Team_for_join, Round, desc(Seconds_Remaining))
}

phase_data %>%
  group_by(Home, Away, Round) %>%
  summarise(has_final = any(Seconds_Remaining == 0), n_rows = n(), .groups = "drop")


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
  "Card_Diff_Team",
  "Possession",
  "adjusted_diff",
  "total_points_both"
)

# Converting categorical variables to factors and making sure Win is numeric

wp_model <- phase_team %>%
  select(Win, Round, all_of(predictors)) %>%
  mutate(
    Win = as.numeric(as.character(Win)),
    across(where(is.character), as.factor)
  ) %>%
  na.omit()

# Setting seed

set.seed(3)

# Splitting and shuffling data
wp_shuffled <- wp_model %>%
  dplyr::select(-Round) %>%
  dplyr::slice_sample(prop = 1)

n_train <- floor(0.8 * nrow(wp_shuffled))
train_data <- wp_shuffled[1:n_train, ]
test_data  <- wp_shuffled[(n_train + 1):nrow(wp_shuffled), ]

# Random Forest

library(randomForest)

rf_fit <- randomForest(
  Win ~ .,
  data = train_data,
  ntree = 250,
  mtry = 3,
  nodesize = 25,
  importance = TRUE
)

rf_pred <- predict(rf_fit, newdata = test_data)

# Variable importance plot

var_imp <- as.data.frame(importance(rf_fit)) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(IncNodePurity))

var_imp_plot <- ggplot(
  var_imp,
  aes(x = reorder(Variable, IncNodePurity), y = IncNodePurity)
) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(
    x = "Variable",
    y = "Increase in Node Purity",
    title = "Random Forest Variable Importance (Regression)"
  ) +
  theme_minimal()

ggsave(
  "plots/rf_variable_importance.png",
  var_imp_plot,
  width = 7,
  height = 5,
  dpi = 300
)

# Logistic Regression

logit_fit <- glm(
  Win ~ . - Seconds_Remaining,
  data = train_data,
  family = gaussian()  # Regression for continuous [0,1]
)

logit_pred <- predict(
  logit_fit,
  newdata = test_data,
  type = "response"
)


# XGBoost

library(xgboost)

X_train <- model.matrix(Win ~ . - 1, data = train_data)
X_test  <- model.matrix(Win ~ . - 1, data = test_data)

y_train <- train_data$Win
y_test  <- test_data$Win

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

# Monotonicity constraints
mono_constraints <- rep(0, ncol(X_train))
names(mono_constraints) <- colnames(X_train)

mono_constraints["Points_Diff_Team"] <- 1
mono_constraints["WinPct_Diff_Team"] <- 1
mono_constraints["Card_Diff_Team"]   <- 1
mono_constraints["Possession"]       <- 1
mono_constraints["adjusted_diff"]    <- 1
mono_constraints["meter_line_Team"]  <- -1

xgb_fit <- xgb.train(
  data = dtrain,
  nrounds = 2000,
  watchlist = list(train = dtrain, test = dtest),
  objective = "reg:logistic",   # regression → output in [0,1]
  monotone_constraints = mono_constraints,
  eval_metric = "rmse",
  max_depth = 3,
  eta = 0.1,
  min_child_weight = 20,
  gamma = 5,
  early_stopping_rounds = 30
)

xgb_pred <- predict(xgb_fit, dtest)


# Model Performance

table(y_true) / length(y_true)

# Reduction in Error
library(scales)
# Baselines
naive_rmse <- sqrt(mean((y_true - 0.5)^2))        # Always predict draw
prop_rmse <- sqrt(mean((y_true - mean(y_true))^2)) # Predict average outcome

# Error Reduction
rf_reduction_naive <- (naive_rmse - rf_rmse) / naive_rmse * 100
rf_reduction_prop <- (prop_rmse - rf_rmse) / prop_rmse * 100

# Table
error_reduction <- tibble(
  Model = c("RF", "Logistic", "XGBoost"),
  RMSE = c(rf_rmse, logit_rmse, xgb_rmse),
  vs_Naive_Reduction = percent((naive_rmse - c(rf_rmse, logit_rmse, xgb_rmse)) / naive_rmse),
  vs_Prop_Reduction = percent((prop_rmse - c(rf_rmse, logit_rmse, xgb_rmse)) / prop_rmse)
)

error_reduction

# RMSE

rmse <- function(y, p) sqrt(mean((y - p)^2))

y_true <- as.numeric(as.character(test_data$Win))

# Compute RMSE
rf_rmse <- rmse(y_true, rf_pred)
logit_rmse <- rmse(y_true, logit_pred)
xgb_rmse <- rmse(y_true, xgb_pred)

# Log-loss

log_loss_3way <- function(y_true, p_pred, eps = 1e-15) {
  p_pred <- pmax(pmin(p_pred, 1 - eps), eps)
  -mean(
    case_when(
      y_true == 1 ~ log(p_pred),
      y_true == 0 ~ log(1 - p_pred),
      y_true == 0.5 ~ 0.5 * log(p_pred) + 0.5 * log(1 - p_pred)  # Draw hedge
    )
  )
}

rf_logloss <- log_loss_3way(y_true, rf_pred)
logit_logloss <- log_loss_3way(y_true, logit_pred)
xgb_logloss <- log_loss_3way(y_true, xgb_pred)

results <- tibble(
  Model = c("Logistic", "Random Forest", "XGBoost"),
  RMSE = c(logit_rmse, rf_rmse, xgb_rmse),
  LogLoss = c(logit_logloss, rf_logloss, xgb_logloss)
)

results %>% arrange(LogLoss)

#########################
### Model Calibration ###
#########################

# Calibration (using out-of-sample test data)

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


# Calibration tables (using out-of-sample test data)

cal_logit <- calibration_df(
  logit_pred,
  y_test
)

cal_rf <- calibration_df(
  rf_pred,
  y_test
)

cal_xgb <- calibration_df(
  xgb_pred,
  y_test
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

ggsave("plots/calibration_logistic.png", cal_plot_logit, width = 6, height = 5, dpi = 300)
ggsave("plots/calibration_rf.png",        cal_plot_rf,    width = 6, height = 5, dpi = 300)
ggsave("plots/calibration_xgboost.png",   cal_plot_xgb,   width = 6, height = 5, dpi = 300)


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


X_example <- model.matrix(
  ~ . - 1,
  data = example_game[, predictors]
)

xgb_matrix_example <- xgb.DMatrix(
  data = X_example
)

example_game$Win_Prob_XGB <- predict(
  xgb_fit,
  xgb_matrix_example
)

example_game$Win_Prob_RF <- predict(
  rf_fit,
  newdata = example_game
)

example_game$Win_Prob_Logit <- predict(
  logit_fit,
  newdata = example_game,
  type = "response"
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

ggsave("plots/example_game_plot.png", example_plot, width = 10, height = 4, dpi = 300)


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
  
  win_prob <- predict(
    rf_fit,
    newdata = newdata
  )
  
  data.frame(
    value = grid_vals,
    win_prob = win_prob
  )
}

partial_rf_seconds_by_score <- function(
    seconds_var = "Seconds_Remaining",
    points_var  = "Points_Diff_Team",
    adj_var     = "adjusted_diff",
    score_vals  = c(-7, 7),
    grid_length = 50
) {
  
  grid_vals <- seq(
    min(train_data[[seconds_var]], na.rm = TRUE),
    max(train_data[[seconds_var]], na.rm = TRUE),
    length.out = grid_length
  )
  
  pd_list <- lapply(score_vals, function(s) {
    
    newdata <- baseline[rep(1, grid_length), ]
    
    # Set time
    newdata[[seconds_var]] <- grid_vals
    
    # Set score state
    newdata[[points_var]] <- s
    
    newdata[[adj_var]]    <- s / sqrt(grid_vals + 1)
    
    data.frame(
      Seconds_Remaining = grid_vals,
      win_prob = predict(rf_fit, newdata),
      Score_State = ifelse(
        s > 0,
        paste0("+", s, " (Good)"),
        paste0(s, " (Bad)")
      )
    )
  })
  
  dplyr::bind_rows(pd_list)
}

pd_list <- list(
  "Field_Position"     = partial_rf("meter_line_Team"),
  "Points_Differential"= partial_rf("Points_Diff_Team"),
  "Team_Strength"      = partial_rf("WinPct_Diff_Team"),
  "Card_Differential"  = partial_rf("Card_Diff_Team"),
  "total_points_both"  = partial_rf("total_points_both")
)

pd_seconds <- partial_rf_seconds_by_score()

for (name in names(pd_list)) {
  
  p <- ggplot(pd_list[[name]], aes(value, win_prob)) +
    geom_line(linewidth = 1) +
    labs(
      x = name,
      y = "Win Probability",
      title = paste("Partial Dependence:", name)
    ) +
    theme_minimal()
  
  ggsave(
    paste0("plots/partial_", tolower(gsub(" ", "_", name)), ".png"),
    p,
    width = 7,
    height = 5,
    dpi = 300
  )
}

p_seconds <- ggplot(
  pd_seconds,
  aes(Seconds_Remaining, win_prob, color = Score_State)
) +
  geom_line(linewidth = 1) +
  labs(
    x = "Seconds Remaining",
    y = "Win Probability",
    title = "Partial Dependence of Time Remaining\nby Score Differential"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("firebrick", "steelblue")
  )

ggsave(
  "plots/partial_seconds_remaining_by_score.png",
  p_seconds,
  width = 7,
  height = 5,
  dpi = 300
)

# Plotting xgboost partials

partial_xgb <- function(var, grid_length = 50) {
  
  grid_vals <- seq(
    min(train_data[[var]], na.rm = TRUE),
    max(train_data[[var]], na.rm = TRUE),
    length.out = grid_length
  )
  
  newdata <- baseline[rep(1, grid_length), predictors, drop = FALSE]
  newdata[[var]] <- grid_vals
  
  X_new <- model.matrix(
    ~ . - 1,
    data = newdata
  )
  
  # Ensure column alignment
  X_new <- X_new[, colnames(X_train), drop = FALSE]
  
  xgb_mat <- xgb.DMatrix(X_new)
  
  data.frame(
    value = grid_vals,
    win_prob = predict(xgb_fit, xgb_mat)
  )
}

partial_xgb_seconds_by_score <- function(
    seconds_var = "Seconds_Remaining",
    points_var  = "Points_Diff_Team",
    adj_var     = "adjusted_diff",
    score_vals  = c(-7, 7),
    grid_length = 50
) {
  
  grid_vals <- seq(
    min(train_data[[seconds_var]], na.rm = TRUE),
    max(train_data[[seconds_var]], na.rm = TRUE),
    length.out = grid_length
  )
  
  pd_list <- lapply(score_vals, function(s) {
    
    newdata <- baseline[rep(1, grid_length), predictors, drop = FALSE]
    
    # Set time
    newdata[[seconds_var]] <- grid_vals
    
    # Set score state
    newdata[[points_var]] <- s
    
    # Derived variables (MUST be consistent)
    newdata[[adj_var]]    <- s / sqrt(grid_vals + 1)
    
    # Build matrix exactly as in training
    X_new <- model.matrix(
      ~ . - 1,
      data = newdata
    )
    
    # Enforce column alignment
    X_new <- X_new[, colnames(X_train), drop = FALSE]
    
    xgb_mat <- xgb.DMatrix(X_new)
    
    data.frame(
      Seconds_Remaining = grid_vals,
      win_prob = predict(xgb_fit, xgb_mat),
      Score_State = ifelse(
        s > 0,
        paste0("+", s, " (Good)"),
        paste0(s, " (Bad)")
      )
    )
  })
  
  dplyr::bind_rows(pd_list)
}


pd_list_xgb <- list(
  "Field_Position"      = partial_xgb("meter_line_Team"),
  "Points_Differential" = partial_xgb("Points_Diff_Team"),
  "Team_Strength"       = partial_xgb("WinPct_Diff_Team"),
  "Card_Differential"   = partial_xgb("Card_Diff_Team"),
  "total_points_both"   = partial_xgb("total_points_both")
)

pd_seconds_xgb <- partial_xgb_seconds_by_score()

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
    paste0("plots/partial_xgb_", tolower(gsub(" ", "_", name)), ".png"),
    p,
    width = 7,
    height = 5,
    dpi = 300
  )
}

p_seconds_xgb <- ggplot(
  pd_seconds_xgb,
  aes(Seconds_Remaining, win_prob, color = Score_State)
) +
  geom_line(linewidth = 1) +
  labs(
    x = "Seconds Remaining",
    y = "Win Probability",
    title = "Partial Dependence (XGBoost): Time Remaining\nby Score Differential"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("firebrick", "steelblue")
  )

ggsave(
  "plots/partial_xgb_seconds_remaining_by_score.png",
  p_seconds_xgb,
  width = 7,
  height = 5,
  dpi = 300
)




