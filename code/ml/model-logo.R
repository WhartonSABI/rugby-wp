library(tidyverse)
library(stringr)
library(patchwork)
library(ggplot2)
library(xgboost)
library(Matrix)
library(randomForest)
library(tidyr)
library(isotone)
library(scales)

# load data
phase_data = read.csv("data/raw/phase_2018-19.csv")

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
  "Card_Diff_Team",
  "Possession",
  "adjusted_diff",
  "total_points_both",
  "WinPct_Diff_Team"
)

# Converting categorical variables to factors and making sure Win is numeric

# Splitting data

phase_team <- phase_team %>%
  mutate(game_id = paste(Round, Home, Away, sep = "_"))

wp_model <- phase_team %>%
  arrange(ID) %>%
  select(game_id, Win, Round, all_of(predictors)) %>%
  mutate(
    Win = as.numeric(as.character(Win)),
    across(where(is.character), as.factor)
  ) %>%
  na.omit()

library(dplyr)
library(randomForest)
library(xgboost)
library(ggplot2)
library(tibble)

# Get unique game IDs
game_ids <- wp_model %>%
  distinct(game_id) %>%
  pull(game_id)

# Initialize storage for predictions
rf_all_predictions <- tibble()
logit_all_predictions <- tibble()
xgb_all_predictions <- tibble()

# Initialize storage for variable importance (will average across folds)
rf_var_imp_list <- list()
xgb_var_imp_list <- list()

# Leave-One-Game-Out Cross-Validation
for (i in seq_along(game_ids)) {
  
  test_game <- game_ids[i]
  
  cat(sprintf("Processing fold %d/%d: Game %s\n", i, length(game_ids), test_game))
  
  # Split data: leave one game out
  train_data <- wp_model %>%
    filter(game_id != test_game) %>%
    select(-game_id, -Round)
  
  test_data <- wp_model %>%
    filter(game_id == test_game) %>%
    select(-game_id, -Round)
  
  # Store actual outcomes and game_id for later joining
  test_game_info <- wp_model %>%
    filter(game_id == test_game) %>%
    select(game_id, Win)
  
  # ============================================================
  # 1. RANDOM FOREST
  # ============================================================
  rf_fit <- randomForest(
    Win ~ .,
    data = train_data,
    ntree = 250,
    mtry = 3,
    nodesize = 25,
    importance = TRUE
  )
  
  rf_pred <- predict(rf_fit, newdata = test_data)
  
  # Store variable importance for this fold
  rf_var_imp_list[[i]] <- as.data.frame(importance(rf_fit)) %>%
    rownames_to_column("Variable")
  
  # Store predictions
  rf_all_predictions <- bind_rows(
    rf_all_predictions,
    test_game_info %>% mutate(rf_pred = rf_pred)
  )
  
  # ============================================================
  # 2. LOGISTIC REGRESSION
  # ============================================================
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
  
  # Store predictions
  logit_all_predictions <- bind_rows(
    logit_all_predictions,
    test_game_info %>% mutate(logit_pred = logit_pred)
  )
  
  # ============================================================
  # 3. XGBOOST
  # ============================================================
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
  mono_constraints["Card_Diff_Team"]   <- 1
  mono_constraints["adjusted_diff"]    <- 1
  mono_constraints["WinPct_Diff_Team"]    <- 1
  
  xgb_fit <- xgb.train(
    data = dtrain,
    nrounds = 2000,
    watchlist = list(train = dtrain, test = dtest),
    objective = "reg:logistic",
    monotone_constraints = mono_constraints,
    eval_metric = "rmse",
    max_depth = 3,
    eta = 0.1,
    min_child_weight = 20,
    gamma = 5,
    early_stopping_rounds = 30,
    verbose = 0  # Suppress output during loop
  )
  
  xgb_pred <- predict(xgb_fit, dtest)
  
  # Store variable importance for this fold
  xgb_var_imp_list[[i]] <- xgb.importance(model = xgb_fit) %>%
    as.data.frame()
  
  # Store predictions
  xgb_all_predictions <- bind_rows(
    xgb_all_predictions,
    test_game_info %>% mutate(xgb_pred = xgb_pred)
  )
}

# ============================================================
# POST-PROCESSING: VARIABLE IMPORTANCE
# ============================================================

# Average Random Forest variable importance across all folds
rf_var_imp_avg <- rf_var_imp_list %>%
  bind_rows() %>%
  group_by(Variable) %>%
  summarise(
    IncNodePurity = mean(IncNodePurity),
    .groups = "drop"
  ) %>%
  arrange(desc(IncNodePurity))

# Average XGBoost variable importance across all folds
xgb_var_imp_avg <- xgb_var_imp_list %>%
  bind_rows() %>%
  group_by(Feature) %>%
  summarise(
    Gain = mean(Gain),
    Cover = mean(Cover),
    Frequency = mean(Frequency),
    .groups = "drop"
  ) %>%
  arrange(desc(Gain))

# Create Random Forest variable importance plot
var_imp_plot_rf <- ggplot(
  rf_var_imp_avg,
  aes(x = reorder(Variable, IncNodePurity), y = IncNodePurity)
) +
  geom_col(fill = "#2E86AB") +
  coord_flip() +
  labs(
    x = "Variable",
    y = "Increase in Node Purity (Averaged)",
    title = "LOGO CV — Random Forest Variable Importance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12)
  )

ggsave(
  "plots/ml/logo/rf_variable_importance_logo.png",
  var_imp_plot_rf,
  width = 7,
  height = 5,
  dpi = 300
)

# Create XGBoost variable importance plot (by Gain)
var_imp_plot_xgb <- ggplot(
  xgb_var_imp_avg,
  aes(x = reorder(Feature, Gain), y = Gain)
) +
  geom_col(fill = "#A23B72") +
  coord_flip() +
  labs(
    x = "Variable",
    y = "Average Gain (Averaged)",
    title = "LOGO CV — XGBoost Variable Importance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12)
  )

ggsave(
  "plots/ml/logo/xgb_variable_importance_logo.png",
  var_imp_plot_xgb,
  width = 7,
  height = 5,
  dpi = 300
)

# Combined comparison plot
var_imp_combined <- bind_rows(
  rf_var_imp_avg %>% 
    mutate(Model = "Random Forest", 
           Importance = IncNodePurity / max(IncNodePurity)) %>%  # Normalize
    select(Variable, Importance, Model),
  xgb_var_imp_avg %>% 
    mutate(Model = "XGBoost", 
           Importance = Gain / max(Gain)) %>%  # Normalize
    rename(Variable = Feature) %>%
    select(Variable, Importance, Model)
)

var_imp_comparison_plot <- ggplot(
  var_imp_combined,
  aes(x = reorder(Variable, Importance), y = Importance, fill = Model)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("Random Forest" = "#2E86AB", "XGBoost" = "#A23B72")) +
  labs(
    x = "Variable",
    y = "Normalized Importance",
    title = "LOGO CV — Variable Importance Comparison: Random Forest vs XGBoost",
    subtitle = "Averaged across LOGO cross-validation folds"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "bottom"
  )

ggsave(
  "plots/ml/logo/variable_importance_comparison_logo.png",
  var_imp_comparison_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# Save importance tables
write.csv(rf_var_imp_avg, "data/results/rf_variable_importance_logo.csv", row.names = FALSE)
write.csv(xgb_var_imp_avg, "data/results/xgb_variable_importance_logo.csv", row.names = FALSE)

cat("\n=== Variable Importance Results ===\n")
cat("Random Forest - Top 5 Variables:\n")
print(rf_var_imp_avg %>% head(5))

cat("\nXGBoost - Top 5 Variables (by Gain):\n")
print(xgb_var_imp_avg %>% head(5))

cat("\n=== Variable importance plots and tables saved ===\n")

write.csv(rf_all_predictions, "data/results/rf_logo_predictions.csv", row.names = FALSE)
write.csv(logit_all_predictions, "data/results/logit_logo_predictions.csv", row.names = FALSE)
write.csv(xgb_all_predictions, "data/results/xgb_logo_predictions.csv", row.names = FALSE)

#============================================================
# RESULTS SUMMARY
#============================================================
cat("\n=== LOGO Cross-Validation Complete ===\n")
cat(sprintf("Total games: %d\n", length(game_ids)))
cat(sprintf("RF predictions: %d rows\n", nrow(rf_all_predictions)))
cat(sprintf("Logit predictions: %d rows\n", nrow(logit_all_predictions)))
cat(sprintf("XGB predictions: %d rows\n", nrow(xgb_all_predictions)))

#########################
### Model Performance ###
#########################

# Load LOGO predictions
rf_all_predictions <- read.csv("data/results/rf_logo_predictions.csv")
logit_all_predictions <- read.csv("data/results/logit_logo_predictions.csv")
xgb_all_predictions <- read.csv("data/results/xgb_logo_predictions.csv")

# Extract actual outcomes and predictions
y_test <- rf_all_predictions$Win  # Same for all models
rf_pred <- rf_all_predictions$rf_pred
logit_pred <- logit_all_predictions$logit_pred
xgb_pred <- xgb_all_predictions$xgb_pred

# Distribution of outcomes
table(y_test) / length(y_test)

# RMSE function
rmse <- function(y, p) sqrt(mean((y - p)^2))

# MSE function
mse <- function(y, p) mean((y - p)^2)

# Compute RMSE for each model
rf_rmse <- rmse(y_test, rf_pred)
logit_rmse <- rmse(y_test, logit_pred)
xgb_rmse <- rmse(y_test, xgb_pred)

# Compute MSE for each model
rf_mse <- mse(y_test, rf_pred)
logit_mse <- mse(y_test, logit_pred)
xgb_mse <- mse(y_test, xgb_pred)

# Baselines for error reduction
naive_rmse <- sqrt(mean((y_test - 0.5)^2))        
prop_rmse <- sqrt(mean((y_test - mean(y_test))^2)) 

naive_mse <- mean((y_test - 0.5)^2)        
prop_mse <- mean((y_test - mean(y_test))^2)

# Error reduction table
error_reduction <- tibble(
  Model = c("Logistic", "Random Forest", "XGBoost"),
  RMSE = c(logit_rmse, rf_rmse, xgb_rmse),
  MSE = c(logit_mse, rf_mse, xgb_mse),
  RMSE_vs_Naive = percent((naive_rmse - c(logit_rmse, rf_rmse, xgb_rmse)) / naive_rmse),
  MSE_vs_Naive = percent((naive_mse - c(logit_mse, rf_mse, xgb_mse)) / naive_mse),
  RMSE_vs_Prop = percent((prop_rmse - c(logit_rmse, rf_rmse, xgb_rmse)) / prop_rmse),
  MSE_vs_Prop = percent((prop_mse - c(logit_mse, rf_mse, xgb_mse)) / prop_mse)
)

print("=== Error Reduction vs Baselines ===")
print(error_reduction)

# Log Loss function
logloss <- function(y, p) {
  # Clip predictions to avoid log(0)
  p <- pmax(pmin(p, 1 - 1e-15), 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

# Compute Log Loss for each model
logit_logloss <- logloss(y_test, logit_pred)
rf_logloss <- logloss(y_test, rf_pred)
xgb_logloss <- logloss(y_test, xgb_pred)

# Combined results table
results <- tibble(
  Model = c("Logistic", "Random Forest", "XGBoost"),
  RMSE = c(logit_rmse, rf_rmse, xgb_rmse),
  MSE = c(logit_mse, rf_mse, xgb_mse),
  LogLoss = c(logit_logloss, rf_logloss, xgb_logloss)
)

print("=== Model Performance (LOGO CV) ===")
print(results %>% arrange(LogLoss))

# Save results
write.csv(error_reduction, "data/results/logo_error_reduction.csv", row.names = FALSE)
write.csv(results, "data/results/logo_model_performance.csv", row.names = FALSE)

#########################
### Model Calibration ###
#########################

# Calibration function
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

# Calibration tables (using LOGO CV predictions)
cal_logit <- calibration_df(logit_pred, y_test)
cal_rf <- calibration_df(rf_pred, y_test)
cal_xgb <- calibration_df(xgb_pred, y_test)

cal_all <- bind_rows(
  cal_logit %>% mutate(model = "Logistic"),
  cal_rf    %>% mutate(model = "Random Forest"),
  cal_xgb   %>% mutate(model = "XGBoost")
)

# Calibration plot function
plot_calibration <- function(cal_df, model_name) {
  ggplot(cal_df, aes(mean_pred, win_rate)) +
    geom_line(linewidth = 1) +
    geom_point(alpha = 0.7) +
    geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", colour = "grey50"
    ) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(
      title = paste("LOGO CV Calibration —", model_name),
      x = "Predicted Win Probability",
      y = "Observed Win Rate",
      size = "N"
    ) +
    theme_minimal()
}

# Generate individual calibration plots
cal_plot_logit <- plot_calibration(cal_logit, "Logistic Regression")
cal_plot_rf    <- plot_calibration(cal_rf, "Random Forest")
cal_plot_xgb   <- plot_calibration(cal_xgb, "XGBoost")

# Save individual plots
ggsave("plots/ml/logo/calibration_logistic_logo.png", cal_plot_logit, width = 6, height = 5, dpi = 300)
ggsave("plots/ml/logo/calibration_rf_logo.png",        cal_plot_rf,    width = 6, height = 5, dpi = 300)
ggsave("plots/ml/logo/calibration_xgboost_logo.png",   cal_plot_xgb,   width = 6, height = 5, dpi = 300)

# Combined calibration plot (all models)
cal_plot_combined <- ggplot(cal_all, aes(mean_pred, win_rate, color = model)) +
  geom_line(linewidth = 1) +
  geom_point(aes(size = n), alpha = 0.6) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", colour = "grey50"
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "LOGO CV — Model Calibration Comparison",
    x = "Predicted Win Probability",
    y = "Observed Win Rate",
    color = "Model",
    size = "N"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("plots/ml/logo/calibration_all_models_logo.png", cal_plot_combined, width = 8, height = 6, dpi = 300)

# Calculate calibration metrics (Brier score)
brier_score <- function(y, p) {
  mean((y - p)^2)
}

brier_results <- tibble(
  Model = c("Logistic", "Random Forest", "XGBoost"),
  Brier_Score = c(
    brier_score(y_test, logit_pred),
    brier_score(y_test, rf_pred),
    brier_score(y_test, xgb_pred)
  )
)

print("=== Brier Scores (Lower is Better) ===")
print(brier_results %>% arrange(Brier_Score))

# Save all results
results_combined <- results %>%
  left_join(brier_results, by = "Model") %>%
  arrange(LogLoss)

write.csv(results_combined, "data/results/logo_model_performance.csv", row.names = FALSE)

cat("\n=== All results saved ===\n")
cat("Performance metrics: data/results/logo_model_performance.csv\n")
cat("Calibration plots: plots/ml/logo/calibration_*_logo.png\n")

########################
### Isotonic Mapping ###
########################

# Function to apply isotonic calibration
apply_isotonic <- function(raw_preds, iso_model) {
  # Sort by x values to ensure increasing order
  ord <- order(iso_model$x)
  x_sorted <- iso_model$x[ord]
  y_sorted <- iso_model$yf[ord]
  
  # Create step function with sorted values
  calib_fn <- stepfun(x_sorted, c(y_sorted[1], y_sorted))
  calibrated <- calib_fn(raw_preds)
  
  # Clip to [0, 1]
  pmin(pmax(calibrated, 0), 1)
}

# Split test set into calibration (50%) and evaluation (50%)
n_test <- length(y_test)
cal_idx <- sample(1:n_test, size = floor(n_test * 0.5))
eval_idx <- setdiff(1:n_test, cal_idx)

# Fit isotonic on calibration subset
iso_logit <- isoreg(logit_pred[cal_idx], y_test[cal_idx])
iso_rf <- isoreg(rf_pred[cal_idx], y_test[cal_idx])
iso_xgb <- isoreg(xgb_pred[cal_idx], y_test[cal_idx])

# Apply to evaluation subset
logit_pred_cal <- apply_isotonic(logit_pred[eval_idx], iso_logit)
rf_pred_cal <- apply_isotonic(rf_pred[eval_idx], iso_rf)
xgb_pred_cal <- apply_isotonic(xgb_pred[eval_idx], iso_xgb)

# Calibration tables on evaluation subset only
cal_logit <- calibration_df(logit_pred[eval_idx], y_test[eval_idx])
cal_rf <- calibration_df(rf_pred[eval_idx], y_test[eval_idx])
cal_xgb <- calibration_df(xgb_pred[eval_idx], y_test[eval_idx])

cal_logit_iso <- calibration_df(logit_pred_cal, y_test[eval_idx])
cal_rf_iso <- calibration_df(rf_pred_cal, y_test[eval_idx])
cal_xgb_iso <- calibration_df(xgb_pred_cal, y_test[eval_idx])

# Now plot
bind_rows(
  cal_xgb %>% mutate(model = "Original"),
  cal_xgb_iso %>% mutate(model = "Isotonic Calibrated")
) %>%
  ggplot(aes(x = mean_pred, y = win_rate, color = model)) +
  geom_point(aes(size = n), alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Mean Predicted Probability",
    y = "Observed Win Rate",
    title = "LOGO CV — XGBoost Calibration: Original vs Isotonic (Eval Set)"
  ) +
  theme_minimal()

# This is wrong currently




######################
### Game example #####
######################

# Identify the example game first
example_game_id <- phase_team %>%
  filter(
    Round == 22,
    Home == "Exeter",
    Away == "Northampton"
  ) %>%
  distinct(game_id) %>%
  pull(game_id)

cat(sprintf("Example game ID: %s\n", example_game_id))

####################################################
### Train Models on All Data EXCEPT Example Game ###
####################################################

cat("Training models on all data except example game...\n")

# Prepare training data (all games EXCEPT the example)
train_data_holdout <- wp_model %>%
  filter(game_id != example_game_id) %>%
  select(-game_id, -Round)

# Get predictor names (excluding Win)
predictors <- setdiff(names(train_data_holdout), "Win")

# 1. RANDOM FOREST (Holdout Training)

rf_fit_holdout <- randomForest(
  Win ~ .,
  data = train_data_holdout,
  ntree = 250,
  mtry = 3,
  nodesize = 25,
  importance = TRUE
)

# 2. LOGISTIC REGRESSION (Holdout Training)

logit_fit_holdout <- glm(
  Win ~ . - Seconds_Remaining,
  data = train_data_holdout,
  family = gaussian()
)

# 3. XGBOOST (Holdout Training)

X_train_holdout <- model.matrix(Win ~ . - 1, data = train_data_holdout)
y_train_holdout <- train_data_holdout$Win

dtrain_holdout <- xgb.DMatrix(data = X_train_holdout, label = y_train_holdout)

# Monotonicity constraints
mono_constraints <- rep(0, ncol(X_train_holdout))
names(mono_constraints) <- colnames(X_train_holdout)
mono_constraints["Points_Diff_Team"] <- 1
mono_constraints["Card_Diff_Team"]   <- 1
mono_constraints["adjusted_diff"]    <- 1
mono_constraints["WinPct_Diff_Team"] <- 1

xgb_fit_holdout <- xgb.train(
  data = dtrain_holdout,
  nrounds = 2000,
  objective = "reg:logistic",
  monotone_constraints = mono_constraints,
  eval_metric = "rmse",
  max_depth = 3,
  eta = 0.1,
  min_child_weight = 20,
  gamma = 5,
)

cat("Models trained successfully (holdout approach)!\n\n")

############################
### Predict Example Game ###
############################

# Extract example game data
example_game <- phase_team %>%
  filter(
    Round == 22,
    Home == "Exeter",
    Away == "Northampton",
    Team == "Home"
  )

# Select and prepare predictors
example_game <- example_game %>%
  select(all_of(predictors)) %>%
  mutate(
    across(where(is.character), as.factor)
  )

# Align factor levels with training data
example_game <- example_game %>%
  mutate(
    across(
      where(is.factor),
      ~ factor(.x, levels = levels(train_data_holdout[[cur_column()]]))
    )
  )

# Random forest predictions

example_game$Win_Prob_RF <- predict(
  rf_fit_holdout,
  newdata = example_game
)

# Logistic regression predictions

example_game$Win_Prob_Logit <- predict(
  logit_fit_holdout,
  newdata = example_game,
  type = "response"
)

# XGBOOST predictions

# Create model matrix for XGBoost
X_example <- model.matrix(
  ~ . - 1,
  data = example_game[, predictors]
)

xgb_matrix_example <- xgb.DMatrix(data = X_example)

example_game$Win_Prob_XGB <- predict(
  xgb_fit_holdout,
  xgb_matrix_example
)

# Preparing data for plotting

# Reshape to long format
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

# Identify scoring events
score_changes <- example_game %>%
  arrange(desc(Seconds_Remaining)) %>%
  mutate(
    score_change = Points_Diff_Team != lag(Points_Diff_Team, default = first(Points_Diff_Team))
  ) %>%
  filter(score_change) %>%
  select(Seconds_Remaining, Points_Diff_Team)

example_plot <- ggplot(
  example_long,
  aes(x = Seconds_Remaining, y = Win_Prob)
) +
  geom_line(linewidth = 1, color = "#2E86AB") +
  geom_vline(
    data = score_changes,
    aes(xintercept = Seconds_Remaining),
    linetype = "dotted",
    colour = "grey40",
    alpha = 0.7
  ) +
  scale_x_reverse(
    breaks = seq(0, 4800, 1200)
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  facet_wrap(~ Model, nrow = 1) +
  labs(
    title = "LOGO CV — Win Probability — Exeter vs Northampton (Round 22)",
    subtitle = "Out-of-sample predictions (models trained excluding this game)",
    x = "Time Remaining",
    y = "Exeter Win Probability"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

ggsave(
  "plots/ml/logo/example_game_plot.png",
  example_plot,
  width = 10,
  height = 4,
  dpi = 300
)

cat("Example game plot saved to plots/ml/logo/example_game_plot.png\n")


#########################################################
### Load LOGO Predictions & Prepare for Partial Plots ###
#########################################################

train_data_full <- wp_model %>%
  select(-game_id, -Round)

predictors <- setdiff(names(train_data_full), "Win")

###################################################
### Train Models on Full Data for Partial Plots ###
###################################################

cat("Training models on full data for partial dependence plots...\n")

# Random Forest
rf_fit <- randomForest(
  Win ~ .,
  data = train_data_full,
  ntree = 250,
  mtry = 3,
  nodesize = 25,
  importance = TRUE
)

# XGBoost
X_train <- model.matrix(Win ~ . - 1, data = train_data_full)
y_train <- train_data_full$Win
dtrain <- xgb.DMatrix(data = X_train, label = y_train)

mono_constraints <- rep(0, ncol(X_train))
names(mono_constraints) <- colnames(X_train)
mono_constraints["Points_Diff_Team"] <- 1
mono_constraints["Card_Diff_Team"]   <- 1
mono_constraints["adjusted_diff"]    <- 1
mono_constraints["WinPct_Diff_Team"] <- 1

xgb_fit <- xgb.train(
  data = dtrain,
  nrounds = 2000,
  objective = "reg:logistic",
  monotone_constraints = mono_constraints,
  eval_metric = "rmse",
  max_depth = 3,
  eta = 0.1,
  min_child_weight = 20,
  gamma = 5
)

cat("Models trained!\n\n")

#######################
### Baseline Values ###
#######################

baseline <- train_data_full %>%
  summarise(
    across(
      all_of(predictors),
      ~ if (is.numeric(.)) median(., na.rm = TRUE) else {
        names(sort(table(.), decreasing = TRUE))[1]
      }
    )
  )

##################################################
### Random Forest Partial Dependence Functions ###
##################################################

partial_rf <- function(var, grid_length = 50) {
  
  grid_vals <- seq(
    min(train_data_full[[var]], na.rm = TRUE),
    max(train_data_full[[var]], na.rm = TRUE),
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
    score_vals  = c(-10, 10),
    grid_length = 50
) {
  
  grid_vals <- seq(
    min(train_data_full[[seconds_var]], na.rm = TRUE),
    max(train_data_full[[seconds_var]], na.rm = TRUE),
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

############################################
### XGBoost Partial Dependence Functions ###
############################################

partial_xgb <- function(var, grid_length = 50) {
  
  grid_vals <- seq(
    min(train_data_full[[var]], na.rm = TRUE),
    max(train_data_full[[var]], na.rm = TRUE),
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
    score_vals  = c(-10, 10),
    grid_length = 50
) {
  
  grid_vals <- seq(
    min(train_data_full[[seconds_var]], na.rm = TRUE),
    max(train_data_full[[seconds_var]], na.rm = TRUE),
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

######################
### Generate Partial Dependence Data ###
######################

cat("Generating partial dependence plots...\n")

# Random Forest
pd_list <- list(
  "Field_Position"     = partial_rf("meter_line_Team"),
  "Points_Differential"= partial_rf("Points_Diff_Team"),
  "Card_Differential"  = partial_rf("Card_Diff_Team"),
  "total_points_both"  = partial_rf("total_points_both"),
  "WinPct_Diff_Team"  = partial_rf("WinPct_Diff_Team")
  
)

pd_seconds <- partial_rf_seconds_by_score()

# XGBoost
pd_list_xgb <- list(
  "Field_Position"      = partial_xgb("meter_line_Team"),
  "Points_Differential" = partial_xgb("Points_Diff_Team"),
  "Card_Differential"   = partial_xgb("Card_Diff_Team"),
  "total_points_both"   = partial_xgb("total_points_both"),
  "WinPct_Diff_Team"  = partial_rf("WinPct_Diff_Team")
)

pd_seconds_xgb <- partial_xgb_seconds_by_score()

###########################################
### Create and Save Random Forest Plots ###
###########################################

for (name in names(pd_list)) {
  
  p <- ggplot(pd_list[[name]], aes(value, win_prob)) +
    geom_line(linewidth = 1, color = "#2E86AB") +
    labs(
      x = name,
      y = "Win Probability",
      title = paste("LOGO CV — Partial Dependence (Random Forest):", name)
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12)
    )
  
  ggsave(
    paste0("plots/ml/logo/partial_rf_", tolower(gsub(" ", "_", name)), ".png"),
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
  geom_line(linewidth = 1.2) +
  scale_x_reverse(
    breaks = seq(0, 4800, 600),
    labels = function(x) paste0(x / 60, "mins")
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Time Remaining",
    y = "Win Probability",
    title = "LOGO CV — Partial Dependence (Random Forest): Time Remaining by Score Differential",
    color = "Score Differential"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("firebrick", "steelblue")
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

ggsave(
  "plots/ml/logo/partial_rf_seconds_remaining_by_score.png",
  p_seconds,
  width = 7,
  height = 5,
  dpi = 300
)

######################
### Create and Save XGBoost Plots ###
######################

for (name in names(pd_list_xgb)) {
  
  p <- ggplot(pd_list_xgb[[name]], aes(value, win_prob)) +
    geom_line(linewidth = 1, color = "#A23B72") +
    labs(
      x = name,
      y = "Win Probability",
      title = paste("LOGO CV — Partial Dependence (XGBoost):", name)
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12)
    )
  
  ggsave(
    paste0("plots/ml/logo/partial_xgb_", tolower(gsub(" ", "_", name)), ".png"),
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
  geom_line(linewidth = 1.2) +
  scale_x_reverse(
    breaks = seq(0, 4800, 600),
    labels = function(x) paste0(x / 60, "mins")
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Time Remaining",
    y = "Win Probability",
    title = "LOGO CV — Partial Dependence (XGBoost): Time Remaining by Score Differential",
    color = "Score Differential"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("firebrick", "steelblue")
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

ggsave(
  "plots/ml/logo/partial_xgb_seconds_remaining_by_score.png",
  p_seconds_xgb,
  width = 7,
  height = 5,
  dpi = 300
)

#################################
### Combined Comparison Plots ###
#################################

# Combine RF and XGBoost for comparison
pd_combined <- bind_rows(
  pd_list$Points_Differential %>% mutate(Model = "Random Forest"),
  pd_list_xgb$Points_Differential %>% mutate(Model = "XGBoost")
)

p_comparison <- ggplot(pd_combined, aes(value, win_prob, color = Model)) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "Points Differential",
    y = "Win Probability",
    title = "LOGO CV — Model Comparison: Points Differential Effect",
    color = "Model"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_color_manual(values = c("Random Forest" = "#2E86AB", "XGBoost" = "#A23B72")) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

ggsave(
  "plots/ml/logo/partial_comparison_points_differential.png",
  p_comparison,
  width = 7,
  height = 5,
  dpi = 300
)

cat("\n=== Partial dependence plots saved to plots/ml/logo/ ===\n")
cat("- Random Forest: partial_rf_*.png\n")
cat("- XGBoost: partial_xgb_*.png\n")
cat("- Comparison: partial_comparison_*.png\n")

