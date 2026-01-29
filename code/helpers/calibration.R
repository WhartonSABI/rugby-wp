#############
### SETUP ###
#############

# Shared calibration helpers: prepare data, plot calibration curves, print error reduction.

library(ggplot2)
library(rlang)

########################
### CALIBRATION DATA ###
########################

prepare_calibration_data <- function(phase_data, n_samples = 1000) {
  # Remove terminal states and sample a reproducible subset of phases.
  phase_data <- phase_data[phase_data$state_result != "End_of_Game", ]
  set.seed(123)
  sample_indices <- sample(seq_len(nrow(phase_data)), min(n_samples, nrow(phase_data)))
  sampled_states <- phase_data[sample_indices, ]
  # Reconstruct home/away scores and possession from phase-level features.
  sampled_states$home_score <- ifelse(sampled_states$Team_In_Poss == "Home",
    (sampled_states$total_points_both + sampled_states$Points_Difference) / 2,
    (sampled_states$total_points_both - sampled_states$Points_Difference) / 2)
  sampled_states$away_score <- sampled_states$total_points_both - sampled_states$home_score
  sampled_states$team_poss <- ifelse(sampled_states$Team_In_Poss == "Home", "Home", "Away")
  sampled_states$poss_team_won <- sampled_states$Win
  sampled_states
}

#############
### PLOTS ###
#############

plot_calibration <- function(calibration_results, plot_dir, title_calibration, title_distribution,
                             subtitle_calibration, subtitle_distribution,
                             calibration_filename = "calibration_curve.png",
                             distribution_filename = "prediction_distribution.png") {
  # Bin predictions into deciles and summarize average predicted vs actual.
  calibration_results$pred_bin_num <- cut(calibration_results$predicted_win_prob,
    breaks = seq(0, 1, 0.1), include.lowest = TRUE)
  bin_summary <- aggregate(cbind(actual_win, predicted_win_prob) ~ pred_bin_num,
    data = calibration_results, FUN = function(x) c(mean = mean(x), n = length(x)))
  bin_summary$pred_mean <- bin_summary$predicted_win_prob[, 1]
  bin_summary$actual_mean <- bin_summary$actual_win[, 1]
  bin_summary$n <- bin_summary$actual_win[, 2]
  # Calibration curve: mean prediction vs observed outcome by bin.
  p1 <- ggplot(bin_summary, aes(x = .data$pred_mean, y = .data$actual_mean)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = title_calibration, subtitle = subtitle_calibration,
         x = "Predicted Win Probability", y = "Actual Win Rate", size = "N") +
    xlim(0, 1) + ylim(0, 1) + theme_minimal()
  # Prediction distribution split by outcome.
  p2 <- ggplot(calibration_results, aes(x = .data$predicted_win_prob, fill = factor(.data$actual_win))) +
    geom_histogram(bins = 20, alpha = 0.6, position = "identity") +
    labs(title = title_distribution, subtitle = subtitle_distribution,
         x = "Predicted Win Probability", y = "Count", fill = "Actual Win") +
    theme_minimal()
  # Persist plots and return for reuse.
  ggsave(file.path(plot_dir, calibration_filename), p1, width = 6, height = 5, dpi = 300)
  ggsave(file.path(plot_dir, distribution_filename), p2, width = 7, height = 5, dpi = 300)
  invisible(list(calibration = p1, distribution = p2))
}

###############
### METRICS ###
###############

rmse <- function(y, p) sqrt(mean((y - p)^2))
mse <- function(y, p) mean((y - p)^2)

print_error_reduction <- function(calibration_results, model_name) {
  # Compare model errors to naive and prevalence baselines.
  y_actual <- calibration_results$actual_win
  y_pred <- calibration_results$predicted_win_prob
  state_space_rmse <- rmse(y_actual, y_pred)
  state_space_mse <- mse(y_actual, y_pred)
  naive_rmse <- sqrt(mean((y_actual - 0.5)^2))
  prop_rmse <- sqrt(mean((y_actual - mean(y_actual))^2))
  naive_mse <- mean((y_actual - 0.5)^2)
  prop_mse <- mean((y_actual - mean(y_actual))^2)
  error_reduction <- data.frame(
    Model = model_name,
    RMSE = state_space_rmse, MSE = state_space_mse,
    RMSE_vs_Naive = paste0(round(100 * (naive_rmse - state_space_rmse) / naive_rmse, 2), "%"),
    MSE_vs_Naive = paste0(round(100 * (naive_mse - state_space_mse) / naive_mse, 2), "%"),
    RMSE_vs_Prop = paste0(round(100 * (prop_rmse - state_space_rmse) / prop_rmse, 2), "%"),
    MSE_vs_Prop = paste0(round(100 * (prop_mse - state_space_mse) / prop_mse, 2), "%"))
  cat("\n=== Error Reduction vs Baselines ===\n")
  print(error_reduction)
  cat("\n=== Baseline Performance ===\n")
  cat("Naive (0.5) RMSE:", naive_rmse, "\n")
  cat("Naive (0.5) MSE:", naive_mse, "\n")
  cat("Proportion RMSE:", prop_rmse, "\n")
  cat("Proportion MSE:", prop_mse, "\n")
  invisible(calibration_results)
}
