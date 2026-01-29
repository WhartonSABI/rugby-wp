#############
### SETUP ###
#############

library(dplyr)
library(ggplot2)

source("code/helpers/data-prep.R")

######################
### LOAD AND SHAPE ###
######################

# Build enriched phase data with scoring events and game metadata.
phase_data <- prepare_phase_data()

# Create a stable match identifier for grouping.
phase_data <- phase_data %>%
  mutate(match_id = paste(Round, Home, Away, sep = "|"))

# Compute total match length from the final phase in each game.
match_seconds <- phase_data %>%
  group_by(match_id) %>%
  summarise(max_seconds = max(Seconds_Remaining, na.rm = TRUE), .groups = "drop")

# Convert remaining time into elapsed minutes for plotting.
phase_data <- phase_data %>%
  left_join(match_seconds, by = "match_id") %>%
  mutate(
    seconds_elapsed = max_seconds - Seconds_Remaining,
    minute = floor(seconds_elapsed / 60)
  )

# Average scoring per minute across all matches.
match_count <- n_distinct(phase_data$match_id)

scoring_by_minute <- phase_data %>%
  group_by(minute) %>%
  summarise(points = sum(points_change, na.rm = TRUE), .groups = "drop") %>%
  mutate(points_per_minute = points / match_count)

################
### PLOTTING ###
################

# Output path for the time series plot.
plot_path <- "plots/eda/scoring_rate.png"

scoring_plot <- ggplot(scoring_by_minute, aes(x = minute, y = points_per_minute)) +
  geom_line(color = "#2B6CB0", linewidth = 0.9) +
  labs(
    title = "Average Scoring Rate Over Time",
    x = "Minutes Elapsed",
    y = "Points per Minute (per Match)"
  ) +
  theme_minimal(base_size = 12)

# Save the plot at publication resolution.
ggsave(
  filename = plot_path,
  plot = scoring_plot,
  width = 8,
  height = 4.5,
  dpi = 300
)

message("Saved plot to ", plot_path)
