# Empirical joint transition kernel. Lookup uses coarse score buckets;
# simulation and absorption calculations retain the exact integer margin.
TIME_STATES <- c('1h_start', '1h_middle', '1h_end', '2h_start', '2h_middle', '2h_end')
FIELD_ZONES <- c(2.5, 13.5, 31, 45, 55, 69, 86.5, 97.5)
SCORE_BUCKETS <- c('down_14p', 'down_7_13', 'down_1_6', 'tied', 'up_1_6', 'up_7_13', 'up_14p')

score_bucket_from_diff <- function(x) {
  as.character(cut(x, c(-Inf, -14, -7, -1, 0, 6, 13, Inf), labels = SCORE_BUCKETS))
}

build_time_state <- function(seconds_remaining) {
  half <- ifelse(seconds_remaining > 2400, '1h', '2h')
  remain <- ifelse(half == '1h', seconds_remaining - 2400, seconds_remaining)
  part <- ifelse(remain > 2280, 'start', ifelse(remain <= 120, 'end', 'middle'))
  factor(paste(half, part, sep = '_'), levels = TIME_STATES)
}

build_state <- function(time_state, meter_line, team_poss, score_home, score_away) {
  margin <- ifelse(team_poss == 'Home', score_home - score_away, score_away - score_home)
  paste(time_state, meter_line, score_bucket_from_diff(margin), sep = '_')
}

prepare_transition_data <- function(phase_data) {
  # Construct transitions on all phases, before selecting evaluation rows.
  d <- phase_data[order(phase_data$ID), ]
  d$game_id <- paste(d$Home, d$Away, d$Round, sep = '_')
  d$time_state <- build_time_state(d$Seconds_Remaining)
  d$score_diff_poss <- d$Points_Difference
  d$score_bucket <- score_bucket_from_diff(d$score_diff_poss)
  d$state <- paste(d$time_state, d$meter_line, d$score_bucket, sep = '_')
  n <- nrow(d)
  same <- c(d$game_id[-1] == d$game_id[-n], FALSE)
  home_margin <- ifelse(d$Team_In_Poss == 'Home', 1, -1) * d$Points_Difference
  next_margin <- c(home_margin[-1], NA_real_)
  direction <- ifelse(d$Team_In_Poss == 'Home', 1, -1)
  # On the last recorded phase, the signed next-score label supplies any
  # final score before full time. Otherwise use the observed score change.
  d$reward <- ifelse(same, (next_margin - home_margin) * direction, d$points)
  d$terminal <- !same
  d$next_time <- c(as.integer(d$time_state)[-1], NA_integer_)
  d$next_field <- c(d$meter_line[-1], NA_real_)
  d$flip <- c(d$Team_In_Poss[-1] != d$Team_In_Poss[-n], FALSE)
  d$half_end <- d$terminal | (!d$terminal & as.integer(d$time_state) <= 3 & d$next_time > 3)
  d$next_time[d$terminal] <- 0L
  d$next_field[d$terminal] <- 0
  d$flip[d$terminal] <- FALSE
  if (anyNA(d[c('state', 'reward', 'next_time', 'next_field', 'flip')]) ||
      any(!d$reward %in% c(-7, -5, -3, 0, 3, 5, 7))) stop('Invalid transition data')
  if (any(!d$terminal & d$next_time < as.integer(d$time_state))) stop('Time bucket moves backwards')
  d
}

build_state_space <- function(phase_data, save_transition = FALSE,
                             transition_path = 'data/results/transition_probs_v2.rds') {
  d <- prepare_transition_data(phase_data)
  event_columns <- c('reward', 'next_time', 'next_field', 'flip', 'terminal', 'half_end')
  count_events <- function(rows) {
    out <- aggregate(rep(1L, nrow(rows)), rows[event_columns], sum)
    names(out)[ncol(out)] <- 'n'
    out$probability <- out$n / sum(out$n)
    out
  }
  exact <- lapply(split(d, d$state), count_events)
  # Training-only backoff for unobserved score buckets, then locations.
  # Observed rows retain empirical frequencies; no automatic smoothing.
  by_field <- lapply(split(d, paste(d$time_state, d$meter_line, sep = '_')), count_events)
  by_time <- lapply(split(d, as.character(d$time_state)), count_events)
  grid <- expand.grid(time = TIME_STATES, field = FIELD_ZONES, bucket = SCORE_BUCKETS,
                      stringsAsFactors = FALSE)
  keys <- paste(grid$time, grid$field, grid$bucket, sep = '_')
  kernels <- setNames(vector('list', length(keys)), keys)
  support <- data.frame(state = keys, n = 0L, source = 'exact')
  for (i in seq_along(keys)) {
    key <- keys[i]
    events <- exact[[key]]
    if (is.null(events)) {
      events <- by_field[[paste(grid$time[i], grid$field[i], sep = '_')]]
      support$source[i] <- 'time_field'
    }
    if (is.null(events)) {
      events <- by_time[[grid$time[i]]]
      support$source[i] <- 'time'
    }
    if (is.null(events)) stop('No training support for time bucket ', grid$time[i])
    kernels[[key]] <- events
    support$n[i] <- if (key %in% names(exact)) sum(exact[[key]]$n) else 0L
  }
  model <- list(version = 2L, phase_data = d, kernels = kernels, support = support,
                time_state_levels = TIME_STATES)
  if (save_transition) {
    dir.create(dirname(transition_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(model, transition_path)
  }
  model
}

get_kernel <- function(model, time, field, margin_poss) {
  if (!identical(model$version, 2L)) stop('Rebuild legacy transition cache with version 2')
  key <- paste(time, field, score_bucket_from_diff(margin_poss), sep = '_')
  out <- model$kernels[[key]]
  if (is.null(out)) stop('Unsupported state: ', key)
  out
}
