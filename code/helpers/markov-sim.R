# Optional trajectories from the same joint kernel used by the direct solver.
# Never turn a numerical cutoff into a completed match.
simulate_game_play_by_play <- function(time_state, meter_line, score_home, score_away,
                                       team_poss, model, max_plays = 10000L) {
  stopifnot(time_state %in% TIME_STATES, meter_line %in% FIELD_ZONES,
            team_poss %in% c('Home', 'Away'), score_home >= 0, score_away >= 0)
  log <- vector('list', max_plays)
  for (i in seq_len(max_plays)) {
    before <- build_state(time_state, meter_line, team_poss, score_home, score_away)
    direction <- if (team_poss == 'Home') 1 else -1
    events <- get_kernel(model, time_state, meter_line, direction * (score_home - score_away))
    e <- events[sample.int(nrow(events), 1L, prob = events$probability), ]
    scoring_team <- if (e$reward == 0) NA_character_ else
      if (direction * e$reward > 0) 'Home' else 'Away'
    if (!is.na(scoring_team) && scoring_team == 'Home') score_home <- score_home + abs(e$reward)
    if (!is.na(scoring_team) && scoring_team == 'Away') score_away <- score_away + abs(e$reward)
    if (!e$terminal) {
      if (e$flip) team_poss <- if (team_poss == 'Home') 'Away' else 'Home'
      time_state <- TIME_STATES[e$next_time]
      meter_line <- e$next_field
    }
    log[[i]] <- data.frame(play = i, state_before = before, reward = e$reward,
      scoring_team = scoring_team, team = team_poss, home_score = score_home,
      away_score = score_away, terminal = e$terminal, half_end = e$half_end,
      state_after = if (e$terminal) NA_character_ else
        build_state(time_state, meter_line, team_poss, score_home, score_away))
    if (e$terminal) return(do.call(rbind, log[seq_len(i)]))
  }
  stop('Simulation failed to reach full time within max_plays; no result counted')
}

simulate_game <- function(time_state, meter_line, score_home, score_away, team_poss,
                          model, n_sims = 1000L, max_plays = 10000L) {
  margins <- replicate(n_sims, {
    x <- simulate_game_play_by_play(time_state, meter_line, score_home, score_away,
                                   team_poss, model, max_plays)
    tail(x$home_score - x$away_score, 1)
  })
  data.frame(home_win_prob = mean(margins > 0), tie_prob = mean(margins == 0),
             away_win_prob = mean(margins < 0),
             home_expected_result = mean((sign(margins) + 1) / 2))
}
