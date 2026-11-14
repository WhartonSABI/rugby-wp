# Solve (I-Q) B = R. Margin-grid exits are a separate unresolved outcome;
# they are never silently counted as wins or losses. Increase the grid until
# the unresolved mass is negligible for the states being reported.
compile_absorbing_chain <- function(model, margin_limit = 100L) {
  grid <- expand.grid(time = seq_along(TIME_STATES), field = FIELD_ZONES,
                      margin = seq.int(-margin_limit, margin_limit), poss = c(1L, -1L))
  grid$key <- paste(TIME_STATES[grid$time], grid$field,
                    score_bucket_from_diff(grid$margin * grid$poss), sep = '_')
  index <- function(time, field, margin, poss) {
    time + 6L * (match(field, FIELD_ZONES) - 1L) + 48L * (margin + margin_limit) +
      48L * (2L * margin_limit + 1L) * as.integer(poss == -1L)
  }
  groups <- split(seq_len(nrow(grid)), grid$key)
  edges <- lapply(names(groups), function(key) {
    rows <- groups[[key]]
    e <- model$kernels[[key]]
    i <- rep(rows, each = nrow(e))
    j <- rep(seq_len(nrow(e)), times = length(rows))
    new_margin <- grid$margin[i] + grid$poss[i] * e$reward[j]
    terminal <- e$terminal[j]
    overflow <- !terminal & abs(new_margin) > margin_limit
    next_poss <- grid$poss[i] * ifelse(e$flip[j], -1L, 1L)
    next_id <- index(e$next_time[j], e$next_field[j], new_margin, next_poss)
    data.frame(i = i, j = next_id, p = e$probability[j], terminal = terminal,
               overflow = overflow, margin = new_margin)
  })
  edges <- do.call(rbind, edges)
  n <- nrow(grid)
  live <- !edges$terminal & !edges$overflow
  Q <- Matrix::sparseMatrix(i = edges$i[live], j = edges$j[live], x = edges$p[live], dims = c(n, n))
  end <- edges$terminal | edges$overflow
  category <- ifelse(edges$overflow[end], 4L,
                     ifelse(edges$margin[end] > 0, 1L, ifelse(edges$margin[end] == 0, 2L, 3L)))
  R <- Matrix::sparseMatrix(i = edges$i[end], j = category, x = edges$p[end], dims = c(n, 4L))
  stopifnot(max(abs(Matrix::rowSums(Q) + Matrix::rowSums(R) - 1)) < 1e-12)
  A <- Matrix::Diagonal(n) - Q
  B <- as.matrix(Matrix::solve(A, as.matrix(R)))
  colnames(B) <- c('home_win', 'draw', 'away_win', 'unresolved')
  residual <- max(abs(as.matrix(A %*% B) - as.matrix(R)))
  if (any(!is.finite(B)) || min(B) < -1e-8 || max(abs(rowSums(B) - 1)) > 1e-7 || residual > 1e-8)
    stop('Absorption calculation failed; inspect support and recurrent classes')
  # Floating-point solves can return 1 + 1e-15; remove roundoff only, without
  # renormalizing away the explicitly tracked unresolved probability mass.
  B <- pmin(pmax(B, 0), 1)
  list(grid = grid, A = A, Q = Q, R = R, probabilities = B,
       edges = edges, margin_limit = margin_limit, residual = residual, index = index)
}

predict_absorption <- function(chain, time_state, meter_line, margin_home, team_poss,
                               tolerance = 1e-7) {
  if (any(abs(margin_home) > chain$margin_limit) || any(margin_home != round(margin_home)))
    stop('Starting margin outside the integer grid')
  i <- chain$index(match(time_state, TIME_STATES), meter_line, margin_home,
                   ifelse(team_poss == 'Home', 1L, -1L))
  if (anyNA(i)) stop('Invalid query state')
  p <- as.data.frame(chain$probabilities[i, , drop = FALSE])
  if (any(p$unresolved > tolerance)) stop('Margin grid too small: enlarge before reporting predictions')
  p$expected_result_home <- p$home_win + 0.5 * p$draw
  p$expected_result_poss <- ifelse(team_poss == 'Home', p$home_win, p$away_win) + 0.5 * p$draw
  p
}

final_margin_distribution <- function(chain, time_state, meter_line, margin_home, team_poss) {
  i <- chain$index(match(time_state, TIME_STATES), meter_line, margin_home,
                   ifelse(team_poss == 'Home', 1L, -1L))
  start <- numeric(nrow(chain$grid)); start[i] <- 1
  visits <- as.numeric(Matrix::solve(Matrix::t(chain$A), start))
  e <- chain$edges[chain$edges$terminal, ]
  out <- aggregate(visits[e$i] * e$p, list(margin = e$margin), sum)
  names(out)[2] <- 'probability'
  attr(out, 'unresolved') <- chain$probabilities[i, 'unresolved']
  out
}

next_score_probabilities <- function(model, margin_home = 0, horizon = c('half', 'match')) {
  horizon <- match.arg(horizon)
  grid <- expand.grid(time = seq_along(TIME_STATES), field = FIELD_ZONES, poss = c(1L, -1L))
  n <- nrow(grid); Q <- matrix(0, n, n); values <- c(-7, -5, -3, 0, 3, 5, 7)
  R <- matrix(0, n, length(values))
  index <- function(time, field, poss) time + 6L * (match(field, FIELD_ZONES) - 1L) + 48L * (poss == -1L)
  for (i in seq_len(n)) {
    e <- get_kernel(model, TIME_STATES[grid$time[i]], grid$field[i], grid$poss[i] * margin_home)
    for (j in seq_len(nrow(e))) {
      # Scoring at the boundary occurs before the half/match ends.
      reward_home <- grid$poss[i] * e$reward[j]
      if (reward_home != 0 || e$terminal[j] || (horizon == 'half' && e$half_end[j])) {
        k <- match(reward_home, values)
        R[i, k] <- R[i, k] + e$probability[j]
      } else {
        k <- index(e$next_time[j], e$next_field[j], grid$poss[i] * ifelse(e$flip[j], -1L, 1L))
        Q[i, k] <- Q[i, k] + e$probability[j]
      }
    }
  }
  B <- solve(diag(n) - Q, R)
  stopifnot(min(B) > -1e-9, max(abs(rowSums(B) - 1)) < 1e-8)
  colnames(B) <- paste0('score_', values)
  cbind(grid, B, ep_home = as.numeric(B %*% values), horizon = horizon)
}
