#' Process NSGA-II Results
#'
#' @keywords internal
process_results <- function(ga_result, data, prep, treatment_var) {

  # Keep only the non-dominated solutions. mco::nsga2() returns the whole
  # final population together with a logical flag marking the Pareto-optimal
  # members; without this filter the reported front would contain dominated
  # points, and both plot() and the Shiny app would display them as if they
  # were attainable optima.
  keep <- ga_result$pareto.optimal
  if (is.null(keep) || !any(keep)) keep <- rep(TRUE, nrow(ga_result$value))
  ga_result$value <- ga_result$value[keep, , drop = FALSE]
  ga_result$par   <- ga_result$par[keep, , drop = FALSE]

  # Sort by imbalance and drop duplicates in objective space. NSGA-II returns
  # one entry per population member, and distinct selection vectors frequently
  # map to the same pair of objective values; without this step the frontier
  # is reported several times larger than the number of trade-offs it offers.
  # This matches how the analysis scripts count frontier solutions, so the
  # package and the published figures report the same size.
  o <- order(ga_result$value[, 1])
  ga_result$value <- ga_result$value[o, , drop = FALSE]
  ga_result$par   <- ga_result$par[o, , drop = FALSE]
  dup <- duplicated(round(ga_result$value, 10))
  ga_result$value <- ga_result$value[!dup, , drop = FALSE]
  ga_result$par   <- ga_result$par[!dup, , drop = FALSE]

  # Extract objectives
  vals <- ga_result$value
  vals[, 2] <- -vals[, 2]  # Convert coverage back to positive

  # Normalize for knee point calculation
  norm_vals <- apply(vals, 2, function(x) {
    r <- max(x) - min(x)
    if (r == 0) return(rep(0, length(x)))
    (x - min(x)) / r
  })

  # Find extreme points
  p1 <- norm_vals[which.min(norm_vals[, 1]), ]
  p2 <- norm_vals[which.max(norm_vals[, 2]), ]
  line_vec <- p2 - p1
  line_len <- sqrt(sum(line_vec^2))

  # Calculate distances to line
  distances <- apply(norm_vals, 1, function(p) {
    v <- p - p1
    cross <- abs(v[1] * line_vec[2] - v[2] * line_vec[1])
    cross / line_len
  })

  # Best solutions
  knee_idx <- which.max(distances)
  min_bal_idx <- which.min(ga_result$value[, 1])
  max_cov_idx <- which.max(vals[, 2])

  # Extract selected data
  sel_minbal <- ga_result$par[min_bal_idx, ] > 0.5
  sel_knee <- ga_result$par[knee_idx, ] > 0.5
  sel_maxcov <- ga_result$par[max_cov_idx, ] > 0.5

  # Pareto front data frame
  pareto_front <- data.frame(
    Solution = seq_len(nrow(vals)),
    Balance = vals[, 1],
    Coverage = vals[, 2],
    N_Selected = rowSums(ga_result$par > 0.5),
    stringsAsFactors = FALSE
  )

  # Return results
  list(
    pareto_front = pareto_front,
    best_solutions = list(
      min_balance = min_bal_idx,
      knee_point = knee_idx,
      max_coverage = max_cov_idx
    ),
    selected_data = list(
      min_balance = data[sel_minbal, ],
      knee_point = data[sel_knee, ],
      max_coverage = data[sel_maxcov, ]
    )
  )
}
