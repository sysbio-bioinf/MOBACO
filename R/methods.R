#' Summary Method for MOBACO
#'
#' @param object A mobaco object
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.mobaco <- function(object, ...) {
  cat("MOBACO Results\n")
  cat("==============\n\n")

  cat("Balance Metric: BOSS-B (Nikolaev et al. 2013)\n")
  cat(sprintf("Treatment Variable: %s\n", object$treatment_var))
  cat(sprintf("Treatment Level: %s\n", object$treatment_level))
  cat("\n")

  pf <- object$pareto_front
  bs <- object$best_solutions

  cat("Pareto Front Solutions:\n")
  cat(sprintf("  Total solutions: %d\n", nrow(pf)))
  cat("\n")

  cat("Best Solutions:\n")
  cat(sprintf("  Min Balance:  Balance = %.4f | Coverage = %.4f | N = %d\n",
              pf$Balance[bs$min_balance],
              pf$Coverage[bs$min_balance],
              pf$N_Selected[bs$min_balance]))
  cat(sprintf("  Knee Point:   Balance = %.4f | Coverage = %.4f | N = %d\n",
              pf$Balance[bs$knee_point],
              pf$Coverage[bs$knee_point],
              pf$N_Selected[bs$knee_point]))
  cat(sprintf("  Max Coverage: Balance = %.4f | Coverage = %.4f | N = %d\n",
              pf$Balance[bs$max_coverage],
              pf$Coverage[bs$max_coverage],
              pf$N_Selected[bs$max_coverage]))

  invisible(object)
}


#' Print Method for MOBACO
#'
#' @param x A mobaco object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.mobaco <- function(x, ...) {
  cat("MOBACO object\n")
  cat("  Balance metric: BOSS-B\n")
  cat(sprintf("  Pareto solutions: %d\n", nrow(x$pareto_front)))
  cat("  Use summary() for details\n")
  invisible(x)
}

#' Plot Pareto Front with optional PSM comparison
#'
#' @param x A mobaco object
#' @param interactive Logical. Use plotly for interactive plot? (default: TRUE)
#' @param show_psm Logical. Show PSM point if available? (default: TRUE)
#' @param ... Additional arguments (ignored)
#'
#' @importFrom graphics legend points
#' @export
plot.mobaco <- function(x, interactive = TRUE, show_psm = TRUE, ...) {

  pf <- x$pareto_front
  bs <- x$best_solutions

  # Calculate PSM balance and coverage if available
  psm_point <- NULL
  if (show_psm && !is.null(x$psm_data)) {
    psm_point <- calculate_psm_point(x)
  }

  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    # Prepare data for plotting
    # Separate Pareto solutions from highlighted points
    regular_idx <- setdiff(1:nrow(pf), c(bs$min_balance, bs$knee_point, bs$max_coverage))

    pf_regular <- pf[regular_idx, ]
    pf_regular$hover <- paste0(
      "<b>Pareto solution</b><br>",
      "Solution #", pf_regular$Solution, "<br>",
      "Balance (BOSS-B): ", sprintf("%.6f", pf_regular$Balance), "<br>",
      "Coverage: ", sprintf("%.4f", pf_regular$Coverage), "<br>",
      "Patients: ", pf_regular$N_Selected
    )

    # Create base plot with regular Pareto solutions only
    p <- plotly::plot_ly()

    # Add regular Pareto solutions
    p <- p %>%
      plotly::add_markers(
        data = pf_regular,
        x = ~Balance,
        y = ~Coverage,
        marker = list(size = 8, color = "steelblue", opacity = 0.7),
        name = "Pareto solutions",
        text = ~hover,
        hoverinfo = "text"
      )

    # Add Min Balance
    p <- p %>%
      plotly::add_markers(
        x = pf$Balance[bs$min_balance],
        y = pf$Coverage[bs$min_balance],
        marker = list(size = 14, color = "darkgreen"),
        name = "Min Balance",
        text = paste0(
          "<b>Min Balance</b><br>",
          "Balance (BOSS-B): ", sprintf("%.6f", pf$Balance[bs$min_balance]), "<br>",
          "Coverage: ", sprintf("%.4f", pf$Coverage[bs$min_balance]), "<br>",
          "Patients: ", pf$N_Selected[bs$min_balance]
        ),
        hoverinfo = "text"
      )

    # Add Knee Point
    p <- p %>%
      plotly::add_markers(
        x = pf$Balance[bs$knee_point],
        y = pf$Coverage[bs$knee_point],
        marker = list(size = 14, color = "red", symbol = "triangle-up"),
        name = "Knee Point",
        text = paste0(
          "<b>Knee Point</b><br>",
          "Balance (BOSS-B): ", sprintf("%.6f", pf$Balance[bs$knee_point]), "<br>",
          "Coverage: ", sprintf("%.4f", pf$Coverage[bs$knee_point]), "<br>",
          "Patients: ", pf$N_Selected[bs$knee_point]
        ),
        hoverinfo = "text"
      )

    # Add Max Coverage
    p <- p %>%
      plotly::add_markers(
        x = pf$Balance[bs$max_coverage],
        y = pf$Coverage[bs$max_coverage],
        marker = list(size = 14, color = "orange"),
        name = "Max Coverage",
        text = paste0(
          "<b>Max Coverage</b><br>",
          "Balance (BOSS-B): ", sprintf("%.6f", pf$Balance[bs$max_coverage]), "<br>",
          "Coverage: ", sprintf("%.4f", pf$Coverage[bs$max_coverage]), "<br>",
          "Patients: ", pf$N_Selected[bs$max_coverage]
        ),
        hoverinfo = "text"
      )

    # Add PSM point if available
    if (!is.null(psm_point)) {
      psm_hover <- paste0(
        "<b>PSM (MatchIt)</b><br>",
        "Balance (BOSS-B): ", sprintf("%.6f", psm_point$balance), "<br>",
        "Coverage: ", sprintf("%.4f", psm_point$coverage), "<br>",
        "Patients: ", psm_point$n, "<br>",
        "<i>", psm_point$status, "</i>"
      )

      p <- p %>%
        plotly::add_markers(
          x = psm_point$balance,
          y = psm_point$coverage,
          marker = list(
            size = 16,
            color = psm_point$color,
            symbol = "diamond",
            line = list(color = "black", width = 2)
          ),
          name = "PSM",
          text = psm_hover,
          hoverinfo = "text"
        )
    }

    # Layout
    subtitle_text <- sprintf(
      "%d solutions | Balance: %.4f - %.4f | Coverage: %.4f - %.4f",
      nrow(pf),
      min(pf$Balance),
      max(pf$Balance),
      min(pf$Coverage),
      max(pf$Coverage)
    )

    if (!is.null(psm_point)) {
      subtitle_text <- paste0(
        subtitle_text,
        sprintf("<br><i>PSM: %s</i>", psm_point$status)
      )
    }

    p <- p %>%
      plotly::layout(
        title = list(
          text = paste0(
            "Pareto Front -- MOBACO (BOSS-B)<br>",
            "<sup>", subtitle_text, "</sup>"
          )
        ),
        xaxis = list(title = "Balance Error (BOSS-B, lower = better)"),
        yaxis = list(title = "Coverage (higher = better)"),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.2,
          xanchor = "left",
          yanchor = "top"
        )
      )

    return(p)

  } else {
    # Base R plot
    plot(
      pf$Balance, pf$Coverage,
      pch = 19, col = "steelblue",
      xlab = "Balance Error (BOSS-B, lower = better)",
      ylab = "Coverage (higher = better)",
      main = "Pareto Front -- MOBACO (BOSS-B)"
    )

    points(pf$Balance[bs$min_balance], pf$Coverage[bs$min_balance],
           pch = 19, col = "darkgreen", cex = 2)
    points(pf$Balance[bs$knee_point], pf$Coverage[bs$knee_point],
           pch = 17, col = "red", cex = 2)
    points(pf$Balance[bs$max_coverage], pf$Coverage[bs$max_coverage],
           pch = 19, col = "orange", cex = 2)

    if (!is.null(psm_point)) {
      points(psm_point$balance, psm_point$coverage,
             pch = 18, col = psm_point$color, cex = 2.5)
    }

    legend_items <- c("Pareto solutions", "Min Balance", "Knee Point", "Max Coverage")
    legend_cols <- c("steelblue", "darkgreen", "red", "orange")
    legend_pch <- c(19, 19, 17, 19)

    if (!is.null(psm_point)) {
      legend_items <- c(legend_items, paste("PSM -", psm_point$status))
      legend_cols <- c(legend_cols, psm_point$color)
      legend_pch <- c(legend_pch, 18)
    }

    legend("topright",
           legend = legend_items,
           col = legend_cols,
           pch = legend_pch)

    invisible(x)
  }
}

#' Extract Selected Solution Data
#'
#' @param object A mobaco object
#' @param solution Character. Which solution:
#'   "min_balance" (default), "knee", or "max_coverage"
#'
#' @return A data frame with selected patients
#'
#' @export
extract_solution <- function(object, solution = "min_balance") {

  solution <- match.arg(solution, c("min_balance", "knee", "max_coverage"))

  if (solution == "knee") {
    solution <- "knee_point"
  }

  object$selected_data[[solution]]
}

#' Calculate PSM point for Pareto plot
#' @keywords internal
calculate_psm_point <- function(mobaco_result) {

  if (is.null(mobaco_result$psm_data)) {
    return(NULL)
  }

  psm_data <- mobaco_result$psm_data
  prep <- mobaco_result$prep

  # Bin the matched units with the structure used for the optimization
  psm_bins <- apply_bins_to_data(psm_data, mobaco_result$bins,
                                 mobaco_result$covariates)

  treatment_var <- mobaco_result$treatment_var
  treatment_level <- mobaco_result$treatment_level

  T_ind <- psm_data[[treatment_var]] == treatment_level
  C_ind <- !T_ind

  # Balance and coverage come from the same function that drove the
  # optimization, so the comparison point cannot drift away from the
  # frontier. Every matched unit is selected (ind = 1), and the group
  # totals are those of the FULL cohort — that makes coverage the
  # symmetric mean of Equation (8), not a plain retained share.
  norm <- mobaco_result$normalization
  if (is.null(norm)) norm <- "asymmetric"   # results from older versions

  fit <- calc_boss_fitness_cpp(
    ind           = rep(1, nrow(psm_data)),
    bin_mat       = psm_bins$bin_mat,
    k_vec         = psm_bins$k_vec,
    T_int         = as.integer(T_ind),
    C_int         = as.integer(C_ind),
    nT_total      = prep$n_treat,
    nC_total      = prep$n_control,
    min_per_group = 0L,
    normalization = unname(NORMALIZATIONS[norm])
  )

  psm_balance  <-  fit[1]
  psm_coverage <- -fit[2]

  # Pareto domination in the usual sense: at least as good in both
  # objectives and strictly better in at least one.
  pf <- mobaco_result$pareto_front
  dominated <- any(
    pf$Balance <= psm_balance & pf$Coverage >= psm_coverage &
      (pf$Balance < psm_balance | pf$Coverage > psm_coverage)
  )

  # PSM is on Pareto front if no solution dominates it
  on_pareto <- !dominated

  # Determine status and color
  if (on_pareto) {
    status <- "On Pareto Front"
    color <- "purple"
  } else {
    status <- "Dominated by MOBACO"
    color <- "gray"
  }

  list(
    balance = psm_balance,
    coverage = psm_coverage,
    n = nrow(psm_data),
    status = status,
    color = color,
    on_pareto = on_pareto
  )
}


#' Apply bins to new data using the same structure as the original binning
#'
#' Re-uses the bin structure from \code{prepare_data()}. Without this, PSM
#' and MOBACO balance values would not be comparable.
#'
#' @keywords internal
apply_bins_to_data <- function(data, bins_info, covariates) {

  # bins_info$binned contains the factor-encoded reference from prepare_data()
  # We apply the same factor levels / cut breaks to the new (PSM) data.
  ref_binned <- bins_info$binned   # data.frame with factor columns
  all_vars   <- bins_info$all_vars # variable order used in bin_mat

  # Subset to covariates that are in all_vars (same order)
  vars_to_use <- intersect(all_vars, covariates)

  n_feat  <- length(vars_to_use)
  bin_mat <- matrix(0L, nrow = nrow(data), ncol = n_feat)
  k_vec   <- integer(n_feat)

  for (j in seq_along(vars_to_use)) {
    var <- vars_to_use[j]
    ref_col <- ref_binned[[var]]   # factor with correct levels from training data
    new_x   <- data[[var]]

    if (!is.null(bins_info$breaks[[var]])) {
      # Continuous variable: assign via the stored interval boundaries.
      # Matching the interval labels instead would turn every raw value into
      # NA, because the reference levels read "(45.2,56.4]" and not "67".
      recoded <- cut(new_x, breaks = bins_info$breaks[[var]],
                     include.lowest = TRUE)
      recoded <- factor(recoded, levels = levels(ref_col))
    } else if (is.factor(ref_col)) {
      # Categorical variable: re-apply same levels; unseen levels -> NA -> 0L
      recoded <- factor(new_x, levels = levels(ref_col))
    } else {
      # Fallback (should not happen after prepare_data)
      recoded <- factor(new_x)
    }

    codes <- as.integer(recoded)
    codes[is.na(codes)] <- 0L
    bin_mat[, j] <- codes
    k_vec[j]     <- nlevels(ref_col)
  }

  list(
    bin_mat = bin_mat,
    k_vec   = k_vec
  )
}


# calculate_boss_balance() was removed on 18.08.2026: a second, independent
# R implementation of the balance formula with the asymmetric denominator
# hard-coded. Once the metric became selectable it would have reported the
# comparison point in a different metric than the frontier it is drawn on.
# All balance values now come from calc_boss_fitness_cpp().
