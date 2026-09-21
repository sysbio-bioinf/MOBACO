#' Run Propensity Score Matching via MatchIt
#'
#' Internal helper to run 1:1 nearest-neighbor PSM for comparison with MOBACO.
#'
#' @param data A data frame (already cleaned/prepared)
#' @param treatment_var Character. Name of the treatment variable.
#' @param treatment_level Character/numeric. Value indicating the treatment group.
#' @param covariates Character vector. Covariate names for the propensity model.
#' @param caliper Numeric. Caliper width as a multiple of the SD of the
#'   logit of the propensity score (default: 0.2).
#'
#' @return A data frame with matched observations, or NULL on failure.
#'
#' @keywords internal
run_psm_matchit <- function(data, treatment_var, treatment_level,
                             covariates, caliper = 0.2) {

  if (!requireNamespace("MatchIt", quietly = TRUE)) {
    warning(
      "Package 'MatchIt' is required for PSM comparison.\n",
      "Install with: install.packages('MatchIt')"
    )
    return(NULL)
  }

  # Create binary treatment indicator (MatchIt needs 0/1)
  data$.treat_binary <- as.integer(data[[treatment_var]] == treatment_level)

  # Build formula: .treat_binary ~ cov1 + cov2 + ...
  rhs <- paste(covariates, collapse = " + ")
  formula_str <- paste(".treat_binary ~", rhs)
  match_formula <- stats::as.formula(formula_str)

  # Run MatchIt
  tryCatch({
    match_out <- MatchIt::matchit(
      formula  = match_formula,
      data     = data,
      method   = "nearest",
      distance = "glm",
      link     = "logit",
      ratio    = 1,
      replace = FALSE,
      caliper  = caliper,
      std.caliper = TRUE
    )

    matched_data <- MatchIt::match.data(match_out)

    # Remove helper columns added by MatchIt and us
    drop_cols <- intersect(
      c(".treat_binary", "distance", "weights", "subclass"),
      names(matched_data)
    )
    matched_data[, drop_cols] <- NULL

    matched_data

  }, error = function(e) {
    warning(sprintf("PSM (MatchIt) failed: %s", conditionMessage(e)))
    return(NULL)
  })
}
