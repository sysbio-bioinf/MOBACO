#' Multi-Objective Balance Optimization
#'
#' Optimize covariate balance between treatment and control groups using
#' BOSS-B algorithm (Nikolaev et al. 2013) with NSGA-II genetic algorithm.
#'
#' @param data A data frame containing the dataset
#' @param treatment_var Character. Name of the treatment/group variable.
#'   If NULL (default), will attempt auto-detection.
#' @param treatment_level Character/numeric. Value indicating treatment group.
#'   If NULL (default), will use smaller group or auto-detect based on name.
#'   The treatment group is the group of interest (e.g., "poor" prognosis, "treated", etc.)
#' @param covariates Character vector. Names of covariates to balance.
#'   If NULL (default), uses all variables except treatment_var and IDs.
#' @param n_bins Integer. Number of bins for continuous variables (default: 5)
#' @param popsize Integer. NSGA-II population size (default: 200)
#' @param generations Integer. Number of generations (default: 500)
#' @param crossover_prob Numeric. Crossover probability (default: 0.8)
#' @param mutation_prob Numeric. Mutation probability (default: 0.1)
#' @param crossover_dist Integer. Crossover distribution index (default: 10)
#' @param mutation_dist Integer. Mutation distribution index (default: 10)
#' @param min_per_group Integer. Minimum patients per group (default: 5% of smaller group)
#' @param normalization Character. Balance normalization: \code{"asymmetric"}
#'   (default) divides by max(N_T, 1) as in Nikolaev et al. (2013);
#'   \code{"symmetric"} divides by max(N_T + N_C, 1). The published results
#'   use \code{"asymmetric"}.
#' @param run_psm Logical. Run PSM (MatchIt) for comparison? (default: FALSE)
#' @param psm_caliper Numeric. PSM caliper = caliper × SD(PS) (default: 0.2)
#' @param seed Integer. Random seed (default: 42)
#' @param verbose Logical. Print progress? (default: TRUE)
#'
#'
#' @return A mobaco object containing:
#'   \itemize{
#'     \item \code{result}: NSGA-II result object
#'     \item \code{pareto_front}: Data frame with Balance (BOSS-B) and Coverage
#'     \item \code{best_solutions}: Indices for min_balance, knee_point, max_coverage
#'     \item \code{selected_data}: Selected datasets for each solution
#'     \item \code{original_data}: Original input data
#'     \item \code{psm_data}: PSM matched data (if run_psm = TRUE)
#'   }
#'
#' @examples
#' \dontrun{
#' # Minimal usage
#' result <- mobaco(data = mydata)
#'
#' # With PSM comparison
#' result <- mobaco(data = mydata, run_psm = TRUE)
#'
#' # Full control
#' result <- mobaco(
#'   data = mydata,
#'   treatment_var = "group",
#'   treatment_level = "treatment",
#'   popsize = 500,
#'   generations = 10000,
#'   crossover_prob = 0.9,
#'   mutation_prob = 0.05,
#'   run_psm = TRUE
#' )
#' }
#'
#' @references
#' Nikolaev, A. G., Jacobson, S. H., Cho, W. K. T., Sauppe, J. J., &
#' Sewell, E. C. (2013). Balance optimization subset selection (BOSS):
#' An alternative approach for causal inference with observational data.
#' Operations Research, 61(2), 398-412.
#' https://doi.org/10.1287/opre.1120.1118
#'
#' @export
mobaco <- function(data,
                   treatment_var = NULL,
                   treatment_level = NULL,
                   covariates = NULL,
                   n_bins = 5,
                   popsize = 200,
                   generations = 500,
                   crossover_prob = 0.8,
                   mutation_prob = 0.1,
                   crossover_dist = 10,
                   mutation_dist = 10,
                   min_per_group = NULL,
                   normalization = "asymmetric",
                   run_psm = FALSE,
                   psm_caliper = 0.2,
                   seed = 42,
                   verbose = TRUE) {

  normalization <- match.arg(normalization, names(NORMALIZATIONS))

  call <- match.call()

  # Auto-detection
  if (verbose) cat("\n=== MOBACO Setup ===\n")

  treatment_var <- auto_detect_treatment(data, treatment_var)
  if (verbose) cat(sprintf("Treatment variable: %s\n", treatment_var))

  treatment_level <- auto_detect_treatment_level(data, treatment_var, treatment_level)
  if (verbose) cat(sprintf("Treatment level: %s\n", treatment_level))

  if (is.null(covariates)) {
    covariates <- auto_detect_covariates(data, treatment_var)
    if (verbose) {
      cat(sprintf("Auto-detected %d covariates:\n", length(covariates)))
      cat(sprintf("  %s\n", paste(covariates, collapse = ", ")))
    }
  }

  validate_data(data, treatment_var, treatment_level, covariates)

  # Prepare data
  if (verbose) cat("\n=== Data Preparation ===\n")
  prep <- prepare_data(
    data = data,
    treatment_var = treatment_var,
    treatment_level = treatment_level,
    covariates = covariates,
    n_bins = n_bins,
    verbose = verbose
  )

  if (is.null(min_per_group)) {
    min_per_group <- max(5, floor(0.10 * min(prep$n_treat, prep$n_control)))
  }

  metric_label <- c(
    asymmetric = "asymmetric, denominator max(N_T, 1) (Nikolaev et al. 2013)",
    symmetric  = "symmetric, denominator max(N_T + N_C, 1)"
  )[normalization]

  if (verbose) {
    cat(sprintf("\n=== Fitness Function ===\n"))
    cat(sprintf("Balance metric: %s\n", metric_label))
  }

  fitness_fn <- build_boss_fitness(
    prep = prep,
    min_per_group = min_per_group,
    normalization = normalization
  )

  # Run NSGA-II
  if (verbose) {
    cat("\n=====================================================\n")
    cat(" NSGA-II Optimization\n")
    cat("=====================================================\n")
    cat(sprintf(" Balance Metric: %s\n", normalization))
    cat(sprintf(" Population: %d | Generations: %d\n", popsize, generations))
    cat(sprintf(" Crossover: %.2f | Mutation: %.2f\n", crossover_prob, mutation_prob))
    cat(sprintf(" Treatment: %d | Control: %d\n", prep$n_treat, prep$n_control))
    cat(sprintf(" Min per group: %d\n", min_per_group))
    cat("=====================================================\n\n")
  }

  set.seed(seed)
  ga_result <- run_nsga2(
    fitness_fn = fitness_fn,
    n_individuals = nrow(prep$data),
    popsize = popsize,
    generations = generations,
    crossover_prob = crossover_prob,
    mutation_prob = mutation_prob,
    crossover_dist = crossover_dist,
    mutation_dist = mutation_dist,
    verbose = verbose
  )

  # Process results
  if (verbose) cat("\n=== Processing Results ===\n")
  results <- process_results(
    ga_result = ga_result,
    data = prep$data,
    prep = prep,
    treatment_var = treatment_var
  )

  # Run PSM if requested
  psm_result <- NULL
  if (run_psm) {
    if (verbose) cat("\n=== Running PSM (MatchIt) ===\n")

    if (requireNamespace("MatchIt", quietly = TRUE)) {
      psm_result <- run_psm_matchit(
        data = prep$data,
        treatment_var = treatment_var,
        treatment_level = treatment_level,
        covariates = covariates,
        caliper = psm_caliper
      )

      if (verbose) {
        cat(sprintf("  Matched: %d patients\n", nrow(psm_result)))
        cat(sprintf("  Treatment: %d | Control: %d\n",
                    sum(psm_result[[treatment_var]] == treatment_level),
                    sum(psm_result[[treatment_var]] != treatment_level)))
      }
    } else {
      warning("MatchIt package not installed. Install with: install.packages('MatchIt')")
    }
  }

  if (verbose) {
    cat(sprintf("\n=== Optimization Complete ===\n"))
    cat(sprintf("Pareto solutions: %d\n", nrow(results$pareto_front)))
    cat(sprintf("Best balance: %.4f (Coverage: %.4f)\n",
                results$pareto_front$Balance[results$best_solutions$min_balance],
                results$pareto_front$Coverage[results$best_solutions$min_balance]))
    cat(sprintf("\nUse summary(result) and plot(result) to explore.\n"))
  }

  # Return
  structure(
    list(
      result = ga_result,
      pareto_front = results$pareto_front,
      best_solutions = results$best_solutions,
      selected_data = results$selected_data,
      bins = prep$bins,
      prep = prep,
      original_data = prep$data,
      psm_data = psm_result,
      treatment_var = treatment_var,
      treatment_level = treatment_level,
      covariates = covariates,
      normalization = normalization,
      min_per_group = min_per_group,
      call = call
    ),
    class = "mobaco"
  )
}
