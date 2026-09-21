#' Run NSGA-II Optimization
#'
#' @importFrom mco nsga2
#' @importFrom utils flush.console
#' @keywords internal
run_nsga2 <- function(fitness_fn, n_individuals, popsize, generations,
                      crossover_prob = 0.8,
                      mutation_prob = 0.1,
                      crossover_dist = 10,
                      mutation_dist = 10,
                      verbose) {

  # Logging wrapper
  if (verbose) {
    eval_count <- 0L
    gen_count <- 0L
    gen_best_balance <- Inf
    gen_best_coverage <- -Inf

    logged_fitness <- function(ind) {
      res <- fitness_fn(ind)
      bal <- res[1]
      cov <- -res[2]

      eval_count <<- eval_count + 1L
      if (bal < gen_best_balance) gen_best_balance <<- bal
      if (cov > gen_best_coverage) gen_best_coverage <<- cov

      if (eval_count %% popsize == 0L) {
        gen_count <<- gen_count + 1L
        if (gen_count %% 100L == 0L || gen_count <= 5L) {
          cat(sprintf(
            "Gen %5d | Best Balance: %10.4f | Best Coverage: %.4f\n",
            gen_count, gen_best_balance, gen_best_coverage
          ))
          flush.console()
        }
        gen_best_balance <<- Inf
        gen_best_coverage <<- -Inf
      }
      res
    }
  } else {
    logged_fitness <- fitness_fn
  }

  # Run NSGA-II
  result <- mco::nsga2(
    fn = logged_fitness,
    idim = n_individuals,
    odim = 2,
    lower.bounds = rep(0, n_individuals),
    upper.bounds = rep(1, n_individuals),
    popsize = popsize,
    generations = generations,
    cprob = crossover_prob,
    mprob = mutation_prob,
    cdist = crossover_dist,
    mdist = mutation_dist
  )

  result
}
