# Creates inst/extdata/example_cohort.rds, the data set offered by the Shiny
# application under "Load Example Cohort".
#
#   Rscript data-raw/make_example_cohort.R
#
# The cohort is simulated rather than clinical, which keeps it freely
# distributable and gives it a known structure: the confounders are known by
# construction, and the whole data set follows from one seed.
#
# Mechanism: unbalanced groups of roughly 1:4, mixed covariate types, group
# assignment through a logistic model. Correlated covariates of mixed type
# come from a latent multivariate normal with exchangeable correlation
# (Gaussian copula, also known as NORTA) mapped to the target marginals. No
# outcome variable is generated; MOBACO operates at the design stage only.
#
# These are the settings of the base scenario in the simulation study that
# accompanies the manuscript.

simulate_cohort <- function(n = 500, p = 9, rho = 0.3, gamma = 0.8,
                            prevalence = 0.20, seed = 1) {
  stopifnot(p >= 3, rho >= 0, rho < 1)
  set.seed(seed)

  # Z = sqrt(rho) * shared factor + sqrt(1-rho) * individual part yields
  # Corr(Z_i, Z_j) = rho for all i != j without a Cholesky factorization.
  common <- stats::rnorm(n)
  Z <- sqrt(rho) * common +
       sqrt(1 - rho) * matrix(stats::rnorm(n * p), nrow = n)

  df <- data.frame(row.names = seq_len(n))
  df$age <- round(65 + 12 * Z[, 1])

  # Thresholds chosen to make the level distribution resemble observed
  # clinical stages.
  df$stage <- as.integer(cut(Z[, 2], breaks = c(-Inf, -1.1, -0.1, 0.8, Inf),
                             labels = FALSE))

  n_bin <- p - 2
  prev_bin <- seq(0.15, 0.50, length.out = n_bin)
  for (k in seq_len(n_bin)) {
    df[[sprintf("x%02d", k)]] <-
      ifelse(Z[, 2 + k] > stats::qnorm(1 - prev_bin[k]), "yes", "no")
  }

  # The linear predictor uses only half of the covariates. Not every
  # covariate is a confounder in real data either.
  n_conf <- max(2L, floor(p / 2))
  lp <- gamma * rowSums(Z[, seq_len(n_conf), drop = FALSE]) / sqrt(n_conf)

  # Intercept calibrated numerically to hit the target prevalence.
  a <- stats::uniroot(function(a) mean(stats::plogis(lp + a)) - prevalence,
                      interval = c(-20, 20))$root
  treat <- stats::rbinom(n, 1L, stats::plogis(lp + a))

  df$prognosis_group <- ifelse(treat == 1L, "poor", "good")

  attr(df, "params") <- list(n = n, p = p, rho = rho, gamma = gamma,
                             prevalence = prevalence, seed = seed,
                             n_confounders = n_conf,
                             n_treat = sum(treat), n_control = sum(1 - treat))
  df
}

cohort <- simulate_cohort(n = 500, p = 9, rho = 0.3, gamma = 0.8, seed = 1)

out <- file.path("inst", "extdata", "example_cohort.rds")
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
saveRDS(cohort, out)

pr <- attr(cohort, "params")
cat(sprintf("Example cohort -> %s\n", out))
cat(sprintf("  n = %d | covariates = %d | confounders = %d\n",
            pr$n, pr$p, pr$n_confounders))
cat(sprintf("  group of interest %d, comparison %d | seed = %d\n",
            pr$n_treat, pr$n_control, pr$seed))
