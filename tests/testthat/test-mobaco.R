# Deliberately tiny settings: these tests check that the machinery works,
# not that it converges. CRAN limits the total runtime of all examples and
# tests, so a realistic optimization would not be acceptable here.

small_cohort <- function(n = 60, seed = 1) {
  set.seed(seed)
  data.frame(
    grp = rep(c("case", "ctrl"), times = c(n / 4, 3 * n / 4)),
    age = round(rnorm(n, 60, 10)),
    sex = sample(c("m", "f"), n, replace = TRUE),
    stage = sample(1:3, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}


test_that("mobaco returns a frontier of the documented shape", {
  d <- small_cohort()
  res <- mobaco(d, treatment_var = "grp", treatment_level = "case",
                covariates = c("age", "sex", "stage"),
                popsize = 20, generations = 10, seed = 1, verbose = FALSE)

  expect_s3_class(res, "mobaco")
  expect_true(nrow(res$pareto_front) >= 1)
  expect_true(all(c("Balance", "Coverage") %in% names(res$pareto_front)))
  expect_true(all(res$pareto_front$Balance >= 0))
  expect_true(all(res$pareto_front$Coverage > 0 & res$pareto_front$Coverage <= 1))
})


test_that("no frontier solution dominates another", {
  d <- small_cohort()
  res <- mobaco(d, treatment_var = "grp", treatment_level = "case",
                covariates = c("age", "sex", "stage"),
                popsize = 20, generations = 10, seed = 2, verbose = FALSE)

  f <- res$pareto_front
  # Pareto optimality: for no pair (i, j) is j at least as good in both
  # objectives and strictly better in one.
  for (i in seq_len(nrow(f))) {
    dominated <- f$Balance <= f$Balance[i] & f$Coverage >= f$Coverage[i] &
                 (f$Balance < f$Balance[i] | f$Coverage > f$Coverage[i])
    expect_false(any(dominated))
  }
})


test_that("the same seed reproduces the same frontier", {
  d <- small_cohort()
  args <- list(data = d, treatment_var = "grp", treatment_level = "case",
               covariates = c("age", "sex", "stage"),
               popsize = 20, generations = 10, seed = 7, verbose = FALSE)
  a <- do.call(mobaco, args)
  b <- do.call(mobaco, args)

  expect_equal(a$pareto_front, b$pareto_front)
})


test_that("extract_solution returns a subset of the input", {
  d <- small_cohort()
  res <- mobaco(d, treatment_var = "grp", treatment_level = "case",
                covariates = c("age", "sex", "stage"),
                popsize = 20, generations = 10, seed = 3, verbose = FALSE)

  sol <- extract_solution(res, "min_balance")
  expect_s3_class(sol, "data.frame")
  expect_true(nrow(sol) > 0 && nrow(sol) <= nrow(d))
  expect_true(all(names(d) %in% names(sol)))
})


test_that("an unknown treatment level is rejected", {
  d <- small_cohort()
  expect_error(
    mobaco(d, treatment_var = "grp", treatment_level = "does_not_exist",
           covariates = c("age", "sex"), popsize = 20, generations = 5,
           verbose = FALSE)
  )
})
