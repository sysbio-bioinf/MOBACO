make_prep <- function(d, covariates, n_bins = 5) {
  MOBACO:::prepare_data(d, treatment_var = "g", treatment_level = 1,
                        covariates = covariates, n_bins = n_bins,
                        verbose = FALSE)
}


test_that("binning follows the documented rule", {
  d <- data.frame(
    cont = seq(1, 100, length.out = 50),   # many distinct values
    ord  = rep(1:4, length.out = 50),      # few distinct values
    bin  = rep(c("a", "b"), length.out = 50),
    g    = rep(c(0, 1), length.out = 50),
    stringsAsFactors = FALSE
  )
  prep <- make_prep(d, c("cont", "ord", "bin"))
  # k_vec is unnamed and follows the order of prep$covariates.
  k <- setNames(prep$k_vec, prep$covariates)

  # Continuous covariates are cut into n_bins intervals; discrete ones keep
  # one bin per observed level.
  expect_equal(unname(k[["cont"]]), 5)
  expect_equal(unname(k[["ord"]]),  4)
  expect_equal(unname(k[["bin"]]),  2)
})


test_that("a perfectly balanced subset has zero imbalance", {
  # Both groups have the same composition, so every bin holds equal counts
  # and the objective must be exactly zero under either normalization.
  d <- data.frame(
    x = rep(c("a", "b"), each = 10),
    g = rep(c(0, 1), times = 10),
    stringsAsFactors = FALSE
  )
  prep <- make_prep(d, "x")
  sel <- rep(1, nrow(d))

  for (code in c(0L, 1L)) {
    val <- MOBACO:::calc_boss_fitness_cpp(
      sel, prep$bin_mat, as.integer(prep$k_vec),
      prep$T_int, prep$C_int, prep$n_treat, prep$n_control, 1L, code)
    expect_equal(val[1], 0)
  }
})


test_that("coverage is the mean retention of the two groups", {
  d <- data.frame(
    x = rep("a", 50),
    g = c(rep(1, 10), rep(0, 40)),
    stringsAsFactors = FALSE
  )
  prep <- make_prep(d, "x")

  # Retain 5 of 10 treated and 20 of 40 controls: 0.5 * (0.5 + 0.5) = 0.5
  sel <- c(rep(1, 5), rep(0, 5), rep(1, 20), rep(0, 20))
  val <- MOBACO:::calc_boss_fitness_cpp(
    sel, prep$bin_mat, as.integer(prep$k_vec),
    prep$T_int, prep$C_int, prep$n_treat, prep$n_control, 1L, 0L)

  expect_equal(-val[2], 0.5)
})


test_that("subsets below the minimum group size are penalized", {
  d <- data.frame(
    x = rep("a", 20),
    g = c(rep(1, 10), rep(0, 10)),
    stringsAsFactors = FALSE
  )
  prep <- make_prep(d, "x")

  sel <- rep(0, 20)
  sel[1] <- 1                          # one treated unit, no controls
  val <- MOBACO:::calc_boss_fitness_cpp(
    sel, prep$bin_mat, as.integer(prep$k_vec),
    prep$T_int, prep$C_int, prep$n_treat, prep$n_control, 5L, 0L)

  expect_gte(val[1], 1e6)
})
