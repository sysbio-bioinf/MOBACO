#' Balance normalizations supported by the fitness function
#'
#' The integer codes are passed straight to \code{calc_boss_fitness_cpp}.
#' @keywords internal
NORMALIZATIONS <- c(asymmetric = 0L, symmetric = 1L)

#' Build Balance-Coverage Fitness Function
#'
#' @param normalization Character. Either \code{"asymmetric"} (default,
#'   Nikolaev et al. 2013) or \code{"symmetric"}.
#' @keywords internal
build_boss_fitness <- function(prep, min_per_group,
                               normalization = "asymmetric") {

  normalization <- match.arg(normalization, names(NORMALIZATIONS))
  norm_code <- unname(NORMALIZATIONS[normalization])

  function(ind) {
    calc_boss_fitness_cpp(
      ind = ind,
      bin_mat = prep$bin_mat,
      k_vec = prep$k_vec,
      T_int = prep$T_int,
      C_int = prep$C_int,
      nT_total = prep$n_treat,
      nC_total = prep$n_control,
      min_per_group = min_per_group,
      normalization = norm_code
    )
  }
}
