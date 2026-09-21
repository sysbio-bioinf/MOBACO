#include <Rcpp.h>
using namespace Rcpp;

// Incremental evaluation of the balance objective for BOSS.
//
// Under BOSS-B the group of interest is fixed (Nikolaev et al. 2013, p. 402:
// S^T is an input). Its per-bin counts NT are therefore constant over the
// whole run; only NC changes.
//
// A 1-exchange swaps a selected control for an unselected one. Per covariate
// at most two bins change: one loses a unit, one gains a unit. Instead of
// rebuilding the sum over all units, the difference of these terms suffices.
//
// Random numbers stay entirely in R, exactly as for the MOBACO objective.
// The results are therefore bit-identical to the earlier R implementation,
// not merely equivalent.

// Contribution of a single bin, identical to fitness_cpp.cpp
static inline double bin_term(int nt, int nc, int normalization) {
  double diff = (double)(nc - nt);
  int den_i = (normalization == 1) ? (nt + nc) : nt;
  double denom = (den_i > 0) ? (double)den_i : 1.0;
  return (diff * diff) / denom;
}

//' Full balance from the bin counts
//'
//' Needed only at the start and after a restart.
//' @keywords internal
// [[Rcpp::export]]
double boss_objective_cpp(IntegerVector NT, IntegerVector NC,
                          int normalization = 0) {
  int m = NT.size();
  double total = 0.0;
  for (int b = 0; b < m; b++)
    total += bin_term(NT[b], NC[b], normalization);
  return total;
}

//' Change in balance from a 1-exchange
//'
//' @param off       0-based start position of each covariate in the flat vector
//' @param code_out  bin code of the leaving unit, per covariate (1-based)
//' @param code_in   bin code of the entering unit
//' @keywords internal
// [[Rcpp::export]]
double boss_delta_cpp(IntegerVector NT, IntegerVector NC, IntegerVector off,
                      IntegerVector code_out, IntegerVector code_in,
                      int normalization = 0) {
  int P = off.size();
  double delta = 0.0;

  for (int j = 0; j < P; j++) {
    int co = code_out[j], ci = code_in[j];
    if (co == ci) continue;               // same bin, no change

    if (co > 0) {
      int p = off[j] + co - 1;
      delta -= bin_term(NT[p], NC[p], normalization);
      delta += bin_term(NT[p], NC[p] - 1, normalization);
    }
    if (ci > 0) {
      int p = off[j] + ci - 1;
      delta -= bin_term(NT[p], NC[p], normalization);
      delta += bin_term(NT[p], NC[p] + 1, normalization);
    }
  }
  return delta;
}
