#include <Rcpp.h>
using namespace Rcpp;

//' Calculate Balance-Coverage Fitness (C++)
 //'
 //' @param normalization Integer. 0 = asymmetric, denominator max(N_T, 1),
 //'   as in Nikolaev et al. (2013), Eq. (4); 1 = symmetric, denominator
 //'   max(N_T + N_C, 1). Default 0 reproduces earlier results bit for bit.
 //' @keywords internal
 // [[Rcpp::export]]
 NumericVector calc_boss_fitness_cpp(
     NumericVector ind,
     IntegerMatrix bin_mat,
     IntegerVector k_vec,
     IntegerVector T_int,
     IntegerVector C_int,
     int nT_total,
     int nC_total,
     int min_per_group,
     int normalization = 0
 ) {
   int N = ind.size();
   int P = k_vec.size();

   // Selection
   int n_treat = 0, n_control = 0;
   for (int i = 0; i < N; i++) {
     if (ind[i] > 0.5) {
       n_treat   += T_int[i];
       n_control += C_int[i];
     }
   }

   NumericVector result(2);
   if (n_treat < min_per_group || n_control < min_per_group) {
     result[0] = 1e6;
     result[1] = 1.0;
     return result;
   }

   // BOSS-B balance calculation
   double total_balance = 0.0;

   for (int j = 0; j < P; j++) {
     int k = k_vec[j];
     std::vector<int> N_T(k + 1, 0);
     std::vector<int> N_C(k + 1, 0);

     for (int i = 0; i < N; i++) {
       if (ind[i] > 0.5) {
         int code = bin_mat(i, j);
         if (code > 0) {  // 0 = NA
           if (T_int[i] == 1) {
             N_T[code]++;
           } else {
             N_C[code]++;
           }
         }
       }
     }

     for (int b = 1; b <= k; b++) {
       double diff = (double)(N_C[b] - N_T[b]);
       // asymmetric: max(N_T, 1) — symmetric: max(N_T + N_C, 1)
       int den_i = (normalization == 1) ? (N_T[b] + N_C[b]) : N_T[b];
       double denom = (den_i > 0) ? (double)den_i : 1.0;
       total_balance += (diff * diff) / denom;
     }
   }

   // Coverage
   double coverage = 0.5 * (((double)n_treat / nT_total) +
                            ((double)n_control / nC_total));

   result[0] = total_balance;
   result[1] = -coverage;
   return result;
 }
