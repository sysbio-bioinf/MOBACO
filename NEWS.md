# MOBACO 0.1.0

First public release, accompanying the manuscript *Evolutionary
multi-objective optimization in observational studies* (Oruc, Kraus,
Stucke-Straub, Kestler).

## Features

* `mobaco()` constructs covariate-balanced cohorts by jointly optimizing
  covariate balance and sample coverage with NSGA-II, returning the Pareto
  front of non-dominated subsets rather than a single adjusted cohort.
* `extract_solution()` retrieves a cohort from the frontier, either by name
  (`"min_balance"`, `"knee"`, `"max_coverage"`) or by row index.
* `comparison_table()` reports covariate-level balance before adjustment,
  after MOBACO, and after propensity score matching.
* `mobaco_app()` opens an interactive Shiny application for exploring the
  frontier, selecting a cohort and exporting balance diagnostics.
* `read_data()` imports cohorts from CSV, Excel, SPSS, Stata and RDS files.
* Covariate imbalance is evaluated in C++ (`Rcpp`), with an incremental
  1-exchange update used by the BOSS baseline.
* A simulated example cohort ships with the package in `inst/extdata/`,
  generated with a fixed seed by `data-raw/make_example_cohort.R`.
