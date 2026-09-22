# MOBACO — Multi-Objective Balance–Coverage Optimization

R package accompanying the manuscript *Evolutionary multi-objective
optimization in observational studies* (Oruc, Kraus, Stucke-Straub,
Kestler).

MOBACO constructs covariate-balanced cohorts for observational studies by
jointly optimizing covariate balance and sample coverage with NSGA-II.
Rather than a single adjusted cohort it returns the Pareto frontier of
non-dominated subsets, leaving the choice among them to the analyst.

## Installation

```r
install.packages("remotes")
remotes::install_github("sysbio-bioinf/MOBACO")
```

Installing from source requires a C++ compiler: Xcode command line tools on
macOS (`xcode-select --install`), Rtools on Windows, `build-essential` on
Linux. To run the baseline comparisons and open the Shiny application:

```r
install.packages(c("MatchIt", "shiny", "DT", "plotly", "gt", "ggplot2"))
```

Alternatively, clone the repository and install from the directory:

```r
install.packages(".", repos = NULL, type = "source")
```

## Interactive application

The quickest way to see what the method does. No programming required:

```r
library(MOBACO)
mobaco_app()
```

Press **Load Example Cohort**, confirm the preselected treatment variable
and covariates, and start the optimization. The frontend is interactive:
click a point to inspect the corresponding cohort and its balance
diagnostics.

## Quick start in R

```r
library(MOBACO)

cohort <- readRDS(system.file("extdata", "example_cohort.rds",
                              package = "MOBACO"))

res <- mobaco(
  cohort,
  treatment_var   = "prognosis_group",
  treatment_level = "poor",
  covariates      = c("age", "stage", paste0("x0", 1:7)),
  popsize     = 500,
  generations = 2000,
  seed        = 1
)

res                                      # frontier summary
plot(res)                                # balance against coverage
selected <- extract_solution(res, "min_balance")
comparison_table(res)                    # covariate-level balance
```

`extract_solution()` also accepts `"knee"` and `"max_coverage"`, or a row
index into the frontier.

## The example cohort

`inst/extdata/example_cohort.rds` holds 500 simulated units, 103 of them in
the group of interest, with nine covariates of which four are confounders.
Group assignment runs through a logistic model, which puts confounding there
by construction and leaves the confounders known.

The cohort is simulated rather than clinical. That keeps it freely
distributable and makes it reproducible from a single seed:

```
Rscript data-raw/make_example_cohort.R
```

## Contents

| Directory | Contents |
|---|---|
| `R/`, `src/`, `man/`, `tests/` | the R package, including the C++ objective functions |
| `inst/shiny/` | interactive Shiny application |
| `inst/extdata/` | the example cohort |
| `data-raw/` | the script that generates the example cohort |

## Data and analysis code from the manuscript

This repository holds the software. The simulation study and the real-data
analysis reported in the manuscript, together with the computed results,
are available from the authors on request.

## Funding

MO acknowledges support by the "Kooperative Promotionskolloquium Data
Science und Analytics" funded by the State of Baden-Wuerttemberg. HAK
acknowledges funding from the German Science Foundation (DFG, SFB 1506,
Aging at Interfaces, no. 450627322 and GRK 3012, KEMAI, no. 520750254) and
the German Federal Ministry of Education and Research (BMFTR, Medical
Informatics Initiative, project PM4Onco: 01ZZ2322O).

## Citation

If you use this software, please cite the accompanying manuscript:

> Oruc M, Kraus JM, Stucke-Straub K, Kestler HA. Evolutionary
> multi-objective optimization in observational studies.

## License

MIT. See `LICENSE`. Copyright held by Metehan Oruc.

The package is maintained by Hans A. Kestler
(<hans.kestler@uni-ulm.de>), Institute of Medical Systems Biology,
Ulm University. Please direct questions and bug reports there or to the
issue tracker.
