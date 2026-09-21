# ============================================================
# MOBACO Shiny App - Global Settings
# ============================================================

# Load required packages
library(shiny)
library(MOBACO)
library(DT)

# Optional packages with feature detection
has_plotly <- requireNamespace("plotly", quietly = TRUE)
has_gt <- requireNamespace("gt", quietly = TRUE)
has_MatchIt <- requireNamespace("MatchIt", quietly = TRUE)

# Maximum file upload size (100 MB)
options(shiny.maxRequestSize = 100 * 1024^2)

# Color scheme
colors <- list(
  primary = "#3498DB",
  primary_dark = "#2874A6",
  success = "#1ABC9C",
  success_dark = "#16A085",
  warning = "#F39C12",
  danger = "#E74C3C",
  info = "#5DADE2",
  light_blue = "#EBF5FB",
  dark_gray = "#2C3E50"
)

# Helper function: Read various data formats
# Named read_data_local to avoid masking MOBACO::read_data()
read_data_local <- function(filepath) {
  ext <- tools::file_ext(filepath)

  data <- switch(
    tolower(ext),
    "csv" = read.csv(filepath, stringsAsFactors = FALSE),
    "rds" = readRDS(filepath),
    "xlsx" = {
      if (requireNamespace("readxl", quietly = TRUE)) {
        readxl::read_excel(filepath)
      } else {
        stop("Package 'readxl' required for Excel files")
      }
    },
    "xls" = {
      if (requireNamespace("readxl", quietly = TRUE)) {
        readxl::read_excel(filepath)
      } else {
        stop("Package 'readxl' required for Excel files")
      }
    },
    "sav" = {
      if (requireNamespace("haven", quietly = TRUE)) {
        haven::read_sav(filepath)
      } else {
        stop("Package 'haven' required for SPSS files")
      }
    },
    "dta" = {
      if (requireNamespace("haven", quietly = TRUE)) {
        haven::read_dta(filepath)
      } else {
        stop("Package 'haven' required for Stata files")
      }
    },
    "sas7bdat" = {
      if (requireNamespace("haven", quietly = TRUE)) {
        haven::read_sas(filepath)
      } else {
        stop("Package 'haven' required for SAS files")
      }
    },
    stop("Unsupported file format: ", ext)
  )

  # Convert to data.frame if tibble
  if (inherits(data, "tbl_df")) {
    data <- as.data.frame(data)
  }

  data
}

# Helper function: Format time duration
format_duration <- function(seconds) {
  if (seconds < 60) {
    sprintf("%.0f seconds", seconds)
  } else if (seconds < 3600) {
    minutes <- seconds / 60
    sprintf("%.1f minutes (%.0f seconds)", minutes, seconds)
  } else {
    hours <- seconds / 3600
    minutes <- (seconds %% 3600) / 60
    sprintf("%.1f hours (%.1f minutes)", hours, seconds / 60)
  }
}

# Helper function: Estimate runtime
estimate_runtime <- function(popsize, generations) {
  evals <- popsize * generations
  time_sec <- evals * 0.001  # ~0.001s per evaluation (rough estimate)

  if (time_sec < 60) {
    sprintf("~%.0f seconds", time_sec)
  } else if (time_sec < 3600) {
    sprintf("~%.1f minutes", time_sec / 60)
  } else {
    sprintf("~%.1f hours", time_sec / 3600)
  }
}

# App configuration
APP_CONFIG <- list(
  name = "MOBACO",
  version = as.character(packageVersion("MOBACO")),
  title = "Multi-Objective Balance and Coverage Optimization",
  institution = "Universitaet Ulm",
  department = "Institute of Medical Systems Biology",
  year = "2025"
)
