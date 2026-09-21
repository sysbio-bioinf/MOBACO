#' Auto-detect Treatment Variable
#'
#' Automatically identify the treatment/group variable in a dataset
#'
#' @param data Data frame
#' @param treatment_var Character. Manual specification (overrides auto-detection)
#'
#' @return Character. Name of treatment variable
#'
#' @importFrom stats na.omit
#' @keywords internal
auto_detect_treatment <- function(data, treatment_var = NULL) {

  if (!is.null(treatment_var)) {
    # Manual specification
    if (!treatment_var %in% names(data)) {
      stop(sprintf("Treatment variable '%s' not found in data", treatment_var))
    }
    return(treatment_var)
  }

  # Auto-detection: look for common patterns
  candidates <- character(0)

  # Common treatment variable names
  treatment_patterns <- c(
    "^treat", "^trt$", "^group$", "^arm$", "^cohort$",
    "^condition$", "^intervention$", "^exposure$", "^exposed$",
    "^case$", "^status$",
    "treated", "control", "prognosis", "outcome"
  )

  for (pattern in treatment_patterns) {
    matches <- grep(pattern, names(data), ignore.case = TRUE, value = TRUE)
    candidates <- c(candidates, matches)
  }

  # Filter to binary or categorical variables. The emptiness check must
  # come first: sapply() over an empty list returns list(), and indexing a
  # character vector with it raises "invalid subscript type".
  candidates <- unique(candidates)
  if (length(candidates) > 0) {
    keep <- vapply(candidates,
                   function(v) length(unique(na.omit(data[[v]]))) <= 10,
                   logical(1))
    candidates <- candidates[keep]
  }

  if (length(candidates) == 0) {
    stop(paste(
      "Could not auto-detect treatment variable.",
      "Please specify manually with treatment_var = '...'",
      "\nAvailable variables:", paste(names(data), collapse = ", ")
    ))
  }

  if (length(candidates) > 1) {
    warning(sprintf(
      "Multiple treatment candidates found: %s. Using '%s'. Specify manually if incorrect.",
      paste(candidates, collapse = ", "),
      candidates[1]
    ))
  }

  candidates[1]
}


#' Auto-detect Treatment Level
#'
#' Automatically identify which level is the "treatment" group
#'
#' @param data Data frame
#' @param treatment_var Character. Treatment variable name
#' @param treatment_level Character/numeric. Manual specification
#'
#' @return Character/numeric. Treatment level
#'
#' @keywords internal
auto_detect_treatment_level <- function(data, treatment_var, treatment_level = NULL) {

  if (!is.null(treatment_level)) {
    # Manual specification
    if (!treatment_level %in% data[[treatment_var]]) {
      stop(sprintf(
        "Treatment level '%s' not found in variable '%s'.\nAvailable levels: %s",
        treatment_level,
        treatment_var,
        paste(unique(data[[treatment_var]]), collapse = ", ")
      ))
    }
    return(treatment_level)
  }

  # Auto-detection: smaller group or treatment-like name
  levels_var <- unique(data[[treatment_var]])

  if (length(levels_var) != 2) {
    stop(sprintf(
      "Treatment variable '%s' has %d levels (expected 2).\nLevels: %s\nPlease specify treatment_level manually.",
      treatment_var,
      length(levels_var),
      paste(levels_var, collapse = ", ")
    ))
  }

  # Count group sizes
  counts <- table(data[[treatment_var]])

  # Treatment patterns (prefer these if present)
  treatment_patterns <- c(
    "treatment", "treated", "case", "exposed", "intervention",
    "poor", "high", "positive", "yes", "1", "true"
  )

  for (pattern in treatment_patterns) {
    match <- grep(pattern, levels_var, ignore.case = TRUE, value = TRUE)
    if (length(match) == 1) {
      message(sprintf(
        "Auto-detected treatment level: '%s' (based on name pattern)",
        match
      ))
      return(match)
    }
  }

  # Default: smaller group
  smaller_group <- names(which.min(counts))
  message(sprintf(
    "Auto-detected treatment level: '%s' (smaller group: %d vs %d)",
    smaller_group,
    min(counts),
    max(counts)
  ))

  smaller_group
}


#' Auto-detect Covariates
#'
#' Automatically identify which variables should be balanced
#'
#' @param data Data frame
#' @param treatment_var Character. Treatment variable name
#' @param exclude Character vector. Variables to exclude
#'
#' @return Character vector. Covariate names
#'
#' @keywords internal
auto_detect_covariates <- function(data, treatment_var, exclude = NULL) {

  # Start with all variables except treatment
  all_vars <- setdiff(names(data), treatment_var)

  # Exclude specified variables
  if (!is.null(exclude)) {
    all_vars <- setdiff(all_vars, exclude)
  }

  # Exclude common ID variables
  id_patterns <- c(
    "^id$", "^patient_id$", "^subject_id$", "^case_id$",
    "^row_number$", "^index$", "^sequence$"
  )

  for (pattern in id_patterns) {
    id_vars <- grep(pattern, all_vars, ignore.case = TRUE, value = TRUE)
    all_vars <- setdiff(all_vars, id_vars)
  }

  # Exclude propensity score if present
  ps_vars <- grep("propensity|pscore|ps$", all_vars, ignore.case = TRUE, value = TRUE)
  all_vars <- setdiff(all_vars, ps_vars)

  # Exclude variables with too many missing values (>50%)
  missing_pct <- sapply(data[all_vars], function(x) sum(is.na(x)) / length(x))
  high_missing <- names(missing_pct[missing_pct > 0.5])

  if (length(high_missing) > 0) {
    message(sprintf(
      "Excluding %d variables with >50%% missing: %s",
      length(high_missing),
      paste(high_missing, collapse = ", ")
    ))
    all_vars <- setdiff(all_vars, high_missing)
  }

  # Exclude constant variables
  constant_vars <- all_vars[sapply(data[all_vars], function(x) {
    length(unique(na.omit(x))) == 1
  })]

  if (length(constant_vars) > 0) {
    message(sprintf(
      "Excluding %d constant variables: %s",
      length(constant_vars),
      paste(constant_vars, collapse = ", ")
    ))
    all_vars <- setdiff(all_vars, constant_vars)
  }

  all_vars
}


#' Smart Data Validation
#'
#' Validate and prepare data with helpful error messages
#'
#' @keywords internal
validate_data <- function(data, treatment_var, treatment_level, covariates) {

  errors <- character(0)
  warnings <- character(0)

  # Check data is data frame
  if (!is.data.frame(data)) {
    errors <- c(errors, "Input must be a data.frame")
  }

  # Check sufficient rows
  if (nrow(data) < 20) {
    errors <- c(errors, sprintf("Dataset too small: %d rows (minimum: 20)", nrow(data)))
  }

  # Check treatment variable exists
  if (!treatment_var %in% names(data)) {
    errors <- c(errors, sprintf("Treatment variable '%s' not found", treatment_var))
  } else {
    # Check treatment level exists
    if (!treatment_level %in% data[[treatment_var]]) {
      errors <- c(errors, sprintf(
        "Treatment level '%s' not found in '%s'",
        treatment_level, treatment_var
      ))
    }

    # Check group sizes
    counts <- table(data[[treatment_var]])
    if (min(counts) < 5) {
      warnings <- c(warnings, sprintf(
        "Small group size: %d (minimum recommended: 10)",
        min(counts)
      ))
    }
  }

  # Check covariates exist
  missing_covars <- setdiff(covariates, names(data))
  if (length(missing_covars) > 0) {
    errors <- c(errors, sprintf(
      "Covariates not found: %s",
      paste(missing_covars, collapse = ", ")
    ))
  }

  # Check for duplicate column names
  if (any(duplicated(names(data)))) {
    errors <- c(errors, "Dataset contains duplicate column names")
  }

  # Report errors
  if (length(errors) > 0) {
    stop(paste(
      "Data validation failed:",
      paste("  -", errors, collapse = "\n"),
      sep = "\n"
    ))
  }

  # Report warnings
  if (length(warnings) > 0) {
    warning(paste(
      "Data validation warnings:",
      paste("  -", warnings, collapse = "\n"),
      sep = "\n"
    ))
  }

  invisible(TRUE)
}
