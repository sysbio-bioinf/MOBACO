#' Prepare Data for MOBACO
#'
#' @keywords internal
prepare_data <- function(data,
                         treatment_var,
                         treatment_level,
                         covariates = NULL,
                         n_bins = 5,
                         verbose = TRUE) {

  # Auto-detect covariates
  if (is.null(covariates)) {
    covariates <- setdiff(names(data), treatment_var)
    if (verbose) {
      cat(sprintf("Auto-detected %d covariates\n", length(covariates)))
    }
  }

  # Classify variable types
  var_types <- classify_variables(data[covariates])

  if (verbose) {
    cat(sprintf("  Continuous:  %d\n", length(var_types$continuous)))
    cat(sprintf("  Ordinal:     %d\n", length(var_types$ordinal)))
    cat(sprintf("  Binary:      %d\n", length(var_types$binary)))
    cat(sprintf("  Categorical: %d\n", length(var_types$categorical)))
  }

  # Treatment indicators
  T_ind <- data[[treatment_var]] == treatment_level
  C_ind <- !T_ind

  # Remove rows with missing covariates
  complete_rows <- complete.cases(data[covariates])
  if (sum(!complete_rows) > 0) {
    if (verbose) {
      cat(sprintf("  Removing %d rows with missing data\n",
                  sum(!complete_rows)))
    }
    data <- data[complete_rows, ]
    T_ind <- T_ind[complete_rows]
    C_ind <- C_ind[complete_rows]
  }

  # Create bins
  bins <- create_bins(
    data = data[covariates],
    var_types = var_types,
    n_bins = n_bins,
    verbose = verbose
  )

  list(
    data = data,
    T_ind = T_ind,
    C_ind = C_ind,
    T_int = as.integer(T_ind),
    C_int = as.integer(C_ind),
    n_treat = sum(T_ind),
    n_control = sum(C_ind),
    covariates = covariates,
    var_types = var_types,
    bins = bins,
    bin_mat = bins$bin_mat,
    k_vec = bins$k_vec
  )
}


#' Classify Variables
#' @importFrom stats na.omit
#' @keywords internal
classify_variables <- function(data) {
  continuous <- character(0)
  ordinal <- character(0)
  binary <- character(0)
  categorical <- character(0)

  for (var in names(data)) {
    x <- data[[var]]

    if (is.numeric(x)) {
      unique_vals <- length(unique(na.omit(x)))

      if (unique_vals == 2) {
        binary <- c(binary, var)

      } else if (unique_vals <= 10) {
        # Ordinal: discrete numeric with few unique values
        # -> Use unique values as categories (NO EMPTY BINS!)
        ordinal <- c(ordinal, var)

      } else {
        # Continuous: many unique values
        continuous <- c(continuous, var)
      }

    } else if (is.factor(x) || is.character(x)) {
      unique_vals <- length(unique(na.omit(x)))
      if (unique_vals == 2) {
        binary <- c(binary, var)
      } else {
        categorical <- c(categorical, var)
      }
    }
  }

  list(
    continuous = continuous,
    ordinal = ordinal,
    binary = binary,
    categorical = categorical
  )
}


#' Create Bins
#' @keywords internal
create_bins <- function(data, var_types, n_bins = 5, verbose = TRUE) {

  binned <- data
  all_vars <- c(var_types$continuous, var_types$ordinal,
                var_types$binary, var_types$categorical)

  if (verbose) cat("\nBinning variables:\n")

  # Continuous: uniform bins (age)
  # Break points are stored explicitly: apply_bins_to_data() needs them to
  # place new observations into the same intervals. Reproduces what
  # cut(x, breaks = n) computes internally.
  breaks_info <- list()
  for (var in var_types$continuous) {
    rx <- range(data[[var]], na.rm = TRUE)
    dx <- diff(rx)
    nb <- n_bins + 1L
    if (dx == 0) {
      dx <- if (rx[1L] != 0) abs(rx[1L]) else 1
      br <- seq.int(rx[1L] - dx / 1000, rx[2L] + dx / 1000, length.out = nb)
    } else {
      br <- seq.int(rx[1L], rx[2L], length.out = nb)
      br[c(1L, nb)] <- c(rx[1L] - dx / 1000, rx[2L] + dx / 1000)
    }
    breaks_info[[var]] <- br
    binned[[var]] <- cut(data[[var]], breaks = br, include.lowest = TRUE)
    if (verbose) {
      cat(sprintf("  %-20s [continuous] -> %d uniform bins\n", var, n_bins))
    }
  }

  # Ordinal (TNM_stage): unique values as categories (NO EMPTY BINS!)
  for (var in var_types$ordinal) {
    unique_vals <- sort(unique(na.omit(data[[var]])))
    binned[[var]] <- factor(data[[var]], levels = unique_vals)
    if (verbose) {
      cat(sprintf("  %-20s [ordinal] -> %d categories (unique values)\n",
                  var, length(unique_vals)))
      cat(sprintf("                     Values: %s\n",
                  paste(unique_vals, collapse = ", ")))
    }
  }

  # Binary/Categorical: natural levels
  for (var in c(var_types$binary, var_types$categorical)) {
    binned[[var]] <- factor(data[[var]])
    binned[[var]] <- droplevels(binned[[var]])  # Remove empty levels
    if (verbose) {
      cat(sprintf("  %-20s [%s] -> %d categories\n",
                  var,
                  ifelse(var %in% var_types$binary, "binary", "categorical"),
                  nlevels(binned[[var]])))
    }
  }

  # Integer matrix for C++
  n_feat <- length(all_vars)
  bin_mat <- matrix(0L, nrow = nrow(data), ncol = n_feat)
  k_vec <- integer(n_feat)

  for (j in seq_along(all_vars)) {
    var <- all_vars[j]
    codes <- as.integer(binned[[var]])
    codes[is.na(codes)] <- 0L
    bin_mat[, j] <- codes
    k_vec[j] <- nlevels(binned[[var]])
  }

  list(
    binned = binned,
    bin_mat = bin_mat,
    k_vec = k_vec,
    all_vars = all_vars,
    breaks = breaks_info
  )
}
