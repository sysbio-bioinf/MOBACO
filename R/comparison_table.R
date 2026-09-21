# Suppress R CMD check NOTEs for dplyr non-standard evaluation variables
utils::globalVariables(c(
  "Characteristic",
  "Before_Treat", "Before_Control", "Before_SMD",
  "After_Treat",  "After_Control",  "After_SMD",
  "MOBACO_Treat", "MOBACO_Control", "MOBACO_SMD",
  "PSM_Treat",    "PSM_Control",    "PSM_SMD"
))

#' Create Comparison Table
#'
#' @param mobaco_result A mobaco object
#' @param solution Character. Which solution:
#'   "min_balance" (default), "knee", or "max_coverage"
#' @param include_psm Logical. Include PSM? (default: TRUE if PSM was run)
#' @param psm_caliper Numeric. PSM caliper (default: 0.2)
#'
#' @export
comparison_table <- function(mobaco_result,
                             solution = "min_balance",
                             include_psm = NULL,
                             psm_caliper = 0.2) {

  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' required. Install: install.packages('gt')")
  }

  # Auto-detect PSM
  if (is.null(include_psm)) {
    include_psm <- !is.null(mobaco_result$psm_data)
  }

  data_before <- mobaco_result$original_data
  treatment_var <- mobaco_result$treatment_var
  treatment_level <- mobaco_result$treatment_level
  covariates <- mobaco_result$covariates

  # Get solution
  solution <- match.arg(solution, c("min_balance", "knee", "max_coverage"))
  if (solution == "knee") solution <- "knee_point"

  data_mobaco <- mobaco_result$selected_data[[solution]]

  # Build master level structure from Before data
  level_structure <- build_level_structure(data_before, treatment_var,
                                           treatment_level, covariates)

  # PSM
  if (include_psm) {
    if (!is.null(mobaco_result$psm_data)) {
      data_psm <- mobaco_result$psm_data
    } else if (requireNamespace("MatchIt", quietly = TRUE)) {
      cat("Running PSM (MatchIt) for comparison...\n")
      data_psm <- run_psm_matchit(
        data = data_before,
        treatment_var = treatment_var,
        treatment_level = treatment_level,
        covariates = covariates,
        caliper = psm_caliper
      )
    } else {
      warning("MatchIt not installed. Showing MOBACO only.")
      include_psm <- FALSE
      data_psm <- NULL
    }

    if (!is.null(data_psm)) {
      return(build_3way_table(
        data_before, data_mobaco, data_psm,
        treatment_var, treatment_level, covariates,
        level_structure,
        gsub("_", " ", solution),
        psm_caliper = psm_caliper
      ))
    }
  }

  build_2way_table(
    data_before, data_mobaco,
    treatment_var, treatment_level, covariates,
    level_structure,
    gsub("_", " ", solution)
  )
}


#' Build Level Structure
#' @keywords internal
build_level_structure <- function(data, treatment_var, treatment_level, covariates) {

  structure <- list()

  for (var in covariates) {
    x <- data[[var]]

    if (is.numeric(x)) {
      structure[[var]] <- list(type = "numeric", levels = NULL)
    } else {
      # Get all levels from before data
      x_factor <- as.factor(x)
      x_factor <- droplevels(x_factor)
      all_levels <- levels(x_factor)
      all_levels <- all_levels[all_levels != "" & !is.na(all_levels)]

      structure[[var]] <- list(type = "categorical", levels = all_levels)
    }
  }

  structure
}


#' Get Table Columns with Fixed Structure
#' @keywords internal
get_table_cols <- function(data, treatment_var, treatment_level, covariates, level_structure) {

  treat_data <- data[data[[treatment_var]] == treatment_level, ]
  control_data <- data[data[[treatment_var]] != treatment_level, ]

  n_treat <- nrow(treat_data)
  n_control <- nrow(control_data)

  treat_col <- character(0)
  control_col <- character(0)
  smd_col <- character(0)
  char_col <- character(0)

  for (var in covariates) {
    var_structure <- level_structure[[var]]

    x_treat <- treat_data[[var]]
    x_control <- control_data[[var]]

    if (var_structure$type == "numeric") {
      # Numeric variable
      decimals <- if (grepl("stage|tnm|severity|disease", tolower(var))) 2 else 1

      char_col <- c(char_col, var)
      treat_col <- c(treat_col, fmt_mean_sd(x_treat, decimals))
      control_col <- c(control_col, fmt_mean_sd(x_control, decimals))
      smd_col <- c(smd_col, smd_continuous(x_treat, x_control))

    } else {
      # Categorical
      all_levels <- var_structure$levels

      # Convert to factor
      x_treat <- factor(x_treat, levels = all_levels)
      x_control <- factor(x_control, levels = all_levels)

      # Variable name row
      char_col <- c(char_col, var)
      treat_col <- c(treat_col, "")
      control_col <- c(control_col, "")
      smd_col <- c(smd_col, smd_binary(x_treat, x_control, all_levels[1]))

      # Level rows
      for (lev in all_levels) {
        char_col <- c(char_col, paste0("  ", lev))
        treat_col <- c(treat_col, fmt_n_pct(x_treat, lev, n_treat))
        control_col <- c(control_col, fmt_n_pct(x_control, lev, n_control))
        smd_col <- c(smd_col, "")
      }
    }
  }

  list(
    characteristics = char_col,
    treat = treat_col,
    control = control_col,
    smd = smd_col,
    n_treat = n_treat,
    n_control = n_control
  )
}


#' Run PSM with MatchIt
#' @keywords internal
run_psm_matchit <- function(data, treatment_var, treatment_level,
                            covariates, caliper = 0.2) {

  data$treatment_bin <- as.integer(data[[treatment_var]] == treatment_level)

  formula_str <- paste("treatment_bin ~", paste(covariates, collapse = " + "))
  formula_obj <- as.formula(formula_str)

  psm_result <- MatchIt::matchit(
    formula_obj,
    data = data,
    method = "nearest",
    distance = "logit",
    caliper = caliper,
    ratio = 1,
    replace = FALSE
  )

  matched <- MatchIt::match.data(psm_result)
  matched$treatment_bin <- NULL

  matched
}


#' Calculate SMD for continuous variables
#' @keywords internal
smd_continuous <- function(x1, x0) {
  s <- sqrt((var(x1, na.rm = TRUE) + var(x0, na.rm = TRUE)) / 2)
  if (is.na(s) || s == 0) return(0)
  round(abs((mean(x1, na.rm = TRUE) - mean(x0, na.rm = TRUE)) / s), 3)
}


#' Calculate SMD for binary/categorical variables
#' @keywords internal
smd_binary <- function(x1, x0, level) {
  p1 <- mean(x1 == level, na.rm = TRUE)
  p0 <- mean(x0 == level, na.rm = TRUE)
  denom <- sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
  if (is.na(denom) || denom == 0) return(0)
  round(abs((p1 - p0) / denom), 3)
}


#' Format mean (SD)
#' @keywords internal
fmt_mean_sd <- function(x, decimals = 1) {
  sprintf(paste0("%.", decimals, "f (%.", decimals, "f)"),
          mean(x, na.rm = TRUE),
          sd(x, na.rm = TRUE))
}


#' Format n (%)
#' @keywords internal
fmt_n_pct <- function(x, level, n_total) {
  n <- sum(x == level, na.rm = TRUE)
  sprintf("%d (%.1f%%)", n, 100 * n / n_total)
}


#' Build 2-way comparison table
#' @keywords internal
build_2way_table <- function(data_before, data_mobaco, treatment_var,
                             treatment_level, covariates, level_structure,
                             solution_label) {

  before <- get_table_cols(data_before, treatment_var, treatment_level,
                           covariates, level_structure)
  after <- get_table_cols(data_mobaco, treatment_var, treatment_level,
                          covariates, level_structure)

  tbl <- data.frame(
    Characteristic = before$characteristics,
    Before_Treat = before$treat,
    Before_Control = before$control,
    Before_SMD = before$smd,
    After_Treat = after$treat,
    After_Control = after$control,
    After_SMD = after$smd,
    stringsAsFactors = FALSE
  )

  before_smd_num <- suppressWarnings(as.numeric(tbl$Before_SMD))
  after_smd_num <- suppressWarnings(as.numeric(tbl$After_SMD))

  main_var_rows <- !grepl("^  ", tbl$Characteristic) & tbl$Before_Treat != ""

  tbl %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md("**Baseline Characteristics -- Before vs. After MOBACO**"),
      subtitle = gt::md(sprintf("Solution: %s", solution_label))
    ) %>%
    gt::cols_label(
      Characteristic = gt::md("**Characteristic**"),
      Before_Treat = gt::md(sprintf("**Treatment**<br>N = %d", before$n_treat)),
      Before_Control = gt::md(sprintf("**Control**<br>N = %d", before$n_control)),
      Before_SMD = gt::md("**SMD**"),
      After_Treat = gt::md(sprintf("**Treatment**<br>N = %d", after$n_treat)),
      After_Control = gt::md(sprintf("**Control**<br>N = %d", after$n_control)),
      After_SMD = gt::md("**SMD**")
    ) %>%
    gt::tab_spanner(
      label = gt::md(sprintf("**Before MOBACO**<br>Total N = %d", nrow(data_before))),
      columns = c(Before_Treat, Before_Control, Before_SMD)
    ) %>%
    gt::tab_spanner(
      label = gt::md(sprintf("**After MOBACO**<br>Total N = %d", nrow(data_mobaco))),
      columns = c(After_Treat, After_Control, After_SMD)
    ) %>%
    # SMD colours: before
    gt::tab_style(
      style = gt::cell_text(color = "red", weight = "bold"),
      locations = gt::cells_body(columns = Before_SMD,
                                 rows = !is.na(before_smd_num) & before_smd_num > 0.1)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "darkgreen", weight = "bold"),
      locations = gt::cells_body(columns = Before_SMD,
                                 rows = !is.na(before_smd_num) & before_smd_num <= 0.1)
    ) %>%
    # SMD colours: after
    gt::tab_style(
      style = gt::cell_text(color = "red", weight = "bold"),
      locations = gt::cells_body(columns = After_SMD,
                                 rows = !is.na(after_smd_num) & after_smd_num > 0.1)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "darkgreen", weight = "bold"),
      locations = gt::cells_body(columns = After_SMD,
                                 rows = !is.na(after_smd_num) & after_smd_num <= 0.1)
    ) %>%
    # Subzeilen grau + kursiv
    gt::tab_style(
      style = gt::cell_text(color = "gray40", style = "italic"),
      locations = gt::cells_body(columns = Characteristic,
                                 rows = grepl("^  ", Characteristic))
    ) %>%
    # Kategoriezeilen fett
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = Characteristic,
                                 rows = Before_Treat == "")
    ) %>%
    # Separating rules between covariates, except above the first row
    gt::tab_style(
      style = gt::cell_borders(sides = "top", color = "#CCCCCC", weight = gt::px(1)),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = main_var_rows & seq_len(nrow(tbl)) > 1
      )
    ) %>%
    # Spanner Hintergrund
    gt::tab_style(
      style = gt::cell_fill(color = "#2C3E50"),
      locations = gt::cells_column_spanners()
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "white", weight = "bold"),
      locations = gt::cells_column_spanners()
    ) %>%
    # Spaltenheader
    gt::tab_style(
      style = gt::cell_fill(color = "#34495E"),
      locations = gt::cells_column_labels(gt::everything())
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "white", weight = "bold"),
      locations = gt::cells_column_labels(gt::everything())
    ) %>%
    # Vertikale schwarze Linien (statt rot)
    gt::tab_style(
      style = gt::cell_borders(sides = "left", color = "#34495E", weight = gt::px(2)),
      locations = gt::cells_body(columns = After_Treat)
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "left", color = "#34495E", weight = gt::px(2)),
      locations = gt::cells_column_labels(columns = After_Treat)
    ) %>%
    # Footnote
    gt::tab_footnote(
      footnote = paste(
        "Continuous: mean (SD). Categorical: n (%).",
        "SMD = Standardized Mean Difference.",
        "Red: SMD > 0.1 (imbalanced). Green: SMD <= 0.1 (balanced).",
        "MOBACO: Multi-Objective Balance and Coverage Optimization (BOSS-B)."
      ),
      locations = gt::cells_column_labels(columns = Before_SMD)
    ) %>%
    gt::tab_options(
      table.font.size = 12,
      table.width = gt::pct(100),
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "#F8F9FA"
    )
}

#' Build 3-way comparison table
#' @keywords internal
build_3way_table <- function(data_before, data_mobaco, data_psm,
                             treatment_var, treatment_level, covariates,
                             level_structure, solution_label,
                             psm_caliper = 0.2) {

  before <- get_table_cols(data_before, treatment_var, treatment_level,
                           covariates, level_structure)
  mobaco <- get_table_cols(data_mobaco, treatment_var, treatment_level,
                           covariates, level_structure)
  psm <- get_table_cols(data_psm, treatment_var, treatment_level,
                        covariates, level_structure)

  tbl <- data.frame(
    Characteristic = before$characteristics,
    Before_Treat = before$treat,
    Before_Control = before$control,
    Before_SMD = before$smd,
    MOBACO_Treat = mobaco$treat,
    MOBACO_Control = mobaco$control,
    MOBACO_SMD = mobaco$smd,
    PSM_Treat = psm$treat,
    PSM_Control = psm$control,
    PSM_SMD = psm$smd,
    stringsAsFactors = FALSE
  )

  before_smd_num <- suppressWarnings(as.numeric(tbl$Before_SMD))
  mobaco_smd_num <- suppressWarnings(as.numeric(tbl$MOBACO_SMD))
  psm_smd_num <- suppressWarnings(as.numeric(tbl$PSM_SMD))

  # Identifiziere Hauptvariablen
  main_var_rows <- !grepl("^  ", tbl$Characteristic) & tbl$Before_Treat != ""

  tbl %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md("**Baseline Characteristics -- MOBACO vs. PSM Comparison**"),
      subtitle = gt::md(sprintf("MOBACO Solution: %s | PSM Caliper: %s SD",
                                solution_label, psm_caliper))
    ) %>%
    gt::cols_label(
      Characteristic = gt::md("**Characteristic**"),
      Before_Treat = gt::md(sprintf("**Treat**<br>N = %d", before$n_treat)),
      Before_Control = gt::md(sprintf("**Control**<br>N = %d", before$n_control)),
      Before_SMD = gt::md("**SMD**"),
      MOBACO_Treat = gt::md(sprintf("**Treat**<br>N = %d", mobaco$n_treat)),
      MOBACO_Control = gt::md(sprintf("**Control**<br>N = %d", mobaco$n_control)),
      MOBACO_SMD = gt::md("**SMD**"),
      PSM_Treat = gt::md(sprintf("**Treat**<br>N = %d", psm$n_treat)),
      PSM_Control = gt::md(sprintf("**Control**<br>N = %d", psm$n_control)),
      PSM_SMD = gt::md("**SMD**")
    ) %>%
    gt::tab_spanner(
      label = gt::md(sprintf("**Before**<br>N = %d", nrow(data_before))),
      columns = c(Before_Treat, Before_Control, Before_SMD)
    ) %>%
    gt::tab_spanner(
      label = gt::md(sprintf("**After MOBACO**<br>N = %d", nrow(data_mobaco))),
      columns = c(MOBACO_Treat, MOBACO_Control, MOBACO_SMD)
    ) %>%
    gt::tab_spanner(
      label = gt::md(sprintf("**After PSM**<br>N = %d", nrow(data_psm))),
      columns = c(PSM_Treat, PSM_Control, PSM_SMD)
    ) %>%
    # SMD colours (before, MOBACO, PSM)
    gt::tab_style(
      style = gt::cell_text(color = "red", weight = "bold"),
      locations = gt::cells_body(columns = Before_SMD,
                                 rows = !is.na(before_smd_num) & before_smd_num > 0.1)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "darkgreen", weight = "bold"),
      locations = gt::cells_body(columns = Before_SMD,
                                 rows = !is.na(before_smd_num) & before_smd_num <= 0.1)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "red", weight = "bold"),
      locations = gt::cells_body(columns = MOBACO_SMD,
                                 rows = !is.na(mobaco_smd_num) & mobaco_smd_num > 0.1)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "darkgreen", weight = "bold"),
      locations = gt::cells_body(columns = MOBACO_SMD,
                                 rows = !is.na(mobaco_smd_num) & mobaco_smd_num <= 0.1)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "red", weight = "bold"),
      locations = gt::cells_body(columns = PSM_SMD,
                                 rows = !is.na(psm_smd_num) & psm_smd_num > 0.1)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "darkgreen", weight = "bold"),
      locations = gt::cells_body(columns = PSM_SMD,
                                 rows = !is.na(psm_smd_num) & psm_smd_num <= 0.1)
    ) %>%
    # Subzeilen grau + kursiv
    gt::tab_style(
      style = gt::cell_text(color = "gray40", style = "italic"),
      locations = gt::cells_body(columns = Characteristic,
                                 rows = grepl("^  ", Characteristic))
    ) %>%
    # Kategoriezeilen fett
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = Characteristic,
                                 rows = Before_Treat == "")
    ) %>%
    # Separating rules between covariates
    gt::tab_style(
      style = gt::cell_borders(sides = "top", color = "#CCCCCC", weight = gt::px(1)),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = main_var_rows & seq_len(nrow(tbl)) > 1
      )
    ) %>%
    # Spanner Hintergrund
    gt::tab_style(
      style = gt::cell_fill(color = "#2C3E50"),
      locations = gt::cells_column_spanners()
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "white", weight = "bold"),
      locations = gt::cells_column_spanners()
    ) %>%
    # Spaltenheader
    gt::tab_style(
      style = gt::cell_fill(color = "#34495E"),
      locations = gt::cells_column_labels(gt::everything())
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "white", weight = "bold"),
      locations = gt::cells_column_labels(gt::everything())
    ) %>%
    # Vertikale schwarze Linien (statt farbig)
    gt::tab_style(
      style = gt::cell_borders(sides = "left", color = "#34495E", weight = gt::px(2)),
      locations = gt::cells_body(columns = MOBACO_Treat)
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "left", color = "#34495E", weight = gt::px(2)),
      locations = gt::cells_body(columns = PSM_Treat)
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "left", color = "#34495E", weight = gt::px(2)),
      locations = gt::cells_column_labels(columns = MOBACO_Treat)
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "left", color = "#34495E", weight = gt::px(2)),
      locations = gt::cells_column_labels(columns = PSM_Treat)
    ) %>%
    # Footnote
    gt::tab_footnote(
      footnote = paste(
        "Continuous: mean (SD). Categorical: n (%).",
        "SMD = Standardized Mean Difference.",
        "Red: SMD > 0.1 (imbalanced). Green: SMD <= 0.1 (balanced).",
        "MOBACO: Multi-Objective Balance and Coverage Optimization (BOSS-B).",
        sprintf("PSM: MatchIt package, nearest neighbor, caliper = %s.", psm_caliper)
      ),
      locations = gt::cells_column_labels(columns = Before_SMD)
    ) %>%
    gt::tab_options(
      table.font.size = 11,
      table.width = gt::pct(100),
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "#F8F9FA"
    )
}
