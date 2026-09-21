# ============================================================
# MOBACO Shiny App - Server Logic
# ============================================================

server <- function(input, output, session) {

  # ============================================
  # Reactive Values
  # ============================================

  rv <- reactiveValues(
    data          = NULL,   # current dataset, after any type corrections
    data_original = NULL,   # the uploaded data, unmodified
    col_types     = NULL,   # named list: column -> requested type
    result        = NULL,
    variables_confirmed    = FALSE,
    optimization_complete  = FALSE,
    start_time    = NULL,
    end_time      = NULL
  )

  # ============================================
  # Helper: type string -> R type
  # ============================================

  apply_col_type <- function(col, type_str) {
    tryCatch({
      switch(type_str,
             "numeric"          = as.numeric(col),
             "integer"          = as.integer(col),
             "factor"           = as.factor(col),
             "ordered factor"   = as.ordered(as.factor(col)),
             "logical"          = as.logical(col),
             "character"        = as.character(col),
             col  # fallback: leave unchanged
      )
    }, error = function(e) col)
  }

  # Suggested type for a column, using the same rule as the analysis code:
  # numeric with more than ten distinct values is treated as continuous and
  # binned into intervals, fewer as discrete levels. Getting this wrong is
  # consequential — age read as a factor yields 47 bins instead of 5.
  # Only the distinction that changes the result is flagged. Whether a
  # numeric column is stored as integer or double makes no difference:
  # is.numeric() is TRUE for both, and the binning depends solely on the
  # number of distinct values.
  col_type_suggest <- function(col) {
    k <- length(unique(stats::na.omit(col)))
    if (is.character(col) || is.factor(col)) return(NULL)
    if (is.logical(col)) return(NULL)
    if (is.numeric(col)) {
      if (k == 2)  return("factor")
      if (k <= 10) return("ordered factor")
      return(NULL)                       # continuous either way
    }
    NULL
  }

  # Columns whose role the data cannot settle. A numeric column holding
  # nearly as many distinct values as there are rows is an identifier or a
  # measured quantity such as an outcome, not a grouping covariate. A raw
  # count would depend on cohort size and flag the harmless case: age takes
  # 62 distinct values among 500 patients and is plainly a covariate. The
  # share of distinct values decides instead.
  col_needs_review <- function(col) {
    v <- stats::na.omit(col)
    if (!is.numeric(col) || length(v) == 0L) return(FALSE)
    k <- length(unique(v))
    k > 50L && k / length(v) > 0.5
  }

  # Readable type label for a column
  col_type_label <- function(col) {
    if (is.ordered(col))   return("ordered factor")
    if (is.factor(col))    return("factor")
    if (is.logical(col))   return("logical")
    if (is.integer(col))   return("integer")
    if (is.numeric(col))   return("numeric")
    if (is.character(col)) return("character")
    return("character")
  }

  # ============================================
  # Data loading - shared post-processing
  # ============================================

  init_data <- function(df) {
    names(df)     <- make.names(names(df))
    rv$data          <- df
    rv$data_original <- df
    # Initial type assignment from the actual column types
    rv$col_types <- setNames(
      lapply(df, col_type_label),
      names(df)
    )
    rv$variables_confirmed   <- FALSE
    rv$optimization_complete <- FALSE

    # Preselect the treatment variable when one can be identified; the
    # example cohort is then ready to run without manual setup. Detection
    # relies on name patterns and fails for data-specific names such as
    # "qsmk". The fallback is the first binary column, not the first
    # column, which would often be an identifier.
    detected <- tryCatch(MOBACO:::auto_detect_treatment(df),
                         error = function(e) NULL)
    if (is.null(detected)) {
      binary <- names(df)[vapply(df, function(x)
        length(unique(stats::na.omit(x))) == 2L, logical(1))]
      detected <- if (length(binary)) binary[1] else names(df)[1]
    }
    updateSelectInput(session, "treatment_var", choices = names(df),
                      selected = detected)
  }

  # ============================================
  # Data Upload
  # ============================================

  observeEvent(input$data_file, {
    req(input$data_file)
    tryCatch({
      df <- read_data_local(input$data_file$datapath)
      init_data(df)
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })

  # ============================================
  # Example Data
  # ============================================

  observeEvent(input$load_example, {

    # A simulated cohort: nine covariates, four of them confounders, group
    # assignment through a logistic model, modelled on the unbalanced group
    # sizes of a clinical cohort. Generated by data-raw/make_example_cohort.R
    # from a single seed, which makes it reproducible and freely
    # distributable. Located either in the installed package or, when the app
    # runs straight from inst/shiny/, one directory up.
    f <- system.file("extdata", "example_cohort.rds", package = "MOBACO")
    if (!nzchar(f) || !file.exists(f)) {
      f <- file.path("..", "extdata", "example_cohort.rds")
    }
    if (!file.exists(f)) {
      showNotification(
        "Example data not found. Expected inst/extdata/example_cohort.rds",
        type = "error", duration = 10
      )
      return(invisible(NULL))
    }

    df <- readRDS(f)

    # Character columns become factors; the app then treats them as
    # categorical. Stage keeps its four ordered levels and is not cut into
    # intervals.
    chr <- vapply(df, is.character, logical(1))
    df[chr] <- lapply(df[chr], factor)
    df$prognosis_group <- factor(df$prognosis_group, levels = c("good", "poor"))
    df$stage <- factor(df$stage, ordered = TRUE)

    init_data(df)

    pr <- attr(df, "params")
    showNotification(
      sprintf(paste("Example cohort loaded — the base scenario of the simulation",
                    "study. n = %d units, %d in the group of interest (%.0f%%),",
                    "%d covariates, %s of them confounders."),
              nrow(df), sum(df$prognosis_group == "poor"),
              100 * mean(df$prognosis_group == "poor"), ncol(df) - 1L,
              if (is.null(pr$n_confounders)) "several" else pr$n_confounders),
      type = "message", duration = 8
    )
  })

  # ============================================
  # Data preparation: UI for editing column types
  # ============================================

  output$type_editor_ui <- renderUI({
    req(rv$data, rv$col_types)

    # "integer" is deliberately absent: for the balance objective it behaves
    # exactly like "numeric" (is.numeric() is TRUE for both, and binning
    # depends only on the number of distinct values), while as.integer()
    # truncates decimals and would silently alter the data.
    type_choices <- c("numeric", "factor", "ordered factor", "logical", "character")

    rows <- lapply(names(rv$data), function(col) {
      current_type  <- rv$col_types[[col]]
      current_label <- col_type_label(rv$data[[col]])

      # Badge colour
      badge_class <- switch(current_label,
                            "numeric"        = "type-badge type-numeric",
                            "integer"        = "type-badge type-integer",
                            "factor"         = "type-badge type-factor",
                            "ordered factor" = "type-badge type-ordered",
                            "logical"        = "type-badge type-logical",
                            "character"      = "type-badge type-character",
                            "type-badge type-character"
      )

      suggested <- col_type_suggest(rv$data[[col]])
      hint <- if (!is.null(suggested) && !identical(suggested, current_label)) {
        tags$div(style = "font-size: 11px; color: #B9770E; margin-top: 3px;",
                 sprintf("suggested: %s", suggested))
      } else NULL
      review <- if (col_needs_review(rv$data[[col]])) {
        tags$div(style = "font-size: 11px; color: #922B21; margin-top: 3px;",
                 sprintf("%d distinct values \u2014 confirm this is a pre-treatment covariate",
                         length(unique(stats::na.omit(rv$data[[col]])))))
      } else NULL

      # The grouping variable is the one decision the data cannot make, and
      # it should not look like any other column.
      treat <- if (identical(col, input$treatment_var)) {
        tags$div(style = paste("font-size: 11px; color: #1F618D;",
                               "margin-top: 3px; font-weight: 600;"),
                 "treatment variable \u2014 defines the two groups")
      } else NULL

      tags$tr(
        tags$td(style = "font-weight: 600; font-size: 13px; width: 35%;",
                col, treat, hint, review),
        tags$td(style = "width: 25%;",
                tags$span(class = badge_class, current_label)
        ),
        tags$td(style = "width: 40%;",
                tags$select(
                  id    = paste0("type_", col),
                  class = "form-control input-sm",
                  style = "font-size: 13px; padding: 3px 6px; height: 30px;",
                  lapply(type_choices, function(tc) {
                    if (tc == current_type) {
                      tags$option(value = tc, selected = "selected", tc)
                    } else {
                      tags$option(value = tc, tc)
                    }
                  })
                )
        )
      )
    })

    tags$table(
      class = "prep-table",
      tags$thead(
        tags$tr(
          tags$th("Column"),
          tags$th("Current Type"),
          tags$th("Change To")
        )
      ),
      tags$tbody(rows)
    )
  })

  # ============================================
  # Apply Type Changes
  # ============================================

  observeEvent(input$apply_types, {
    req(rv$data)

    df <- rv$data

    changed <- 0
    errors  <- character(0)

    for (col in names(df)) {
      input_id  <- paste0("type_", col)
      new_type  <- input[[input_id]]

      if (!is.null(new_type) && nchar(new_type) > 0) {
        old_type <- col_type_label(df[[col]])

        if (new_type != old_type) {
          converted <- apply_col_type(df[[col]], new_type)

          # Check whether the conversion was meaningful
          if (all(is.na(converted)) && !all(is.na(df[[col]]))) {
            errors <- c(errors, col)
          } else {
            df[[col]]          <- converted
            rv$col_types[[col]] <- new_type
            changed            <- changed + 1
          }
        }
      }
    }

    rv$data <- df

    # Refresh the covariate selection
    covars <- setdiff(names(rv$data), input$treatment_var)
    session$sendCustomMessage("updateNativeSelect", list(id = "covariates", choices = covars))

    if (length(errors) > 0) {
      showNotification(
        paste("Could not convert (all NA):", paste(errors, collapse = ", ")),
        type = "warning", duration = 8
      )
    }

    if (changed > 0) {
      showNotification(
        paste0(changed, " column(s) converted successfully!"),
        type = "message", duration = 5
      )
    } else if (length(errors) == 0) {
      showNotification("No changes detected.", type = "message", duration = 3)
    }
  })

  # ============================================
  # Reset to Original
  # ============================================

  observeEvent(input$reset_types, {
    req(rv$data_original)

    rv$data      <- rv$data_original
    rv$col_types <- setNames(
      lapply(rv$data_original, col_type_label),
      names(rv$data_original)
    )

    showNotification("Reset to original column types.", type = "message", duration = 4)
  })

  # ============================================
  # Type Summary
  # ============================================

  output$type_summary_ui <- renderUI({
    req(rv$data)

    type_counts <- table(sapply(rv$data, col_type_label))

    color_map <- c(
      "numeric"        = "#3498DB",
      "integer"        = "#1ABC9C",
      "factor"         = "#E67E22",
      "ordered factor" = "#E74C3C",
      "logical"        = "#9B59B6",
      "character"      = "#95A5A6"
    )

    badges <- lapply(names(type_counts), function(tp) {
      col <- if (!is.na(color_map[tp])) color_map[tp] else "#95A5A6"
      tags$span(
        style = sprintf(
          "display: inline-block; background: %s; color: white;
           padding: 6px 14px; border-radius: 20px; margin: 4px;
           font-weight: 700; font-size: 14px;", col
        ),
        sprintf("%s: %d", tp, type_counts[tp])
      )
    })

    tagList(
      div(style = "margin-bottom: 10px;", badges),
      tags$small(
        style = "color: #7F8C8D;",
        sprintf("Total: %d columns, %d rows", ncol(rv$data), nrow(rv$data))
      )
    )
  })

  # Prep Data Preview
  output$prep_data_preview <- renderDT({
    req(rv$data)
    datatable(
      head(rv$data, 100),
      options = list(pageLength = 8, scrollX = TRUE)
    )
  })

  # ============================================
  # Data Loaded Indicator
  # ============================================

  output$data_loaded <- reactive({ !is.null(rv$data) })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  output$data_summary <- renderPrint({
    req(rv$data)
    cat(sprintf("Rows: %d\n", nrow(rv$data)))
    cat(sprintf("Columns: %d\n", ncol(rv$data)))
    cat("\nColumn types:\n")
    str(rv$data, give.attr = FALSE)
  })

  output$data_preview <- renderDT({
    req(rv$data)
    datatable(head(rv$data, 100), options = list(pageLength = 10, scrollX = TRUE))
  })

  # ============================================
  # Variable Selection
  # ============================================

  output$treatment_level_ui <- renderUI({
    req(rv$data, input$treatment_var)
    levels <- unique(rv$data[[input$treatment_var]])
    # Preselect the level denoting the group of interest; for the example
    # cohort this is "poor", the smaller of the two groups. The helper is
    # internal to the package and must be qualified: the app attaches MOBACO
    # with library(), which brings only the exported functions into scope.
    sel <- tryCatch(MOBACO:::auto_detect_treatment_level(rv$data,
                                                         input$treatment_var),
                    error = function(e) NULL)
    selectInput("treatment_level", "Select Treatment Level", choices = levels,
                selected = if (!is.null(sel) && sel %in% levels) sel else levels[1])
  })

  observe({
    req(rv$data, input$treatment_var)
    covars <- setdiff(names(rv$data), input$treatment_var)
    session$sendCustomMessage("updateNativeSelect", list(id = "covariates", choices = covars))
  })

  observeEvent(input$confirm_variables, {
    req(rv$data, input$treatment_var, input$treatment_level)

    if (!input$auto_covariates) {
      if (is.null(input$covariates) || length(input$covariates) == 0) {
        showNotification(
          "Please select at least one covariate, or enable Auto-detect.",
          type = "warning"
        )
        return()
      }
    }

    rv$variables_confirmed   <- TRUE
    rv$optimization_complete <- FALSE
    showNotification("Variables confirmed!", type = "message")
  })

  output$variables_confirmed <- reactive({ rv$variables_confirmed })
  outputOptions(output, "variables_confirmed", suspendWhenHidden = FALSE)

  output$variable_summary <- renderPrint({
    req(rv$variables_confirmed)
    cat("Treatment Variable:", input$treatment_var, "\n")
    cat("Treatment Level:", input$treatment_level, "\n\n")
    if (input$auto_covariates) {
      covars <- setdiff(names(rv$data), input$treatment_var)
      cat("Covariates (auto-detected):\n")
    } else {
      covars <- input$covariates
      cat("Covariates (selected):\n")
    }
    cat(paste(" -", covars, collapse = "\n"))
  })

  output$group_size_plot <- renderPlot({
    req(rv$variables_confirmed)
    counts <- table(rv$data[[input$treatment_var]])
    barplot(counts, col = c(colors$primary, colors$success),
            main = "Group Sizes", ylab = "Count", las = 1, border = NA)
  })

  # ============================================
  # Run Optimization
  # ============================================

  observeEvent(input$run_mobaco, {
    req(rv$variables_confirmed)

    rv$optimization_complete <- FALSE
    rv$start_time <- NULL
    rv$end_time   <- NULL

    covars <- if (input$auto_covariates) NULL else input$covariates
    min_pg <- if (is.na(input$min_per_group)) NULL else input$min_per_group

    withProgress(message = "Running MOBACO Optimization", value = 0, {

      rv$start_time <- Sys.time()

      setProgress(0.05, detail = "Initializing data preparation...")
      Sys.sleep(0.2)
      setProgress(0.10, detail = "Preparing bins & fitness function...")
      Sys.sleep(0.2)
      setProgress(0.15, detail = sprintf(
        "Launching NSGA-II: %s evaluations (%d pop x %d gen)",
        format(input$popsize * input$generations, big.mark = ","),
        input$popsize, input$generations
      ))

      tryCatch({
        rv$result <- mobaco(
          data            = rv$data,
          treatment_var   = input$treatment_var,
          treatment_level = input$treatment_level,
          covariates      = covars,
          popsize         = input$popsize,
          generations     = input$generations,
          crossover_prob  = input$crossover_prob,
          mutation_prob   = input$mutation_prob,
          crossover_dist  = input$crossover_dist,
          mutation_dist   = input$mutation_dist,
          n_bins          = input$n_bins,
          min_per_group   = min_pg,
          normalization   = input$normalization,
          run_psm         = input$run_psm,
          psm_caliper     = input$psm_caliper,
          verbose         = FALSE
        )

        setProgress(0.95, detail = "Processing results...")
        Sys.sleep(0.3)
        setProgress(1.0, detail = "Complete!")

        rv$end_time              <- Sys.time()
        rv$optimization_complete <- TRUE

        session$sendCustomMessage("stopTimer", list())

        runtime <- as.numeric(difftime(rv$end_time, rv$start_time, units = "secs"))
        showNotification(
          sprintf("Optimization complete! Runtime: %s", format_duration(runtime)),
          type = "message", duration = 10
        )

      }, error = function(e) {
        session$sendCustomMessage("stopTimer", list())
        showNotification(paste("Error:", e$message), type = "error", duration = NULL)
      })
    })
  })

  output$optimization_complete <- reactive({ rv$optimization_complete })
  outputOptions(output, "optimization_complete", suspendWhenHidden = FALSE)

  output$total_runtime <- renderText({
    req(rv$optimization_complete, rv$start_time, rv$end_time)
    runtime <- as.numeric(difftime(rv$end_time, rv$start_time, units = "secs"))
    format_duration(runtime)
  })

  output$optimization_log <- renderPrint({
    req(input$run_mobaco > 0)
    if (rv$optimization_complete && !is.null(rv$result)) {
      cat("✓ Optimization completed successfully!\n\n")
      cat("Configuration:\n")
      # from the result object, not from the input: the sliders may have been
      # moved since the run finished
      cat(sprintf("  Balance Metric:    %s\n", rv$result$normalization))
      cat(sprintf("  Population Size:   %d\n", input$popsize))
      cat(sprintf("  Generations:       %d\n", input$generations))
      cat(sprintf("  Total Evaluations: %s\n",
                  format(input$popsize * input$generations, big.mark = ",")))
      cat("\nResults:\n")
      cat(sprintf("  Pareto Solutions: %d\n", nrow(rv$result$pareto_front)))
      cat(sprintf("  Best Balance:     %.4f\n", min(rv$result$pareto_front$Balance)))
      cat(sprintf("  Best Coverage:    %.4f\n", max(rv$result$pareto_front$Coverage)))
    } else {
      cat("Preparing to run optimization...\n")
      cat(sprintf("Population: %d\n", input$popsize))
      cat(sprintf("Generations: %d\n", input$generations))
      cat(sprintf("Estimated evaluations: %s\n",
                  format(input$popsize * input$generations, big.mark = ",")))
    }
  })

  # ============================================
  # Results
  # ============================================

  output$results_summary <- renderPrint({
    req(rv$result)
    summary(rv$result)
  })

  output$pareto_plot_container <- renderUI({
    req(rv$result)
    if (has_plotly) {
      plotly::plotlyOutput("pareto_plot", height = "550px")
    } else {
      plotOutput("pareto_plot_base", height = "550px")
    }
  })

  output$pareto_plot <- plotly::renderPlotly({
    req(rv$result, has_plotly)
    plot(rv$result, interactive = TRUE)
  })

  output$pareto_plot_base <- renderPlot({
    req(rv$result)
    plot(rv$result, interactive = FALSE)
  })

  output$has_plotly <- reactive({ has_plotly })
  outputOptions(output, "has_plotly", suspendWhenHidden = FALSE)

  output$solution_preview <- renderDT({
    req(rv$result, input$solution_type)
    data_selected <- extract_solution(rv$result, input$solution_type)
    datatable(head(data_selected, 100), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$download_selected <- downloadHandler(
    filename = function() paste0("mobaco_", input$solution_type, "_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(extract_solution(rv$result, input$solution_type), file, row.names = FALSE)
    }
  )

  output$download_all <- downloadHandler(
    filename = function() paste0("mobaco_all_solutions_", Sys.Date(), ".zip"),
    content  = function(file) {
      tmpdir <- tempdir()
      for (sol in c("min_balance", "knee", "max_coverage")) {
        write.csv(extract_solution(rv$result, sol),
                  file.path(tmpdir, paste0(sol, ".csv")), row.names = FALSE)
      }
      write.csv(rv$result$pareto_front,
                file.path(tmpdir, "pareto_front.csv"), row.names = FALSE)
      files_to_zip <- list.files(tmpdir, pattern = "\\.csv$", full.names = TRUE)
      utils::zip(file, files = files_to_zip, flags = "-j")
    }
  )

  # ============================================
  # Comparison Table
  # ============================================

  output$has_gt <- reactive({ has_gt })
  outputOptions(output, "has_gt", suspendWhenHidden = FALSE)

  output$comparison_table_gt <- renderUI({
    req(rv$result, input$table_solution)
    if (has_gt) {
      tbl <- comparison_table(rv$result,
                              solution    = input$table_solution,
                              psm_caliper = input$psm_caliper)

      # Insert the caliper value into title and subtitle
      tbl <- tbl |>
        gt::tab_header(
          title    = "Baseline Characteristics \u2013 MOBACO vs. PSM Comparison",
          subtitle = sprintf(
            "MOBACO Solution: %s | PSM Caliper: %s SD",
            gsub("_", " ", input$table_solution),
            input$psm_caliper
          )
        )

      gt::as_raw_html(tbl)
    }
  })

  output$comparison_table_dt <- renderDT({
    req(rv$result, input$table_solution)
    data_after <- extract_solution(rv$result, input$table_solution)
    datatable(head(data_after, 100), options = list(pageLength = 10, scrollX = TRUE))
  })
}
