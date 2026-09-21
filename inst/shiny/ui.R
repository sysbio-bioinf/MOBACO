# MOBACO Shiny App - User Interface (Optimiert)

ui <- fluidPage(

  # Custom CSS
  tags$head(
    tags$style(HTML("
/* Fade-in animation on load */
.welcome-banner {
  animation: fadeInUp 0.8s ease-out;
}
.well {
  animation: fadeIn 1s ease-out;
}
@keyframes fadeInUp {
  from { opacity: 0; transform: translateY(30px); }
  to   { opacity: 1; transform: translateY(0); }
}
@keyframes fadeIn {
  from { opacity: 0; }
  to   { opacity: 1; }
}

/* Body & General */
body {
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  background-color: #F5F7FA;
  margin: 0;
  padding: 0;
}

/* NAVBAR */
.navbar {
  background: linear-gradient(135deg, #3498DB 0%, #2874A6 100%) !important;
  box-shadow: 0 4px 20px rgba(52,152,219,0.3);
  border: none;
  min-height: 70px !important;
  padding: 0 30px !important;
}
.navbar-header {
  height: 70px !important;
  display: flex;
  align-items: center;
}
.navbar-brand {
  color: white !important;
  font-size: 32px !important;
  font-weight: 800;
  letter-spacing: 3px;
  padding: 20px 15px !important;
  height: 70px !important;
  line-height: 30px !important;
  text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
}
.navbar-logo-right {
  position: absolute;
  right: 30px;
  top: 10px;
  height: 50px;
  width: auto;
  filter: drop-shadow(2px 2px 4px rgba(0,0,0,0.2));
}
.navbar-nav > li > a {
  color: rgba(255,255,255,0.9) !important;
  font-size: 15px !important;
  font-weight: 600;
  padding: 25px 18px !important;
  transition: all 0.3s;
  border-bottom: 3px solid transparent;
}
.navbar-nav > li > a:hover {
  color: white !important;
  background-color: rgba(255,255,255,0.15) !important;
  border-bottom: 3px solid #1ABC9C;
}
.navbar-nav > li.active > a {
  background-color: rgba(255,255,255,0.2) !important;
  color: white !important;
  border-bottom: 3px solid #1ABC9C !important;
  font-weight: 700;
}

/* WELCOME BANNER */
.welcome-banner {
  background: linear-gradient(135deg, #3498DB 0%, #5DADE2 40%, #1ABC9C 100%);
  padding: 50px 40px;
  color: white;
  border-radius: 15px;
  margin-bottom: 35px;
  text-align: center;
  box-shadow: 0 8px 30px rgba(52,152,219,0.4);
  position: relative;
  overflow: hidden;
}
.welcome-banner::before {
  content: '';
  position: absolute;
  top: -50%; left: -50%;
  width: 200%; height: 200%;
  background: radial-gradient(circle, rgba(255,255,255,0.1) 0%, transparent 70%);
  animation: pulse 8s ease-in-out infinite;
}
@keyframes pulse {
  0%, 100% { transform: translate(0,0) scale(1); opacity: 0.3; }
  50%       { transform: translate(10%,10%) scale(1.1); opacity: 0.5; }
}
.welcome-banner h2 {
  margin: 0;
  font-weight: 800;
  font-size: 42px;
  text-shadow: 3px 3px 6px rgba(0,0,0,0.3);
  position: relative; z-index: 1;
  letter-spacing: 2px;
}
.welcome-banner p {
  font-size: 22px;
  margin-top: 18px;
  opacity: 0.95;
  position: relative; z-index: 1;
  font-weight: 500;
  text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
}

/* BUTTONS */
.btn-primary {
  background: linear-gradient(135deg, #3498DB 0%, #2874A6 100%);
  border: none;
  box-shadow: 0 4px 12px rgba(52,152,219,0.4);
  transition: all 0.3s;
  font-weight: 700;
  color: white;
}
.btn-primary:hover {
  transform: translateY(-3px);
  box-shadow: 0 6px 20px rgba(52,152,219,0.5);
  background: linear-gradient(135deg, #2874A6 0%, #3498DB 100%);
}
.btn-success {
  background: linear-gradient(135deg, #1ABC9C 0%, #16A085 100%);
  border: none;
  box-shadow: 0 4px 12px rgba(26,188,156,0.4);
  transition: all 0.3s;
  font-weight: 700;
  color: white;
}
.btn-success:hover {
  transform: translateY(-3px);
  box-shadow: 0 6px 20px rgba(26,188,156,0.5);
}
.btn-info {
  background: linear-gradient(135deg, #5DADE2 0%, #3498DB 100%);
  border: none;
  font-weight: 700;
  color: white;
  box-shadow: 0 3px 10px rgba(93,173,226,0.3);
}
.btn-info:hover {
  transform: translateY(-2px);
  box-shadow: 0 5px 15px rgba(93,173,226,0.4);
}
.btn-warning {
  background: linear-gradient(135deg, #F39C12 0%, #D68910 100%);
  border: none;
  font-weight: 700;
  color: white;
  box-shadow: 0 3px 10px rgba(243,156,18,0.3);
}
.btn-warning:hover {
  transform: translateY(-2px);
  box-shadow: 0 5px 15px rgba(243,156,18,0.4);
  color: white;
}

/* WELLPANELS */
.well {
  background-color: #FFFFFF;
  border: 1px solid #E8EAED;
  border-radius: 12px;
  box-shadow: 0 3px 15px rgba(0,0,0,0.08);
  padding: 30px;
  transition: all 0.3s;
}
.well:hover {
  box-shadow: 0 5px 25px rgba(0,0,0,0.12);
  transform: translateY(-2px);
}
h4 {
  color: #2C3E50;
  font-weight: 700;
  margin-bottom: 20px;
  padding-bottom: 15px;
  border-bottom: 3px solid #3498DB;
  font-size: 20px;
}
h4 i {
  color: #3498DB;
  margin-right: 12px;
  font-size: 22px;
}

/* ALERTS */
.alert-success {
  background: linear-gradient(135deg, #D5F4E6 0%, #A9DFBF 100%);
  border: 2px solid #1ABC9C;
  color: #1E8449;
  border-radius: 10px;
  padding: 25px;
  box-shadow: 0 3px 12px rgba(26,188,156,0.2);
}
.alert-info {
  background: linear-gradient(135deg, #EBF5FB 0%, #D6EAF8 100%);
  border: 2px solid #3498DB;
  color: #1F618D;
  border-radius: 10px;
  padding: 20px;
}

/* PROGRESS */
.progress {
  height: 28px;
  border-radius: 14px;
  background-color: #ECF0F1;
  box-shadow: inset 0 2px 4px rgba(0,0,0,0.1);
}
.progress-bar {
  background: linear-gradient(90deg, #3498DB 0%, #1ABC9C 100%);
  border-radius: 14px;
  box-shadow: 0 2px 8px rgba(52,152,219,0.4);
}

/* DATATABLES */
.dataTables_wrapper { padding: 20px; }
table.dataTable thead th {
  background: linear-gradient(135deg, #3498DB 0%, #2874A6 100%);
  color: white;
  font-weight: 700;
  border: none;
  padding: 15px;
}
table.dataTable tbody tr:hover {
  background-color: #EBF5FB !important;
}
table.dataTable tbody tr { transition: all 0.2s; }

/* INPUT FIELDS */
.form-control, .selectize-input {
  border: 2px solid #E8EAED;
  border-radius: 8px;
  transition: all 0.3s;
  padding: 10px 15px;
}
.form-control:focus, .selectize-input.focus {
  border-color: #3498DB;
  box-shadow: 0 0 0 4px rgba(52,152,219,0.15);
  outline: none;
}

/* SLIDERS */
.irs-bar { background: linear-gradient(90deg, #3498DB 0%, #1ABC9C 100%); }
.irs-from, .irs-to, .irs-single { background: #3498DB; }
.irs-handle { border: 3px solid #3498DB; }

/* FOOTER */
.footer {
  margin-top: 70px;
  padding: 30px 0;
  background: linear-gradient(135deg, #ECF0F1 0%, #D5D8DC 100%);
  text-align: center;
  color: #566573;
  border-top: 4px solid #3498DB;
}
.footer p { margin: 8px 0; font-size: 15px; }
.footer strong { color: #2C3E50; }

/* NATIVE MULTISELECT */
select#covariates {
  width: 100%;
  border: 2px solid #E8EAED;
  border-radius: 8px;
  padding: 5px;
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  font-size: 14px;
  color: #2C3E50;
  background-color: #FFFFFF;
  transition: border-color 0.3s;
}
select#covariates:focus {
  border-color: #3498DB;
  box-shadow: 0 0 0 4px rgba(52,152,219,0.15);
  outline: none;
}
select#covariates option:checked {
  background-color: #3498DB;
  color: white;
}

/* DATA PREPARATION TABLE */
.prep-table {
  width: 100%;
  border-collapse: collapse;
}
.prep-table th {
  background: linear-gradient(135deg, #3498DB 0%, #2874A6 100%);
  color: white;
  padding: 12px 15px;
  text-align: left;
  font-weight: 700;
}
.prep-table td {
  padding: 8px 15px;
  border-bottom: 1px solid #E8EAED;
  vertical-align: middle;
}
.prep-table tr:hover td { background-color: #EBF5FB; }
.prep-table select {
  border: 1px solid #E8EAED;
  border-radius: 6px;
  padding: 4px 8px;
  font-size: 13px;
  color: #2C3E50;
  width: 100%;
}
.type-badge {
  display: inline-block;
  padding: 3px 10px;
  border-radius: 12px;
  font-size: 12px;
  font-weight: 700;
}
.type-numeric   { background: #D6EAF8; color: #1F618D; }
.type-integer   { background: #D5F5E3; color: #1E8449; }
.type-factor    { background: #FDEBD0; color: #784212; }
.type-ordered   { background: #F9EBEA; color: #922B21; }
.type-logical   { background: #E8DAEF; color: #6C3483; }
.type-character { background: #EAEDED; color: #2C3E50; }
"))
  ),

  # JavaScript
  tags$script(HTML("
    $(document).ready(function() {
      $('.navbar-header').append(
        '<img src=\"Logo_uulm_Vorlage_100mm_weiss.png\" class=\"navbar-logo-right\" alt=\"Uni Ulm\">'
      );
    });
  ")),

  tags$script(HTML("
    Shiny.addCustomMessageHandler('updateNativeSelect', function(msg) {
      var sel = document.getElementById(msg.id);
      if (!sel) return;
      sel.innerHTML = '';
      msg.choices.forEach(function(c) {
        var opt = document.createElement('option');
        opt.value = c;
        opt.text = c;
        sel.appendChild(opt);
      });
      $(sel).trigger('change');
    });
  ")),

  # ============================================================
  navbarPage(
    title = "MOBACO",
    id = "navbar",
    windowTitle = "MOBACO - Multi-Objective Balance Optimization",

    # ============================================
    # TAB 1: Data Upload
    # ============================================
    tabPanel(
      title = div(icon("upload"), " Data Upload"),
      value = "tab1",

      fluidRow(
        column(
          width = 12,
          div(
            class = "welcome-banner",
            h2("Welcome to MOBACO"),
            p("Multi-Objective Balance and Coverage Optimization"),
            p(style = "font-size: 16px; opacity: 0.9; margin-top: 10px;",
              "Developed at Universität Ulm | Institute of Medical Systems Biology")
          )
        )
      ),

      fluidRow(
        column(
          width = 4,
          wellPanel(
            h4(icon("file-upload"), "Upload Dataset"),

            fileInput(
              "data_file",
              NULL,
              accept = c(".csv", ".rds", ".xlsx", ".xls", ".sav", ".dta", ".sas7bdat"),
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            ),

            helpText(icon("info-circle"), " Supported: CSV, RDS, Excel, SPSS, Stata, SAS"),

            hr(),

            h4(icon("database"), "Example Dataset"),
            actionButton("load_example", "Load Example Cohort",
                         icon = icon("play-circle"), class = "btn-info btn-block"),

            br(),

            div(
              style = "padding: 15px; background-color: #EBF5FB;
                       border-left: 4px solid #3498DB; border-radius: 4px;",
              p(style = "margin: 0; font-size: 13px;",
                icon("lightbulb"), strong(" Tip:"),
                HTML(" A simulated cohort: 500 units, 103 of them in the
                      group of interest, nine covariates. Use it to explore
                      MOBACO before uploading your own data."))
            )
          )
        ),

        column(
          width = 8,
          conditionalPanel(
            condition = "output.data_loaded",
            wellPanel(
              h4(icon("table"), "Dataset Preview"),
              verbatimTextOutput("data_summary"),
              hr(),
              DTOutput("data_preview")
            )
          )
        )
      )
    ),

    # ============================================
    # TAB 2: Data Preparation (NEU)
    # ============================================
    tabPanel(
      title = div(icon("wrench"), " Data Preparation"),
      value = "tab_prep",

      conditionalPanel(
        condition = "output.data_loaded",

        fluidRow(
          column(
            width = 5,
            wellPanel(
              h4(icon("sliders-h"), "Column Types"),

              p(style = "color: #5D6D7E; font-size: 13px; margin-bottom: 15px;",
                icon("info-circle"),
                " Review and adjust column types. Changes are applied to the dataset before analysis."),

              div(
                style = "max-height: 520px; overflow-y: auto;",
                uiOutput("type_editor_ui")
              ),

              hr(),

              fluidRow(
                column(6,
                       actionButton("apply_types", "Apply Changes",
                                    icon = icon("check-circle"),
                                    class = "btn-success btn-block",
                                    style = "font-weight: 700;")
                ),
                column(6,
                       actionButton("reset_types", "Reset to Original",
                                    icon = icon("undo"),
                                    class = "btn-warning btn-block",
                                    style = "font-weight: 700;")
                )
              )
            )
          ),

          column(
            width = 7,
            wellPanel(
              h4(icon("chart-pie"), "Type Summary"),
              uiOutput("type_summary_ui")
            ),

            wellPanel(
              h4(icon("table"), "Updated Data Preview"),
              DTOutput("prep_data_preview")
            )
          )
        )
      ),

      conditionalPanel(
        condition = "!output.data_loaded",
        div(
          style = "text-align: center; padding: 80px; color: #7F8C8D;",
          icon("upload", style = "font-size: 60px; color: #BDC3C7; margin-bottom: 20px;"),
          h4(style = "border: none; color: #7F8C8D;", "Please upload data first"),
          p("Go to the Data Upload tab to load your dataset.")
        )
      )
    ),

    # ============================================
    # TAB 3: Variable Selection
    # ============================================
    tabPanel(
      title = div(icon("sliders-h"), " Variables"),
      value = "tab2",

      fluidRow(
        column(
          width = 4,
          wellPanel(
            h4(icon("flask"), "Treatment Variable"),

            selectInput("treatment_var", "Select Treatment Variable", choices = NULL),

            uiOutput("treatment_level_ui"),

            hr(),

            h4(icon("list-ul"), "Covariates"),

            checkboxInput("auto_covariates", "Auto-detect covariates", value = FALSE),

            conditionalPanel(
              condition = "!input.auto_covariates",
              tags$div(
                tags$label(
                  "Select Covariates",
                  style = "font-weight: 600; color: #2C3E50; margin-bottom: 6px; display: block;"
                ),
                tags$small(
                  style = "color: #7F8C8D; display: block; margin-bottom: 8px;",
                  icon("info-circle"),
                  " Hold Ctrl (Windows) or Cmd (Mac) to select multiple"
                ),
                tags$select(
                  id = "covariates",
                  multiple = "multiple",
                  size = "8",
                  tags$option("")
                )
              )
            ),

            hr(),

            actionButton(
              "confirm_variables", "Confirm Selection",
              icon = icon("check-circle"),
              class = "btn-success btn-block",
              style = "font-size: 16px; padding: 12px;"
            )
          )
        ),

        column(
          width = 8,
          conditionalPanel(
            condition = "output.variables_confirmed",
            wellPanel(
              h4(icon("clipboard-check"), "Selected Configuration"),
              verbatimTextOutput("variable_summary"),
              hr(),
              h4(icon("chart-bar"), "Group Sizes"),
              plotOutput("group_size_plot", height = "250px")
            )
          )
        )
      )
    ),

    # ============================================
    # TAB 4: Algorithm Settings
    # ============================================
    tabPanel(
      title = div(icon("cog"), " Settings"),
      value = "tab3",

      fluidRow(
        column(
          width = 6,
          wellPanel(
            h4(icon("balance-scale"), "Balance Metric"),

            radioButtons(
              "normalization", NULL,
              choices = c(
                "Asymmetric — divide by max(N_T, 1)"     = "asymmetric",
                "Symmetric — divide by max(N_T + N_C, 1)" = "symmetric"
              ),
              selected = "asymmetric"
            ),
            div(
              style = "font-size: 12px; color: #7F8C8D; margin-top: -6px;",
              HTML("Asymmetric is the formulation of Nikolaev et al. (2013),
                    Eq.&nbsp;(4), and the one used for the published results.
                    Both weight every bin of every covariate; they differ only
                    in the denominator.")
            ),

            hr(),

            h4(icon("users"), "Population & Generations"),

            sliderInput("popsize", "Population Size", min = 100, max = 2000, value = 500, step = 50),
            sliderInput("generations", "Generations", min = 100, max = 100000, value = 10000, step = 100),

            hr(),

            h4(icon("dna"), "Genetic Algorithm"),

            sliderInput("crossover_prob", "Crossover Probability", min = 0.5, max = 0.99, value = 0.8, step = 0.01),
            sliderInput("mutation_prob", "Mutation Probability", min = 0.001, max = 0.5, value = 0.1, step = 0.001),

            div(
              style = "margin-top: 12px; font-size: 12px; color: #7F8C8D;",
              HTML("Defaults follow the MOBACO publication &mdash; population 500,
                    crossover 0.8&nbsp;/&nbsp;10, mutation 0.1&nbsp;/&nbsp;10 &mdash;
                    except for the number of generations, reduced from
                    50&nbsp;000 to 10&nbsp;000 for interactive use. Set
                    generations to 50&nbsp;000 to reproduce the published runs.")
            )
          )
        ),

        column(
          width = 6,
          wellPanel(
            h4(icon("sliders-h"), "Advanced Settings"),

            sliderInput("crossover_dist", "Crossover Distribution", min = 5, max = 50, value = 10, step = 5),
            sliderInput("mutation_dist", "Mutation Distribution", min = 10, max = 80, value = 10, step = 5),

            hr(),

            h4(icon("layer-group"), "Constraints"),

            sliderInput("n_bins", "Number of Bins", min = 3, max = 10, value = 5, step = 1),
            numericInput("min_per_group", "Minimum per Group", value = NA, min = 1),

            hr(),

            div(
              style = "padding: 15px; background-color: #FEF9E7;
                       border-left: 4px solid #F39C12; border-radius: 4px;",
              checkboxInput("run_psm", HTML("<strong>Run PSM for comparison</strong>"), value = TRUE),
              conditionalPanel(
                condition = "input.run_psm",
                sliderInput("psm_caliper", "PSM Caliper", min = 0.0, max = 0.5, value = 0.2, step = 0.01)
              )
            )
          )
        )
      )
    ),

    # ============================================
    # TAB 5: Optimize
    # ============================================
    tabPanel(
      title = div(icon("rocket"), " Optimize"),
      value = "tab4",

      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4(icon("play-circle"), "Run MOBACO Optimization"),

            actionButton(
              "run_mobaco", "Start Optimization",
              icon = icon("play"),
              class = "btn-success btn-lg btn-block",
              style = "font-size: 22px; padding: 20px; font-weight: 700;"
            ),

            hr(),

            conditionalPanel(
              condition = "input.run_mobaco > 0",

              h4(icon("tasks"), "Progress"),

              tags$div(
                id = "running_box",
                style = "display:none; margin-bottom: 15px; padding: 15px;
                         background: linear-gradient(135deg, #EBF5FB, #D6EAF8);
                         border-radius: 10px; border-left: 5px solid #3498DB;",
                div(
                  style = "display: flex; align-items: center; gap: 15px;",
                  div(
                    style = "width: 40px; height: 40px; border-radius: 50%;
                             background: linear-gradient(135deg, #3498DB, #1ABC9C);
                             display: flex; align-items: center; justify-content: center;
                             animation: spin 1.5s linear infinite;",
                    icon("cog", style = "color: white; font-size: 18px;")
                  ),
                  div(
                    p(style = "margin: 0; font-weight: 700; color: #2C3E50; font-size: 16px;",
                      "Optimization running..."),
                    p(style = "margin: 0; color: #5D6D7E; font-size: 13px;",
                      "Elapsed: ", tags$strong(tags$span(id = "js_elapsed", "0 sec")))
                  )
                )
              ),

              tags$style(HTML("
                @keyframes spin {
                  from { transform: rotate(0deg); }
                  to   { transform: rotate(360deg); }
                }
              ")),

              tags$script(HTML("
                var _mobacoTimer = null;
                var _mobacoStart = null;

                function startMobacoTimer() {
                  _mobacoStart = Date.now();
                  document.getElementById('running_box').style.display = 'block';
                  _mobacoTimer = setInterval(function() {
                    var secs = Math.floor((Date.now() - _mobacoStart) / 1000);
                    var txt;
                    if (secs < 60) {
                      txt = secs + ' sec';
                    } else {
                      var mins = (secs / 60).toFixed(1);
                      txt = mins + ' min (' + secs + ' sec)';
                    }
                    var el = document.getElementById('js_elapsed');
                    if (el) el.innerText = txt;
                  }, 1000);
                }

                function stopMobacoTimer() {
                  if (_mobacoTimer) { clearInterval(_mobacoTimer); _mobacoTimer = null; }
                  var box = document.getElementById('running_box');
                  if (box) box.style.display = 'none';
                }

                $(document).on('click', '#run_mobaco', function() {
                  startMobacoTimer();
                });

                Shiny.addCustomMessageHandler('stopTimer', function(msg) {
                  stopMobacoTimer();
                });
              ")),

              verbatimTextOutput("optimization_log"),

              conditionalPanel(
                condition = "output.optimization_complete",
                hr(),
                div(
                  class = "alert alert-success",
                  style = "font-size: 18px;",
                  icon("check-circle", style = "font-size: 28px; margin-right: 10px;"),
                  strong("Optimization Complete!"),
                  br(), br(),
                  "Total runtime: ", strong(textOutput("total_runtime", inline = TRUE)),
                  br(),
                  "Proceed to ", strong("Results"), " tab to explore Pareto-optimal solutions."
                )
              )
            )
          )
        )
      )
    ),

    # ============================================
    # TAB 6: Results
    # ============================================
    tabPanel(
      title = div(icon("chart-line"), " Results"),
      value = "tab5",

      conditionalPanel(
        condition = "output.optimization_complete",

        fluidRow(
          column(
            width = 12,
            wellPanel(
              h4(icon("info-circle"), "Optimization Summary"),
              verbatimTextOutput("results_summary")
            )
          )
        ),

        fluidRow(
          column(
            width = 12,
            wellPanel(
              h4(icon("chart-area"), "Pareto Front"),
              conditionalPanel(
                condition = "output.has_plotly",
                uiOutput("pareto_plot_container")
              ),
              conditionalPanel(
                condition = "!output.has_plotly",
                plotOutput("pareto_plot_base", height = "550px")
              )
            )
          )
        ),

        fluidRow(
          column(
            width = 4,
            wellPanel(
              h4(icon("hand-pointer"), "Select Solution"),
              radioButtons(
                "solution_type", NULL,
                choices = c("Min Balance" = "min_balance",
                            "Knee Point"  = "knee",
                            "Max Coverage" = "max_coverage"),
                selected = "min_balance"
              ),
              hr(),
              downloadButton("download_selected", "Download Selected",
                             class = "btn-primary btn-block"),
              downloadButton("download_all", "Download All Solutions",
                             class = "btn-info btn-block")
            )
          ),
          column(
            width = 8,
            wellPanel(
              h4(icon("table"), "Selected Solution Preview"),
              DTOutput("solution_preview")
            )
          )
        )
      )
    ),

    # ============================================
    # TAB 7: Balance Table
    # ============================================
    tabPanel(
      title = div(icon("balance-scale"), " Balance Table"),
      value = "tab6",

      conditionalPanel(
        condition = "output.optimization_complete",

        fluidRow(
          column(
            width = 12,
            wellPanel(
              h4(icon("table"), "Balance Comparison"),

              radioButtons(
                "table_solution", "Solution:",
                choices = c("Min Balance"  = "min_balance",
                            "Knee Point"   = "knee",
                            "Max Coverage" = "max_coverage"),
                selected = "min_balance",
                inline = TRUE
              ),

              hr(),

              conditionalPanel(condition = "output.has_gt",  uiOutput("comparison_table_gt")),
              conditionalPanel(condition = "!output.has_gt", DTOutput("comparison_table_dt"))
            )
          )
        )
      )
    )

  ), # end navbarPage

  # Footer
  tags$footer(
    class = "footer",
    p(strong("MOBACO"), " - Multi-Objective Balance and Coverage Optimization"),
    p("Developed at ", strong("Universität Ulm"),
      " | Institute of Medical Systems Biology | © 2025")
  )
)
