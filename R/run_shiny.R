#' Launch MOBACO Shiny App
#'
#' Interactive web interface for Multi-Objective Balance and Coverage
#' Optimization. No programming required.
#'
#' @param launch.browser Logical. Open in browser? (default: TRUE)
#' @param port Integer. Port number (default: NULL = auto-assign)
#'
#' @examples
#' \dontrun{
#' # Launch MOBACO Shiny App
#' mobaco_app()
#' }
#'
#' @export
mobaco_app <- function(launch.browser = TRUE, port = NULL) {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Package 'shiny' is required to run the MOBACO app.\n",
      "Install it with: install.packages('shiny')",
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny", package = "MOBACO")

  if (app_dir == "") {
    stop(
      "Could not find Shiny app directory.\n",
      "The app files should be in: inst/shiny/\n",
      "Try re-installing MOBACO: devtools::install()",
      call. = FALSE
    )
  }

  message("Starting MOBACO Shiny App...")
  message("App directory: ", app_dir)

  shiny::runApp(
    app_dir,
    launch.browser = launch.browser,
    port = port,
    display.mode = "normal"
  )
}
