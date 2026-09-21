#' Import Data from Various Formats
#'
#' Read data from CSV, RDS, Excel, SPSS, Stata, or SAS files.
#'
#' @param file Character. Path to the data file.
#' @param format Character. File format: "auto" (default), "csv", "rds",
#'   "xlsx", "sav" (SPSS), "dta" (Stata), "sas7bdat" (SAS).
#' @param ... Additional arguments passed to the read function.
#'
#' @return A data frame.
#'
#' @examples
#' \dontrun{
#' # Auto-detect format
#' data <- read_data("mydata.csv")
#'
#' # Specify format
#' data <- read_data("mydata.xlsx", format = "xlsx")
#' }
#'
#' @export
read_data <- function(file, format = "auto", ...) {

  if (!file.exists(file)) {
    stop(sprintf("File not found: %s", file))
  }

  # Auto-detect format
  if (format == "auto") {
    ext <- tolower(tools::file_ext(file))
    format <- switch(
      ext,
      "csv" = "csv",
      "rds" = "rds",
      "xlsx" = "xlsx",
      "xls" = "xlsx",
      "sav" = "sav",
      "dta" = "dta",
      "sas7bdat" = "sas7bdat",
      stop(sprintf("Unknown file extension: %s", ext))
    )
  }

  # Read file
  data <- switch(
    format,
    "csv" = readr::read_csv(file, ...),
    "rds" = readRDS(file),
    "xlsx" = readxl::read_excel(file, ...),
    "sav" = haven::read_sav(file, ...),
    "dta" = haven::read_dta(file, ...),
    "sas7bdat" = haven::read_sas(file, ...),
    stop(sprintf("Unsupported format: %s", format))
  )

  # Convert to data frame
  as.data.frame(data)
}
