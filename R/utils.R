#' Launch bdDwC Shiny Application
#'
#' `run_dwc` is a function that starts bdverse Darwin Cloud cleaning `shiny` app.
#'
#' @param path_shiny a character string that specifies path within bdDwC
#' package to shiny app
#'
#' @return `shiny::runApp()` result within browser.
#'
#' @import shinydashboard
#' @import shiny
#' @import shinyBS
#' @importFrom data.table fread fwrite
#' @importFrom finch dwca_read
#' @importFrom rgbif occ_search
#' @importFrom shinyjs addCssClass disable disabled enable useShinyjs removeCssClass
#' @importFrom spocc occ
#'
#' @export
#'
run_dwc <- function(path_shiny = "shiny") {
  if (
    !is.character(path_shiny) |
      nchar(path_shiny) < 1 |
      length(path_shiny) != 1
  ) {
    stop("Specify correct path to shiny app (e.g. \"shiny\")")
  }
  path_app <- system.file(path_shiny, package = "bdDwC")
  if (!file.exists(path_app)) {
    stop("Given path to shiny app doesn't exist")
  } else if (!interactive()) {
    stop("This shiny session is not interactive, can't run shiny")
  } else {
    return(shiny::runApp(path_app)) # nocov
  }
}
