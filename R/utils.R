#' Launch bdDwC Shiny Application
#'
#' `run_dwc` is a function that starts bdverse Darwin Cloud cleaning `shiny` app.
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
run_dwc <- function() {
  path_app <- system.file("shiny", package = "bdDwC")
  return(shiny::runApp(path_app, launch.browser = TRUE))
}