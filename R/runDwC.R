#' Launch bdDwC Shiny Application
#'
#' `bdDwC` is a function that starts bdverse Darwin Cloud cleaning `shiny` app.
#'
#' @examples \dontrun{
#'     runDwC()
#' }
#' 
#' @return `shiny::runApp()` result within browser.
#' 
#' @import shinydashboard
#' @import shiny
#' @import shinyBS
#' @importFrom shinyjs addCssClass disable disabled enable useShinyjs 
#' 
#' @export
#' 
runDwC <- function() {
    pathApp <- system.file("shiny", package = "bdDwC")
    return(shiny::runApp(pathApp, launch.browser = TRUE))
}