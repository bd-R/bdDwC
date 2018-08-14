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
#' @import shiny
#' @import shinyBS
#' @import shinydashboard
#' @import shinyjs
#' 
#' @export
#' 
runDwC <- function() {
    pathApp <- system.file("shiny", package = "bdDwC")
    return(shiny::runApp(pathApp, launch.browser = TRUE))
}