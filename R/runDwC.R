#' Launch bdDwC Shiny Application
#'
#' `bdDwC` is a function that starts bdverse Darwin Cloud cleaning `shiny` app.
#' 
#' @return `shiny::runApp()` result within browser.
#' 
#' @import shinydashboard
#' @import shiny
#' @import shinyBS
#' @importFrom data.table fread
#' @importFrom finch dwca_read
#' @importFrom rgbif occ_search
#' @importFrom shinyjs addCssClass disable disabled enable useShinyjs removeCssClass
#' @importFrom spocc occ
#' 
#' @export
#' 
runDwC <- function() {
    pathApp <- system.file("shiny", package = "bdDwC")
    return(shiny::runApp(pathApp, launch.browser = TRUE))
}