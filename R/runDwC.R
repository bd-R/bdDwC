#' Launch bdDwC Shiny Application
#'
#' `bdDwC` is a function that starts bdverse Darwin Cloud cleaning `shiny` app.
#'
#' @export
runDwC <- function() {
    library(shiny)
    library(shinyBS)
    library(shinyjs)
    pathApp <- system.file("shiny", package = "bdDwC")
    return(shiny::runApp(pathApp, launch.browser = TRUE))
}