#' Module to upload local users data (call {shiny_server_upload_local})
#'
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_upload_local <- function(input, output, server, rv) {
  shiny::observeEvent(input$path_input_data, {
    rv$data_user <- shiny_server_upload_local(input$path_input_data)
    rv$names_user <- rv$names_user_after <- colnames(rv$data_user)
  })
  return(rv)
}

#' Input module for {module_server_upload_local}
#'
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_upload_localInput <- function(
  id,
  upload_local_file = c(
    "text/csv",
    "text/comma-separated-values,text/plain",
    ".csv",
    ".zip",
    "application/zip"
  )
) {
  ns <- shiny::NS(id)
  shiny::fileInput(ns("path_input_data"),
    shiny::h3("CSV / DWCA ZIP file input"),
    FALSE,
    accept = upload_local_file
  )
}