#' Module to upload local users data
#' (calls {shiny_server_upload_local})
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

#' Module to query data from the remote database
#' (calls {shiny_server_upload_database})
#'
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_upload_database <- function(input, output, server, rv) {
  shiny::observeEvent(input$query_database, {
    rv$data_user <- shiny_server_upload_database(
      input$scientific_name,
      input$record_size,
      input$query_db,
      input$has_coords
    )
    rv$names_user <- rv$names_user_after <- colnames(rv$data_user)
  })
  return(rv)
}
#' Input module for {module_server_upload_database}
#'
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_upload_databaseInput <- function(id) {

online_databases <- list(
  "GBIF (Global Biodiversity Information Facility)" = "gbif",
  "iDigBio (Integrated Digitized Biocollections)" = "idigbio",
  "EcoEngine (Berkeley Ecoinformatics Engine)" = "ecoengine",
  "Vertnet (Vertebrate Network)" = "vertnet",
  "BISON (Biodiversity Information Serving Our Nation)" = "bison",
  "iNaturalist" = "inat",
  "ALA (Atlas of Living Australia)" = "ala",
  "OBIS (Ocean Biogeographic Information System)" = "obis",
  "AntWeb" = "antweb"
)

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textInput(
      ns("scientific_name"),
      shiny::h3("Scientific Name:"),
      "Puma concolor"
    ),
    shiny::numericInput(
      ns("record_size"),
      shiny::h3("Record Size:"),
      500
    ),
    shiny::selectInput(
      ns("has_coords"),
      shiny::h3("Records Filter:"),
      list("With Coordinates" = "1",
           "Without Coordinates" = "2",
           "No Filter" = "3"
      ),
      3
    ),
    shiny::radioButtons(
      ns("query_db"),
      shiny::h3("Online Database:"),
      online_databases,
      "gbif"
    ),
    shiny::br(),
    shiny::div(id = "query_database_div",
      class = "activeButton",
      shiny::actionButton(
        ns("query_database"), "Query Database", shiny::icon("download")
      )
    )
  )
}