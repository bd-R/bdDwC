#' UI module for {module_server_modals}
#' 
#' This module produces UI for modal dialogs
#' 
#' @param id_namespace a character string to be namespaced
#' @param label_citation a character string to display on citation button
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_modal_ui <- function(
  id_namespace = "main",
  id_citation = "citation",
  label_citation = "Cite us"
) {
  ns <- shiny::NS(id_namespace)  
  shiny::actionButton(
    ns(id_citation),
    label_citation,
    style = "border-color: #091520;
             background-color: #e5e5e5"
  )
}

#' Input module for {module_server_upload_local}
#' 
#' This module procuces UI for local file upload
#'
#' @param id_namespace a character string to be namespaced
#' @param id_input a character that specifies input slot
#' @param label_input a character string to display for a file upload
#' @param mime_type a character vector of MIME types
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_upload_local_input <- function(
  id_namespace = "main",
  id_input = "path_input_data",
  label_input = "CSV / DWCA ZIP file input",
  mime_type = c(
    "text/csv",
    "text/comma-separated-values,text/plain",
    ".csv",
    ".zip",
    "application/zip"
  )
) {
  ns <- shiny::NS(id_namespace)
  shiny::fileInput(ns(id_input), shiny::h3(label_input), accept = mime_type)
}

#' Input module for {module_server_upload_database}
#' 
#' This module procuces UI for quering from the database
#'
#' @param id_namespace a character string to be namespaced
#' @param id_text a character that specifies text slot
#' @param label_text a character to display for a text slot
#' @param value_text a character that species scientific name
#' @param id_num a character that specifies numeric slot
#' @param label_num a character to display for a numeric slot
#' @param value_num a numeric value for number of species to download
#' @param id_coord a character that specifies coordinate slot
#' @param label_coord a character to display for a coordinate slot
#' @param value_id_coord numeric values as index for {value_label_coord}
#' @param value_label_coord a character string (vector) to display as
#' coordinate selections
#' @param id_database a character string that specifies database slot
#' @param label_databasea a character string to display for a database slot
#' @param value_id_database a character vector as index for
#' {value_label_database}
#' @param value_label_database a character vector to display as database names
#' @param id_query a character string that specifies action button to query
#' @param label_query a character string to display on query database button
#' 
#' @importFrom stats setNames
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_upload_database_input <- function(
  id_namespace = "main",
  id_text = "scientific_name",
  label_text = "Scientific Name:",
  value_text = "Puma concolor",
  id_num = "record_size",
  label_num = "Record Size:",
  value_num = 500,
  id_coord = "has_coords",
  label_coord = "Records Filter:",
  value_id_coord = 1:3,
  value_label_coord = c("With Coordinates", "Without Coordinates", "No Filter"),
  id_database = "query_db",
  label_database = "Online Database:",
  value_id_database = c(
    "gbif", "idigbio", "ecoengine", "vertnet", "bison",
    "inat", "ala", "obis", "antweb"
  ),
  value_label_database = c(
    "GBIF (Global Biodiversity Information Facility)",
    "iDigBio (Integrated Digitized Biocollections)",
    "EcoEngine (Berkeley Ecoinformatics Engine)",
    "Vertnet (Vertebrate Network)",
    "BISON (Biodiversity Information Serving Our Nation)",
    "iNaturalist",
    "ALA (Atlas of Living Australia)",
    "OBIS (Ocean Biogeographic Information System)",
    "AntWeb"
  ),
  id_query = "query_database",
  label_query = "query_database"
) {
  ns <- shiny::NS(id_namespace)
  shiny::tagList(
    shiny::textInput(ns(id_text), shiny::h3(label_text), value_text),
    shiny::numericInput(ns(id_num), shiny::h3(label_num), value_num),
    shiny::selectInput(
      ns(id_coord),
      shiny::h3(label_coord),
      stats::setNames(value_id_coord, value_label_coord),
      value_id_coord[3]
    ),
    shiny::radioButtons(
      ns(id_database),
      shiny::h3(label_database),
      stats::setNames(value_id_database, value_label_database),
      value_id_database[1]
    ),
    shiny::br(),
    shiny::actionButton(ns(id_query), label_query, shiny::icon("download"))
  )
}

#' UI module for {module_ui_dictionary}
#' 
#' This module procuces UI dictionary panel
#'
#' @param id_namespace a character string to be namespaced
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_dictionary_ui <- function(
  id_namespace,
  id_dic_info = "dic_info",
  id_dwc = "update_darwin_cloud",
  label_dwc = "Update Darwin Cloud dictionary",
  id_file = "path_input_dictionary",
  label_file = "Choose a personal dictionary file",
  type_file = c("text/csv", ".csv", "text/comma-separated-values,text/plain")
) {
  ns <- shiny::NS(id_namespace)
  shiny::tagList(
    shiny::uiOutput(ns(id_dic_info)),
    shiny::br(),
    shiny::tags$b("Update Darwin Cloud dictionary"),
    shiny::br(),
    shiny::actionButton(ns(id_dwc), label_dwc),
    shiny::br(), shiny::br(),
    shiny::fileInput(ns(id_file), label_file, accept = type_file)
  )
}

#' Output module for {module_ui_dictionary_radiobuttons}
#'
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_dictionary_radiobuttons_output <- function(id) {
  ns <- shiny::NS(id)
  shiny::splitLayout(
    shiny::uiOutput(ns("names_user_field")),
    shiny::uiOutput(ns("names_user_standard")),
    cellWidths = 200,
    cellArgs = list(style = "padding: 6px")
  )
}

#' UI module for {module_ui_checkbox_usernames}
#'
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_checkbox_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(2, shiny::uiOutput(ns("names_user"))),
    shiny::column(2,
      shiny::uiOutput(ns("names_standard")),
      shiny::uiOutput(ns("names_standard_hover")),
      offset = 1
    ),
    create_renaming_checkbox(
      shiny::uiOutput(ns("names_renamed_darwinized")),
      "Darwinized Names"
    ),
    create_renaming_checkbox(
      shiny::uiOutput(ns("names_renamed_manual")),
      "Manually Renamed"
    ),
    create_renaming_checkbox(
      shiny::uiOutput(ns("names_renamed_identical")),
      "Identical Matches"
    )
  )
}

#' Input module for {module_server_darwinizer}
#'
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_darwinizer_input <- function(id) {
  ns <- shiny::NS(id)
  shiny::actionButton(
    ns("submit_to_darwinizer"),
    "Submit to Darwinizer",
    width = 250,
    style = "background: url('Darwin.svg');
             background-position: left center;
             background-repeat: no-repeat;
             background-color: #ffffff;
             color: #000000;
             border-color: #091520;
             padding:10px;
             font-size:120%"
  )
}

#' Input module for {module_ui_buttons}
#'
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_buttons_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(2,
      shiny::br(), shiny::br(),
      shiny::actionButton(
        ns("names_rename"),
        "Rename",
        icon = shiny::icon("arrow-circle-right"),
        width = 210,
        style = "color: #000000;
                 background-color: #71a879;
                 border-color: #091520;
                 padding:10px;
                 font-size:120%"
      ),
      offset = 1
    ),
    shiny::column(2,
      shiny::verticalLayout(
        shiny::actionButton(
          ns("names_remove"),
          "Remove selected rename",
          icon = shiny::icon("times"),
          width = 210,
          style = "color: #000000;
                   background-color: #a188bd;
                   border-color: #091520"
        ),
        shiny::br(),
        shiny::actionButton(
          ns("names_clean"),
          "Remove all renames",
          icon = shiny::icon("times"),
          width = 210,
          style = "color: #000000;
                   background-color: #a188bd;
                   border-color: #091520"
        ),
        shiny::br(),
        shiny::actionButton(
          ns("names_rollback"),
          "Rollback to Darwinizer",
          icon = shiny::icon("fast-backward"),
          width = 210,
          style = "color: #000000;
                   background-color: #c4cc6d;
                   border-color: #091520"
        )
      ),
      offset = 2
    ),
    shiny::column(2,
      shiny::downloadButton(
        ns("download_data"),
        "Download final data",
        icon = shiny::icon("check"),
        width = 210,
        style = "color: #000000;
                 background-color: #71a879;
                 border-color: #091520;
                 padding:10px;
                 font-size:120%"
      ),
      offset = 0
    ),
    style = "margin-bottom:30px;
             border-bottom:2px solid;
             padding: 20px"
  )
}

#' Output module for {module_ui_valuebox}
#'
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_valuebox_output <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::valueBoxOutput(ns("vb_all_names"), width = 2),
    shinydashboard::valueBoxOutput(ns("vb_dwc_names"), width = 2),
    shinydashboard::valueBoxOutput(ns("vb_dwc_match"), width = 2),
    shinydashboard::valueBoxOutput(ns("vb_manual"), width = 2),
    shinydashboard::valueBoxOutput(ns("vb_dwc_ident"), width = 2)
  )
}