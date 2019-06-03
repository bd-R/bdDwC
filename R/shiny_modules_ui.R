#' UI Module for {module_server_modals}
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_modal_ui <- function(id) {
  ns <- shiny::NS(id)  
  # Citation
  shiny::actionButton(
    ns("citation"),
    "Cite us",
    style = "border-color: #091520;
             background-color: #e5e5e5"
  )
}

#' Input module for {module_server_upload_local}
#'
#' @param mime_type a character vector of MIME types
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_upload_local_input <- function(
  id,
  mime_type = c(
    "text/csv",
    "text/comma-separated-values,text/plain",
    ".csv",
    ".zip",
    "application/zip"
  )
) {
  ns <- shiny::NS(id)
  shiny::fileInput(
    ns("path_input_data"),
    shiny::h3("CSV / DWCA ZIP file input"),
    accept = mime_type
  )
}

#' Input module for {module_server_upload_database}
#'
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_upload_database_input <- function(id) {
  databases <- list(
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
  coords <- list(
    "With Coordinates" = "1",
    "Without Coordinates" = "2",
    "No Filter" = "3"
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
      coords,
      3
    ),
    shiny::radioButtons(
      ns("query_db"),
      shiny::h3("Online Database:"),
      databases,
      "gbif"
    ),
    shiny::br(),
    shiny::actionButton(
      ns("query_database"),
      "Query Database",
      shiny::icon("download")
    )
  )
}

#' UI module for {module_ui_dictionary}
#'
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_dictionary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("dic_info")),
    shiny::br(),
    # Darwin Cloud dictionary
    shiny::tags$b("Update Darwin Cloud dictionary"),
    shiny::br(),
    shiny::actionButton(ns("update_darwin_cloud"), "Update DC"),
    shiny::br(), shiny::br(),
    shiny::fileInput(
      ns("path_input_dictionary"),
      "Choose a personal dictionary file",
      accept = c(
        "text/csv", ".csv", "text/comma-separated-values,text/plain"
      )
    )
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