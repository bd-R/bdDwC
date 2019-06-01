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

#' Module to upload users dictionary
#'
#' @param rv reactive values
#' 
#' @importFrom data.table fread
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_upload_dictionary <- function(input, output, server, rv) {
  shiny::observeEvent(input$path_input_dictionary, {
    rv$dic_user_raw <- data.table::fread(
      input$path_input_dictionary$datapath,
      data.table = FALSE
    )
    rv$names_user_raw <- sort(colnames(rv$dic_user_raw))
    output$dic_info <- shiny_ui_dictionary(
      input$path_input_dictionary$name,
      rv$info_dc_date
    )
  })
  return(rv)
}
#' Input module for {module_server_upload_dictionary}
#'
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_upload_dictionaryInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::fileInput(
    ns("path_input_dictionary"),
    "Choose a personal dictionary file",
    accept = c(
      "text/csv", ".csv", "text/comma-separated-values,text/plain"
    )
  )
}

#' Module to create radio buttons for users dictionary
#' (calls {shiny_ui_dictionary_radiobuttons})
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_dictionary_radiobuttons_field <- function(input, output, server, rv) {
  output$names_user_field <- shiny::renderUI({
    if (is.null(rv$names_user_raw)) {
      return(NULL)
    }
    shiny_ui_dictionary_radiobuttons(
      rv$names_user_raw,
      "names_user_field",
      "Field Names",
      "userField",
      rv$names_user_raw[1]
    )
  })
  output$names_user_standard <- shiny::renderUI({
    if (is.null(rv$names_user_raw)) {
      return(NULL)
    }
    shiny_ui_dictionary_radiobuttons(
      rv$names_user_raw,
      "names_user_standard",
      "Standard Names",
      "userStandard",
      rv$names_user_raw[2]
    )
  })
}
#' Output module for {module_ui_dictionary_radiobuttons_field}
#'
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_dictionary_radiobuttons_fieldOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::splitLayout(
    shiny::uiOutput(ns("names_user_field")),
    shiny::uiOutput(ns("names_user_standard")),
    cellWidths = 200,
    cellArgs = list(style = "padding: 6px")
  )
}

#' Module to update darwin cloud dictionary
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_dictionary <- function(input, output, server, rv) {
  output$dic_info <- shiny_ui_dictionary(
    input$path_input_dictionary$name,
    rv$info_dc_date
  )
  shiny::observeEvent(input$update_darwin_cloud, {
    # Update DC dictionary
    rv$data_darwin_cloud <- download_cloud_data()
    rv$info_dc_date <- Sys.Date()
    output$dic_info <- shiny_ui_dictionary(
      input$path_input_dictionary$name,
      rv$info_dc_date
    )
  })
  return(rv)
}
#' UI module for {module_ui_dictionary}
#'
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_dictionaryUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("dic_info")),
    shiny::br(),
    # Darwin Cloud dictionary
    shiny::tags$b("Update Darwin Cloud dictionary"),
    shiny::br(),
    shiny::actionButton(ns("update_darwin_cloud"), "Update DC"),
    shiny::br(),
    shiny::br()
  )
}

#' Module to call modals
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_modals <- function(input, output, server)  {
  # Welcoming text
  shiny_server_modal_welcome()
  # Citation
  shiny::observeEvent(input$citation, {
    shiny_server_modal_citation()
  })
}
#' UI Module for {module_server_modals}
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_modalsUI <- function(id) {
  ns <- shiny::NS(id)  
  # Citation
  shiny::actionButton(
    ns("citation"),
    "Cite us",
    style = "border-color: #091520;
             background-color: #e5e5e5"
  )
}

#' Module to create checkbox for user names
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_checkbox <- function(input, output, server, rv, match_type = NULL)  {
  # Create checkbox with current user names
  output$names_user <- shiny::renderUI({
    if (length(rv$names_user_after) == 0) {
      return(NULL)
    } else {
      shiny::radioButtons(
        "names_user_radio",
        "User Names",
        sort(rv$names_user_after)
      )
    }
  })
  # Create checkbox with current standard names
  output$names_standard <- shiny::renderUI({
    if (length(rv$names_standard_after) == 0) {
      return(NULL)
    } else {
      res <- shiny::radioButtons(
        "names_standard_radio",
        "Stand Names",
        sort(rv$names_standard_after)
      )
      # Adding unique ID so we can add info boxes with additional info
      for (i in sort(rv$names_standard_after)) {
        res <- gsub(
          paste0("<span>", i, "</span>"),
          paste0("<span id=\"DWC_", i, "\">", i, "</span>"),
          res
        )
      }
      shiny::HTML(res)
    }
  })
  # Create checkbox manually renamed terms
  output$names_renamed_manual <- shiny::renderUI({
    if (length(rv$data_rename$name_rename) == 0) {
      shiny::h5("Nothing was renamed")
    } else {
      foo <- subset(rv$data_rename, match_type == "Manual")$name_rename
      if (length(foo) > 0) {
        # Use rev to have newest on top
        shiny::checkboxGroupInput("names_renamed_manual", NULL, rev(foo))
      } else {
        shiny::h5("Nothing was renamed")
      }
    }
  })
  # Create checkbox for darwinized terms
  output$names_renamed_darwinized <- shiny::renderUI({
    if (length(rv$data_rename$name_rename) == 0) {
      shiny::h5("No names were Darwinized")
    } else {
      foo <- subset(rv$data_rename, match_type == "Darwinized")$name_rename
      if (length(foo) > 0) {
        # Use rev to have newest on top
        shiny::checkboxGroupInput("names_renamed_darwinized", NULL, foo)
      } else {
        shiny::h5("No names were Darwinized")
      }
    }
  })
  # Create checkbox for identical matches
  output$names_renamed_identical <- shiny::renderUI({
    if (length(rv$data_rename$name_rename) == 0) {
      shiny::h5("No names were Identical")
    } else {
      foo <- subset(rv$data_rename, match_type == "Identical")$name_rename
      if (length(foo) > 0) {
        # Use rev to have newest on top
        shiny::checkboxGroupInput("names_renamed_identical", NULL, foo)
      } else {
        shiny::h5("No names were Identical")
      }
    }
  })
}
#' UI module for {module_ui_checkbox_usernames}
#'
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
#'
module_ui_checkboxUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(2,
      shiny::uiOutput(ns("names_user"))
    ),
    shiny::column(2,
      shiny::uiOutput(ns("names_standard")),
      shiny::uiOutput(ns("names_standard_hover")),
      offset = 1
    ),
    shinydashboard::box(
      title = "Darwinized Names",
      width = 2, status = "success", collapsible = TRUE, solidHeader = TRUE,
      shiny::uiOutput(ns("names_renamed_darwinized"))
    ),
    shinydashboard::box(
      title = "Manually Renamed",
      width = 2, status = "success", collapsible = TRUE, solidHeader = TRUE,
      shiny::uiOutput(ns("names_renamed_manual"))
    ),
    shinydashboard::box(
      title = "Identical Matches",
      width = 2, status = "success", collapsible = TRUE, solidHeader = TRUE,
      shiny::uiOutput(ns("names_renamed_identical"))
    )
  )
}