#' Module to call modals
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_modals <- function(input, output, session)  {
  # Welcoming text
  shiny_server_modal_welcome()
  # Citation
  shiny::observeEvent(input$citation, {
    shiny_server_modal_custom(
      shiny::h3("Cite us"),
      shiny::tags$p("bdverse will be published soon!")
    )
  })
  # Information about the Darwin Cloud
  shiny::observeEvent(input$pop_dc, {
    shiny_server_modal_custom(
      shiny::h3("Darwin Cloud Data"),
      shiny::tags$p(
        "bdDwC uses Darwin Core Dictionary (stored on official",
        shiny::tags$a(
          href = "https://github.com/kurator-org/kurator-validation",
          "Kurator's repository)."
        ),
        shiny::br(),
        "Update Darwin Core version for your analysis by clicking",
        shiny::tags$b("Update DC"), "button bellow."
      ),
    )
  })
  # Information about users dictionary
  shiny::observeEvent(input$pop_dic, {
    shiny_server_modal_custom(
      shiny::h3("Personal Dictionary File"),
      shiny::tags$p("File with columns fieldname and standard name")
    )
  })
}
#' UI Module for {module_server_modals}
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_modals_ui <- function(id) {
  ns <- shiny::NS(id)  
  # Citation
  shiny::actionButton(
    ns("citation"),
    "Cite us",
    style = "border-color: #091520;
             background-color: #e5e5e5"
  )
}



#' Module to upload local users data
#' (calls {shiny_server_upload_local})
#'
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_upload_local <- function(input, output, session, rv) {
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
module_server_upload_local_input <- function(
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
module_server_upload_database <- function(input, output, session, rv) {
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
module_server_upload_database_input <- function(id) {
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

#' Module to create radio buttons for users dictionary
#' (calls {shiny_ui_dictionary_radiobuttons})
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_dictionary_radiobuttons <- function(input, output, session, rv) {
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

#' Module to update darwin cloud dictionary
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_dictionary <- function(input, output, session, rv) {
  ns <- session$ns
  output$dic_info <- shiny_ui_dictionary(
    input$path_input_dictionary$name,
    rv$info_dc_date,
    ns
  )
  shiny::observeEvent(input$update_darwin_cloud, {
    # Update DC dictionary
    rv$data_darwin_cloud <- download_cloud_data()
    rv$info_dc_date <- Sys.Date()
    output$dic_info <- shiny_ui_dictionary(
      input$path_input_dictionary$name,
      rv$info_dc_date,
      ns
    )
  })
  shiny::observeEvent(input$path_input_dictionary, {
    rv$dic_user_raw <- data.table::fread(
      input$path_input_dictionary$datapath,
      data.table = FALSE
    )
    rv$names_user_raw <- sort(colnames(rv$dic_user_raw))
    output$dic_info <- shiny_ui_dictionary(
      input$path_input_dictionary$name,
      rv$info_dc_date,
      ns
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
module_ui_dictionary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("dic_info")),
    shiny::br(),
    # Darwin Cloud dictionary
    shiny::tags$b("Update Darwin Cloud dictionary"),
    shiny::br(),
    shiny::actionButton(ns("update_darwin_cloud"), "Update DC"),
    shiny::br(),
    shiny::br(),
    shiny::fileInput(
      ns("path_input_dictionary"),
      "Choose a personal dictionary file",
      accept = c(
        "text/csv", ".csv", "text/comma-separated-values,text/plain"
      )
    )
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
module_ui_checkbox <- function(input, output, session, rv, match_type = NULL) {
  ns <- session$ns
  # Create checkbox with current user names
  output$names_user <- shiny::renderUI({
    if (length(rv$names_user_after) == 0) {
      return(NULL)
    } else {
      shiny::radioButtons(
        ns("names_user_radio"),
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
        ns("names_standard_radio"),
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
        shiny::checkboxGroupInput(ns("names_renamed_manual"), NULL, rev(foo))
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
        shiny::checkboxGroupInput(ns("names_renamed_darwinized"), NULL, foo)
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
        shiny::checkboxGroupInput(ns("names_renamed_identical"), NULL, foo)
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
module_ui_checkbox_ui <- function(id) {
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

#' Module for darwinizer
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_darwinizer <- function(input, output, session, rv, parent)  {
  shiny::observeEvent(input$submit_to_darwinizer, {
    # Jump to Darwinizer tab
    shinydashboard::updateTabItems(session = parent, "my_tabs", "darwinizer")
    # If user has uploaded dictionary
    if (nrow(rv$dic_user_raw) > 0) {
      # Update reactive user dictionary
      rv$dic_user <- subset(
        rv$dic_user_raw,
        select = c(input$names_user_field, input$names_user_standard)
      )
      colnames(rv$dic_user) <- c("fieldname", "standard")
    }

    # Get all standard names
    rv$names_standard <- unique(rv$data_darwin_cloud$standard)
    rv$names_standard_after <- unique(rv$data_darwin_cloud$standard)
    # Run Darwinizer with user and reference dictionary
    rv$data_darwinized <- bdDwC::darwinize_names(
      rv$data_user, rbind(rv$dic_user, rv$data_darwin_cloud)
    )

    # Checkboxes
    # Update if something was darwinized
    if (nrow(rv$data_darwinized) > 0) {
      rv$data_rename <- rv$data_darwinized
      rv$data_rename$name_rename <- link_old_new(rv$data_rename)
      # Updated (remove name) from standard names
      rv$names_standard_after <- rv$names_standard[
        !rv$names_standard %in% rv$data_rename$name_new
      ]
      # Updated (remove name) from user names
      rv$names_user_after <- rv$names_user[
        !tolower(rv$names_user) %in% tolower(rv$data_rename$name_old)
      ]
    }
  })
  return(rv)
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

#' Module to enable/disable darwinizer buttons
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_buttons <- function(input, output, session, rv)  {
  shiny::observe({
    # Disable submission if there's no data
    shinyjs::toggleState(
      "submit_to_darwinizer",
      nrow(rv$data_user) > 0
    )
    # Disable renaming when no names left
    shinyjs::toggleState(
      "names_rename",
      all(
        length(rv$names_user_after) > 0,
        length(rv$names_standard_after) > 0
      )
        # nrow(rv$data_rename) == 0
    )
    # Disable rollback when no nothing was darwinized
    shinyjs::toggleState(
      "names_rollback",
      length(rv$data_darwinized$name_old) > 0
    )
  })
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

#' Module to control rename button 
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_buttons_rename <- function(input, output, session, rv)  {
  shiny::observeEvent(input$names_rename, {
    # Update renamed dataset
    rv$data_rename$name_rename <- NULL
    rv$data_rename <- rbind(
      rv$data_rename,
      data.frame(
        name_old = input$names_user_radio,
        name_new = input$names_standard_radio,
        match_type = "Manual",
        stringsAsFactors = FALSE
      )
    )
    # Create (combine) renamed name
    rv$data_rename$name_rename <- link_old_new(rv$data_rename)
    # Updated (remove name) from standard names
    rv$names_standard_after <- rv$names_standard[
      !rv$names_standard %in% rv$data_rename$name_new
    ]
    # Updated (remove name) from user names
    rv$names_user_after <- rv$names_user[
      !tolower(rv$names_user) %in% tolower(rv$data_rename$name_old)
    ]
  })
  return(rv)
}

#' Module to control remove button
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_buttons_remove <- function(input, output, session, rv)  {
  shiny::observeEvent(input$names_remove, {
    remove_names <- c()
    if (length(input$names_renamed_manual) > 0) {
      remove_names <- c(remove_names, input$names_renamed_manual)
    }
    if (length(input$names_renamed_darwinized) > 0) {
      remove_names <- c(remove_names, input$names_renamed_darwinized)
    }
    if (length(input$names_renamed_identical) > 0) {
      remove_names <- c(remove_names, input$names_renamed_identical)
    }
    # Remove input from renamed names dataset
    rv$data_rename <- rv$data_rename[
      !rv$data_rename$name_rename %in% remove_names,
    ]
    # Update standard names checkbox
    rv$names_standard_after <- rv$names_standard[
      !rv$names_standard %in% rv$data_rename$name_new
    ]
    # Update user names checkbox
    rv$names_user_after <- rv$names_user[
      !tolower(rv$names_user) %in% tolower(rv$data_rename$name_old)
    ]
  })
  return(rv)
}

#' Module to control clean button
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_buttons_clean <- function(input, output, session, rv)  {
  shiny::observeEvent(input$names_clean, {
    rv$data_rename <- data.frame()
    rv$names_standard_after <- rv$names_standard
    rv$names_user_after <- rv$names_user
  })
  return(rv)
}

#' Module to control rollback button
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_buttons_rollback <- function(input, output, session, rv)  {
  shiny::observeEvent(input$names_rollback, {
    if (nrow(rv$data_darwinized) > 0) {
      rv$data_rename <- rv$data_darwinized
      rv$data_rename$name_rename <- link_old_new(rv$data_rename)
      rv$names_standard_after <- rv$names_standard[
        !rv$names_standard %in% rv$data_rename$name_new
      ]
      rv$names_user_after <- rv$names_user[
        !tolower(rv$names_user) %in% tolower(rv$data_rename$name_old)
      ]
    }
  })
  return(rv)
}

#' Module to control values boxes
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_valuebox <- function(input, output, session, rv)  {
  shiny::observeEvent(
    c(
      input$submit_to_darwinizer,
      input$names_rollback,
      input$names_clean,
      input$names_remove,
      input$names_rename
    ),
  {
    output$vb_all_names <- shiny_ui_valuebox(
      length(rv$names_user), "Names Submitted", "light-blue"
    )
    output$vb_dwc_names <- shiny_ui_valuebox(
      paste0(
        nrow(rv$data_rename),
        " (", round(nrow(rv$data_rename) * 100 / length(rv$names_user)), "%)"
      ),
      "Names Darwinized",
      "olive"
    )
    output$vb_dwc_ident <- shiny_ui_valuebox(
      sum(rv$data_rename$match_type == "Identical"),
      "Darwinized: Identical",
      "green"
    )
    output$vb_dwc_match <- shiny_ui_valuebox(
      sum(rv$data_rename$match_type == "Darwinized"),
      "Darwinized: Matched",
      "green"
    )
    output$vb_manual <- shiny_ui_valuebox(
      sum(rv$data_rename$match_type == "Manual"),
      "Darwinized: Manually",
      "green"
    )
  })
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

#' Module to control download button
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_buttons_download <- function(input, output, session, rv)  {
  output$download_data <- shiny::downloadHandler(
    filename = format(Sys.time(), "darwinizedData_%Y_%b_%d_%X.csv"),
    content = function(file) {
      data.table::fwrite(rename_user_data(rv$data_user, rv$data_rename), file)
    }
  )
}

#' Module to update (enable/disable) radio buttons for users dictionary
#' 
#' @param rv reactive values
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_ui_dictionary_radiobuttons_update <- function(
  input, output, session, rv
) {
  ns <- session$ns
  shiny::observeEvent(input$names_user_standard, {
    # Which button was marked
    result <- grepl(input$names_user_standard, rv$names_user_raw)
    # Disable marked button in a opposite box
    shinyjs::disable(selector = paste0(
      "#", ns("names_user_field"),
      ".radio:nth-child(", which(result), ") label"
    ))
    # Enable all non marked buttons in current box
    shinyjs::enable(selector = paste0(
      "#", ns("names_user_field"),
      ".radio:nth-child(", which(!result), ") label"
    ))
  })
  # If button in field is marked
  shiny::observeEvent(input$names_user_field, {
    # Which button was marked
    result <- grepl(input$names_user_field, rv$names_user_raw)
    # Disable marked button in a opposite box
    shinyjs::disable(selector = paste0(
      "#", ns("names_user_standard"),
      ".radio:nth-child(", which(result), ") label"
    ))
    # Enable all non marked buttons in current box
    shinyjs::enable(selector = paste0(
      "#", ns("names_user_standard"), 
      ".radio:nth-child(", which(!result), ") label"
    ))
  })
}