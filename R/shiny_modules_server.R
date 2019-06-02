#' Module to call modals
#' 
#' @family shiny modules
#'
#' @keywords shiny modules internal
#'
module_server_modal <- function(input, output, session)  {
  # Welcoming text
  shiny_server_modal(
    title = shiny::h3("Welcome to Darwinizer!"),
    body = shiny::tags$p(
      shiny::p("Darwinize Your Data"),
      shiny::img(src = "bdverse.png", align = "center", width = "570"),
      shiny::helpText(
        "GPL-3 License Tomer Gueta, Vijay Barve, Povilas Gibas,
         Thiloshon Nagarajah, Ashwin Agrawal and Carmel Yohay",
         "(", format(Sys.Date(), "%Y"), ").",
         shiny::br(),
         "Package version", as.character(utils::packageVersion("bdDwC"))
      ),
      shiny::helpText(
        "Contribute: ",
        shiny::a(
          "https://github.com/bd-R/bdDwC",
          href = "https://github.com/bd-R/bdDwC"
        ),
        shiny::br(), "Join: ",
        shiny::a(
          "https://bd-r-group.slack.com",
          href = "https://bd-r-group.slack.com"
        )
      )
    )
  )
  # Citation
  shiny::observeEvent(input$citation, {
    shiny_server_modal(
      title = shiny::h3("Cite us"),
      body = shiny::tags$p("bdverse will be published soon!")
    )
  })
  # Information about the Darwin Cloud
  shiny::observeEvent(input$pop_dc, {
    shiny_server_modal(
      title = shiny::h3("Darwin Cloud Data"),
      body = shiny::tags$p(
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
    shiny_server_modal(
      title = shiny::h3("Personal Dictionary File"),
      body = shiny::tags$p("File with columns fieldname and standard name")
    )
  })
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
  ns <- session$ns
  output$names_user_field <- shiny::renderUI({
    if (is.null(rv$names_user_raw)) {
      return(NULL)
    }
    shiny_ui_dictionary_radiobuttons(
      rv$names_user_raw,
      ns("names_user_field"),
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
      ns("names_user_standard"),
      "Standard Names",
      "userStandard",
      rv$names_user_raw[2]
    )
  })
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