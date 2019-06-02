options(
  # debug text (full path)
  shiny.fullstacktrace = TRUE,
  # let bigger input files
  shiny.maxRequestSize = 50 * 1024 ^ 2,
  # debug visual
  shiny.reactlog = TRUE
)

shiny::shinyServer(function(input, output, session) {

  # Automatically stop a Shiny app when closing the browser tab
  session$onSessionEnded(shiny::stopApp)

  # --------------------------
  # META
  # --------------------------
  # All reactive values that we use within app
  rv <- bdDwC:::shiny_server_reactivevalues()
  # Modals dialogs
  # We have to keep this on top as it contains welcoming modal
  shiny::callModule(bdDwC:::module_server_modals, "main")
  # Disable darwinizer tab if no data submitted
  # No module as it's too complicated (and not needed) with Css classes
  shiny::observe({
    if (nrow(rv$data_user) == 0) {
      shinyjs::addCssClass(
        selector = "a[data-value='darwinizer']",
        class = "inactiveLink"
      )
    } else {
      shinyjs::removeCssClass(
        selector = "a[data-value='darwinizer']",
        class = "inactiveLink"
      )
    }
  })


  # --------------------------
  # UPLOAD DATA
  # --------------------------
  # Upload local file
  rv <- shiny::callModule(
    bdDwC:::module_server_upload_local,
    "main",
    rv
  )
  # Download from the remote database
  rv <- shiny::callModule(
    bdDwC:::module_server_upload_database,
    "main",
    rv
  )


  # --------------------------
  # USER DICTIONARY
  # --------------------------
  # Create radiobuttons for users field name column
  shiny::callModule(bdDwC:::module_ui_dictionary_radiobuttons, "main", rv)
  # Update radiobuttons while selecting field & standard names
  shiny::callModule(
    bdDwC:::module_ui_dictionary_radiobuttons_update,
    "main",
    rv
  )
  # Update dictionary information (date, file name)
  shiny::callModule(bdDwC:::module_ui_dictionary, "main", rv)


  # --------------------------
  # DARWINIZER
  # --------------------------
  # Enable darwnizer
  shiny::callModule(bdDwC:::module_ui_buttons, "main", rv)
  # Perform darwnizer
  rv <- shiny::callModule(
    bdDwC:::module_server_darwinizer,
    "main",
    rv,
    parent = session
  )
  # Checkboxes with names
  shiny::callModule(bdDwC:::module_ui_checkbox, "main", rv)
  # Buttons to perform filtering
  rv <- shiny::callModule(bdDwC:::module_server_buttons_rename, "main", rv)
  rv <- shiny::callModule(bdDwC:::module_server_buttons_remove, "main", rv)
  rv <- shiny::callModule(bdDwC:::module_server_buttons_clean, "main", rv)
  rv <- shiny::callModule(bdDwC:::module_server_buttons_rollback, "main", rv)
  shiny::callModule(bdDwC:::module_server_buttons_download, "main", rv)
  # Value boxes
  shiny::callModule(bdDwC:::module_ui_valuebox, "main", rv)
})