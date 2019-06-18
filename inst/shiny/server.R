options(
  # Debug text (full path)
  shiny.fullstacktrace = TRUE,
  # Let bigger input files
  shiny.maxRequestSize = 50 * 1024 ^ 2,
  # Debug visual
  shiny.reactlog = TRUE
)

shiny::shinyServer(function(input, output, session) {

  # Automatically stop a Shiny app when closing the browser tab
  session$onSessionEnded(shiny::stopApp)

  # --------------------------
  # META
  # --------------------------

  # All reactive values that we use within app
  rv <- shiny_server_reactivevalues()
  # Modals dialogs
  # We have to keep this on top as it contains welcoming modal
  shiny::callModule(module_server_modal, "main")
  # Disable darwinizer tab if no data submitted
  shiny::observe({
    shiny_server_tab_darwinizer(rv$data_user)
  })

  # --------------------------
  # UPLOAD DATA
  # --------------------------

  # Upload local file
  rv <- shiny::callModule(
    module_server_upload_local,
    "main",
    rv
  )
  # Download from the remote database
  rv <- shiny::callModule(
    module_server_upload_database,
    "main",
    rv
  )

  # --------------------------
  # DICTIONARY
  # --------------------------

  # Update dictionary information (date, file name)
  rv <- shiny::callModule(module_ui_dictionary, "main", rv)
  # Create radiobuttons for users dictionary
  shiny::callModule(module_ui_dictionary_radiobuttons, "main", rv)
  # Update radiobuttons while selecting field & standard names
  shiny::callModule(
    module_ui_dictionary_radiobuttons_update,
    "main",
    rv
  )

  # --------------------------
  # DARWINIZER
  # --------------------------

  # Enable darwnizer and other buttons
  shiny::callModule(module_ui_buttons, "main", rv)
  # Perform darwnizer
  rv <- shiny::callModule(
    module_server_darwinizer,
    "main",
    rv,
    parent = session
  )
  # Checkboxes with names
  shiny::callModule(module_ui_checkbox, "main", rv)
  # Buttons to perform filtering
  rv <- shiny::callModule(module_server_buttons_rename, "main", rv)
  rv <- shiny::callModule(module_server_buttons_remove, "main", rv)
  rv <- shiny::callModule(module_server_buttons_clean, "main", rv)
  rv <- shiny::callModule(module_server_buttons_rollback, "main", rv)
  shiny::callModule(module_server_buttons_download, "main", rv)
  # Value boxes
  shiny::callModule(module_ui_valuebox, "main", rv)

})