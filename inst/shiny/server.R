options(
  # let bigger input files
  shiny.maxRequestSize = 50 * 1024 ^ 2,
  # debug text (full path)
  shiny.fullstacktrace = TRUE,
  # debug visual
  shiny.reactlog = TRUE
)

shiny::shinyServer(function(input, output, session) {

  # Automatically stop a Shiny app when closing the browser tab
  session$onSessionEnded(shiny::stopApp)

  # --------------------------
  # REACTIVE VALUES
  # --------------------------
  # All reactive values that we use within app
  rv <- shiny::reactiveValues(
    # User data used in Darwinizer
    data_user = data.frame(),
    # Darwinized data (created with darwinize_names)
    data_darwinized = data.frame(),
    # Data that contains all renamings
    data_rename = data.frame(),
    # Darwin Cloud Data (standard and fieldname)
    data_darwin_cloud = bdDwC:::data_darwin_cloud$data,
    # Original set of names in user data
    names_user = c(),
    # Set of names in user data after renaming
    names_user_after = c(),
    # Original set of Darwin Cloud names
    names_standard = c(),
    # Set of Darwin Cloud names after renaming
    names_standard_after = c(),
    # Dictionary version (date)
    info_dc_date = bdDwC:::data_darwin_cloud$date,
    # User original dictionary
    dic_user_raw = data.frame(),
    # Names in user original dictionary used to create radio buttons
    names_user_raw = c(),
    # Subset of users dictionary
    # Subset made using column names specified by user
    dic_user = data.frame()
  )


  # --------------------------
  # MODAL DIALOGS
  # --------------------------
  # We have to keep this on top as welcoming modal is produced here
  shiny::callModule(bdDwC:::module_server_modals, "main")
  # Information about the Darwin Cloud
  # No modal as created within shiny_ui_dictionary
  shiny::observeEvent(input$pop_dc, {
    bdDwC:::shiny_server_modal_cloud()
  })
  # Information about user dictionary
  # No modal as created within shiny_ui_dictionary
  shiny::observeEvent(input$pop_dic, {
    bdDwC:::shiny_server_modal_dictionary()
  })


  # --------------------------
  # DISABLE BUTTONS
  # --------------------------
  # Disable darwinizer tab
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
  shiny::callModule(bdDwC:::module_ui_buttons, "main", rv)


  # --------------------------
  # UPLOAD USER DATA
  # --------------------------
  # Upload local file
  rv <- shiny::callModule(
    bdDwC:::module_server_upload_local,
    "main",
    rv
  )
  # Download from database
  rv <- shiny::callModule(
    bdDwC:::module_server_upload_database,
    "main",
    rv
  )


  # --------------------------
  # USER DICTIONARY
  # --------------------------
  # Upload user dictionary
  rv <- shiny::callModule(
    bdDwC:::module_server_upload_dictionary,
    "main",
    rv
  )
  # Creat radiobuttons for users field name column
  shiny::callModule(
    bdDwC:::module_ui_dictionary_radiobuttons_field,
    "main",
    rv
  )
  # Update dictionary information
  shiny::callModule(
    bdDwC:::module_ui_dictionary,
    "main",
    rv
  )
  # If button in standard is marked
  shiny::observeEvent(input$names_user_standard, {
    # Which button was marked
    result <- grepl(input$names_user_standard, rv$names_user_raw)
    # Disable marked button in a opposite box
    shinyjs::disable(selector = paste0(
      "#names_user_field .radio:nth-child(", which(result), ") label"
    ))
    # Enable all non marked buttons in current box
    shinyjs::enable(selector = paste0(
      "#names_user_field .radio:nth-child(", which(!result), ") label"
    ))
  })
  # If button in field is marked
  shiny::observeEvent(input$names_user_field, {
    # Which button was marked
    result <- grepl(input$names_user_field, rv$names_user_raw)
    # Disable marked button in a opposite box
    shinyjs::disable(selector = paste0(
      "#names_user_standard .radio:nth-child(", which(result), ") label"
    ))
    # Enable all non marked buttons in current box
    shinyjs::enable(selector = paste0(
      "#names_user_standard .radio:nth-child(", which(!result), ") label"
    ))
  })

  # Darwnizer
  rv <- shiny::callModule(
    bdDwC:::module_server_darwinizer,
    "main",
    rv,
    parent = session
  )

  # Checkboxes
  shiny::callModule(bdDwC:::module_ui_checkbox, "main", rv)

  # Buttons
  rv <- shiny::callModule(bdDwC:::module_server_buttons_rename, "main", rv)
  rv <- shiny::callModule(bdDwC:::module_server_buttons_remove, "main", rv)
  rv <- shiny::callModule(bdDwC:::module_server_buttons_clean, "main", rv)
  rv <- shiny::callModule(bdDwC:::module_server_buttons_rollback, "main", rv)
  shiny::callModule(bdDwC:::module_server_buttons_download, "main", rv)

  # Value boxes
  shiny::callModule(bdDwC:::module_ui_valuebox, "main", rv)

  ##DOESN:T WORK Darwin core definition !!!!!!!!!!!!!!!!!!!!!!
  output$names_standard_hover <- shiny::renderUI({
    do.call(shiny::tagList, bdDwC:::shiny_ui_darwin_core_definition())
  })

})