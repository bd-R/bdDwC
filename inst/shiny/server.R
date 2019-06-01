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
  # REACTIVE VALUES
  # --------------------------
  # All reactive values that we use within app
  rv <- bdDwC:::shiny_server_reactivevalues()


  # --------------------------
  # MISC
  # --------------------------
  # Modals dialogs
  # We have to keep this on top as it's a welcoming modal
  shiny::callModule(bdDwC:::module_server_modals, "main")
  # Information about the Darwin Cloud
  # No module as created within shiny_ui_dictionary
  shiny::observeEvent(input$pop_dc, {
    bdDwC:::shiny_server_modal_custom(
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
  # No module as created within shiny_ui_dictionary
  shiny::observeEvent(input$pop_dic, {
    bdDwC:::shiny_server_modal_custom(
      shiny::h3("Personal Dictionary File"),
      shiny::tags$p("File with columns fieldname and standard name")
    )
  })

  # Disable/Enable buttons
  # Disable darwinizer tab if no data submitted
  # No module as it's too complicated with Css classes
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
  # Download from database
  rv <- shiny::callModule(
    bdDwC:::module_server_upload_database,
    "main",
    rv
  )


  # --------------------------
  # USER DICTIONARY
  # --------------------------
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
  shiny::callModule(bdDwC:::module_ui_buttons, "main", rv)

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