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
  shinyjs::addCssClass(
    selector = "a[data-value='darwinizer']",
    class = "inactiveLink"
  )
  # Disable Darwinize button if no user data uploaded
  shiny::observe({
    if (nrow(rv$data_user) == 0) {
        shinyjs::disable("submit_to_darwinizer")
    } else {
        shinyjs::enable("submit_to_darwinizer")
    }
  })
  # Disable all other buttons if not submitted to Darwinizer
  shiny::observeEvent(input$submit_to_darwinizer, {
    shinyjs::removeCssClass(
      selector = "a[data-value='darwinizer']",
      class = "inactiveLink"
    )
    shinyjs::enable("names_rename")
    shinyjs::enable("names_remove")
    shinyjs::enable("names_clean")
    shinyjs::enable("names_rollback")
    shinyjs::enable("download_data")
  })
  # Disable renaming when no names left
  shiny::observe({
    # Check if there are still names left
    foo <- length(rv$names_user_after) == 0 |
           length(rv$names_standard_after) == 0
    bar <- nrow(rv$data_rename) > 0
    if (foo & bar) {
      shinyjs::disable("names_rename")
    }
    if (length(rv$names_user_after) > 0) {
      shinyjs::enable("names_rename")
    }
  })
  # Disable rollback when no nothing was darwinized
  shiny::observe({
    if (length(rv$data_darwinized$name_old) == 0) {
      shinyjs::disable("names_rollback")
    }
  })


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


  # --------------------------
  # DARWINIZER
  # --------------------------
  # Run Darwinizer

  # When Darwinizer button is clicked
  shiny::observeEvent(input$submit_to_darwinizer, {

    # Jump to Darwinizer tab
    shinydashboard::updateTabItems(session, "my_tabs", "darwinizer")

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
      rv$data_rename$name_rename <- bdDwC:::link_old_new(rv$data_rename)
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


  # --------------------------
  # CHECKBOXES
  # --------------------------
  shiny::callModule(bdDwC:::module_ui_checkbox, "main", rv)


  # --------------------------
  # BUTTONS
  # --------------------------

  # renamed
  # This is very similar what happens with Darwinizer part
  # Should refactor this in the future
  shiny::observeEvent(input$names_rename, {
    # Update renamed dataset
    rv$data_rename$name_rename <- NULL
    rv$data_rename <- rbind(
      rv$data_rename,
      data.frame(name_old = input$names_user_radio,
                 name_new = input$names_standard_radio,
                 match_type = "Manual",
                 stringsAsFactors = FALSE)
    )
    # Create (combine) renamed name
    rv$data_rename$name_rename <- bdDwC:::link_old_new(rv$data_rename)
    # Updated (remove name) from standard names
    rv$names_standard_after <- rv$names_standard[
      !rv$names_standard %in% rv$data_rename$name_new
    ]
    # Updated (remove name) from user names
    rv$names_user_after <- rv$names_user[
      !tolower(rv$names_user) %in% tolower(rv$data_rename$name_old)
    ]
  })

  # remove
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

  # Clean all renamings
  shiny::observeEvent(input$names_clean, {
    rv$data_rename <- data.frame()
    rv$names_standard_after <- rv$names_standard
    rv$names_user_after <- rv$names_user
  })

  # Rollback
  # This is the same as part in Darwinize (should refactor)
  shiny::observeEvent(input$names_rollback, {
    if (nrow(rv$data_darwinized) > 0) {
      rv$data_rename <- rv$data_darwinized
      rv$data_rename$name_rename <- bdDwC:::link_old_new(rv$data_rename)
      rv$names_standard_after <- rv$names_standard[
        !rv$names_standard %in% rv$data_rename$name_new
      ]
      rv$names_user_after <- rv$names_user[
        !tolower(rv$names_user) %in% tolower(rv$data_rename$name_old)
      ]
    }
  })

  # download
  output$download_data <- bdDwC:::shiny_server_download_renamed(
    rv$data_user,
    rv$data_rename
  )


  # --------------------------
  # CREATE UI
  # --------------------------

  # Value boxes
  output$vb_all_names <- bdDwC:::shiny_ui_valuebox(
    length(rv$names_user), "Names Submitted", "light-blue"
  )
  output$vb_dwc_names <- bdDwC:::shiny_ui_valuebox(
    paste0(
      nrow(rv$data_rename),
      " (", round(nrow(rv$data_rename) * 100 / length(rv$names_user)), "%)"
    ),
    "Names Darwinized",
    "olive"
  )
  output$vb_dwc_ident <- bdDwC:::shiny_ui_valuebox(
    sum(rv$data_rename$match_type == "Identical"),
    "Darwinized: Identical",
    "green"
  )
  output$vb_dwc_match <- bdDwC:::shiny_ui_valuebox(
    sum(rv$data_rename$match_type == "Darwinized"),
    "Darwinized: Matched",
    "green"
  )
  output$vb_manual <- bdDwC:::shiny_ui_valuebox(
    sum(rv$data_rename$match_type == "Manual"),
    "Darwinized: Manually",
    "green"
  )

  # Darwin core definition
  output$names_standard_hover <- shiny::renderUI({
    do.call(shiny::tagList, bdDwC:::shiny_ui_darwin_core_definition())
  })

})