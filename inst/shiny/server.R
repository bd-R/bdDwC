options(shiny.maxRequestSize = 50 * 1024 ^ 2)

shiny::shinyServer(function(input, output, session) {

  # Automatically stop a Shiny app when closing the browser tab
  session$onSessionEnded(shiny::stopApp)


  # --------------------------
  # MODAL
  # --------------------------

  shiny::showModal(shiny::modalDialog(
    title = shiny::h3("Welcome to Darwinizer!"),
    shiny::p("Darwinize Your Data"),
    shiny::img(src = "bdverse.png", align = "center", width = "570"),
    shiny::helpText(
      "GPL-3 License ©Tomer Gueta, Vijay Barve, Povilas Gibas,
       Thiloshon Nagarajah, Ashwin Agrawal and Carmel Yohay (2019).",
       shiny::br(),
       "bdDwC. R package version 0.1.21"
    ),
    shiny::helpText(
      "Contribute: ",
      shiny::a("https://github.com/bd-R/bdDwC",
               href = "https://github.com/bd-R/bdDwC"),
      shiny::br(), "Join: ",
      shiny::a("https://bd-r-group.slack.com",
               href = "https://bd-r-group.slack.com")
    ),
    size = "m",
    easyClose = TRUE
  ))


  # --------------------------
  # REACTIVE VALUES
  # --------------------------
  # Showing all reactive values that are used in app

  rv <- shiny::reactiveValues(
    # User data used in Darwinizer
    # Uploaded by user (csv)
    data_user = data.frame(),
    # Darwinized data (created with darwinize_names)
    data_darwinized = data.frame(),
    # Data that contains all renamings
    data_rename = data.frame(),
    # Darwin Cloud Data (standard and fieldname)
    data_darwin_cloud = bdDwC:::data_darwin_cloud$data,
    # Darwin Cloud Information (used to display info when hover)
    data_darwin_cloud_info = data.frame(),
    # Original set of names in user data
    names_user = c(),
    # Set of names in user data after renaming
    names_user_after = c(),
    # Original set of Darwin Cloud names
    names_standard = c(),
    # Set of Darwin Cloud names after renaming
    names_standard_after = c(),
    # ------
    # DC DICTIONARY
    # ------
    info_dc_date = bdDwC:::data_darwin_cloud$date,
    # USER DICTIONARY
    # User original dictionary
    # Uploaded by user (csv)
    dic_user_raw = data.frame(),
    # Names in user original dictionary used to create radio buttons
    names_user_raw = c(),
    # Subset of users dictionary
    # Subset made using column names specified by user
    dic_user = data.frame()
  )

  # --------------------------
  # DISABLE BUTTONS
  # --------------------------

  # # Disable darwinizer tab
  shinyjs::addCssClass(selector = "a[data-value='darwinizer']",
                       class = "inactiveLink")

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
    shinyjs::removeCssClass(selector = "a[data-value='darwinizer']",
                            class = "inactiveLink")
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
  shiny::observeEvent(input$path_input_data, {
    shiny::withProgress(message =
      paste("Reading", input$path_input_data$name, "..."), {
        if (is.null(input$path_input_data)) {
          return("No data to view")
        }
        if (grepl("zip", tolower(input$path_input_data$type))) {
          message("Reading DWCA ZIP...")
          rv$data_user <- finch::dwca_read(input$path_input_data$datapath,
                                           read = TRUE)$data[[1]]
        } else {
          rv$data_user <- data.table::fread(input$path_input_data$datapath,
                                            data.table = FALSE)
        }
    })
    rv$names_user <- rv$names_user_after <- colnames(rv$data_user)
  })

  # Download from database
  shiny::observeEvent(input$query_database, {
    shiny::withProgress(message =
      paste("Querying", input$query_db, "..."), {
        if (input$query_db == "gbif") {
          rv$data_user <- rgbif::occ_search(
            scientificName = input$scientific_name,
            limit = input$record_size,
            hasCoordinate = switch(input$has_coords,
                                   "1" = TRUE, "2" = FALSE, "3" = NULL)
          )$data
        } else {
          warnings <- capture.output(
            data <- spocc::occ(
              query = input$scientific_name,
              from = input$query_db,
              limit = input$record_size,
              has_coords = switch(input$has_coords,
                                  "1" = TRUE, "2" = FALSE, "3" = NULL)
            ),
              type = "message"
          )
          if (length(warnings) > 0) {
            shiny::showNotification(
              paste(warnings, collapse = " "), duration = 5
            )
          }
          rv$data_user <- data[[input$query_db]]$data[[1]]
        }
    })
    # Get column names (used for Darwinizer)
    rv$names_user <- rv$names_user_after <- colnames(rv$data_user)
  })


  # --------------------------
  # USER DICTIONARY
  # --------------------------

  # Upload user dictionary
  shiny::observeEvent(input$path_input_dictionary, {
    # Dictionary
    rv$dic_user_raw <- read.csv(input$path_input_dictionary$datapath)
    # Columns
    rv$names_user_raw <- sort(colnames(rv$dic_user_raw))
  })

  # Created radiobuttons for users field name column
  output$names_user_field <- shiny::renderUI({
    # If data is uploaded
    if (nrow(rv$dic_user_raw) == 0) {
      return(NULL)
    } else {
      # Main function to create radio buttons
      res <- shiny::radioButtons("names_user_field", "Field Names",
                                 rv$names_user_raw, rv$names_user_raw[1])
      # For each name change ID
      # We need individual IDs so we can disable them with shinyjs
      # We need to disable them as same ID can't be field and standard
      for (i in rv$names_user_raw) {
        res <- gsub(paste0("<span>", i, "</span>"),
                    paste0("<span id=\"userField_", i, "\">", i, "</span>"),
                    res
        )
      }
      shiny::HTML(res)
    }
  })

  # Created radiobuttons for users standard name column
  output$names_user_standard <- shiny::renderUI({
    # If data is uploaded
    if (nrow(rv$dic_user_raw) == 0) {
      return(NULL)
    } else {
      # Main function to create radio buttons
      res <- shiny::radioButtons("names_user_standard", "Standard Names",
                                 rv$names_user_raw, rv$names_user_raw[2])
      # For each name change ID
      # We need individual IDs so we can disable them with shinyjs
      # We need to disable them as same ID can't be field and standard
      for (i in rv$names_user_raw) {
        res <- gsub(paste0("<span>", i, "</span>"),
                    paste0("<span id=\"userStandard_", i, "\">", i, "</span>"),
                    res
        )
      }
      shiny::HTML(res)
    }
  })

  # If button in standard is marked
  shiny::observeEvent(input$names_user_standard, {
    # Which button was marked
    result <- grepl(input$names_user_standard, rv$names_user_raw)
    # We need double action (PG: I don't know why)
    # Disable marked button in opposite box
    shinyjs::disable(selector = paste0(
      "#names_user_field .radio:nth-child(", which(result), ") label"))
    # Enable all non marked buttons in current box
    shinyjs::enable(selector = paste0(
      "#names_user_field .radio:nth-child(", which(!result), ") label"))
  })
  # If button in field is marked
  shiny::observeEvent(input$names_user_field, {
    # Which button was marked
    result <- grepl(input$names_user_field, rv$names_user_raw)
    # We need double action (PG: I don't know why)
    # Disable marked button in opposite box
    shinyjs::disable(selector = paste0(
      "#names_user_standard .radio:nth-child(", which(result), ") label")
    )
    # Enable all non marked buttons in current box
    shinyjs::enable(selector = paste0(
      "#names_user_standard .radio:nth-child(", which(!result), ") label")
    )
  })


  # --------------------------
  # UPDATED DC DICTIONARY
  # --------------------------

  # Update DC dictionary
  shiny::observeEvent(input$update_darwin_cloud, {
    rv$data_darwin_cloud <- bdDwC::download_cloud_data()
    rv$info_dc_date <- Sys.Date()
  })
  # Information about dictionaries
  # This code is in server part because of mix of reactive and html text
  output$dic_info <- shiny::renderUI({
    # Is user dictionary uploaded
    upload_dictionary <- !is.null(input$path_input_dictionary)
    # Select icon
    user_dic_icon <- ifelse(upload_dictionary > 0, "check", "unchecked")
    if (upload_dictionary) {
      # Get name for user dictionary
      user_dic_file <- paste0(
        "(", sub(".csv$", "", basename(input$path_input_dictionary$name)), ")"
      )
    } else {
      user_dic_file <- NULL
    }
    res <- paste0(
      "<b>Used dictionaries:</b>
      <br/>
      <i class='glyphicon glyphicon-check fa-1x'></i>
      Darwin Cloud (version: ", format(rv$info_dc_date, "%d-%B-%Y"), ")

      <button class='btn btn-default action-button' id='pop_dc'
              style='width: 1px; border-color: #ffffff;
                     background-color: #ffffff;
                     font-size:100%' type='button'>
          <i class='glyphicon glyphicon-question-sign'></i>
      </button>

      <br/>
      <i class='glyphicon glyphicon-", user_dic_icon, " fa-1x'></i>
      Personal Dictionary ", user_dic_file,
      "<button class='btn btn-default action-button' id='pop_dic'
              style='width: 1px; border-color: #ffffff;
                     background-color: #ffffff;
                     font-size:100%' type='button'>
          <i class='glyphicon glyphicon-question-sign'></i>
      </button>"
    )
    shiny::HTML(res)
  })
  # Information about Darwin Cloud
  shiny::observeEvent(input$pop_dc, {
    shiny::showModal(shiny::modalDialog(
      title = shiny::h3("Darwin Cloud Data"),
      tags$p(
        "bdDwC uses Darwin Core Dictionary (stored on official",
        tags$a(
          href = "https://github.com/kurator-org/kurator-validation",
          "Kurator's repository)."
        ),
        shiny::br(),
        "Update Darwin Core version for your analysis by clicking",
        tags$b("Update DC"), "button bellow."
      ),
      size = "m",
      easyClose = TRUE
    ))
  })
  # Information about User dictionary
  shiny::observeEvent(input$pop_dic, {
    shiny::showModal(shiny::modalDialog(
      title = shiny::h3("Personal Dictionary File"),
      tags$p("File with columns fieldname and standard name"),
      size = "m",
      easyClose = TRUE
    ))
  })
  # Text that shows up if user uploaded dictionary
  output$user_dic_text <- shiny::renderUI({
    if (!is.null(input$path_input_dictionary)) {
        tags$b("Select field and standard names")
    } else {
        NULL
    }
  })



  # --------------------------
  # DARWINIZER
  # --------------------------
  # Run Darwinizer

  # When Darwinizer button is clicked
  shiny::observeEvent(input$submit_to_darwinizer, {

    # Jump to Darwinizer tab
    shinydashboard::updateTabItems(session, "my_tabs", "darwinizer")

    # Download Darwin Core information
    rv$data_darwin_cloud_info <- bdDwC:::get_darwin_core_info()

    # If user has uploaded dictionary
    if (nrow(rv$dic_user_raw) > 0) {
      # Update reactive user dictionary
      rv$dic_user <- subset(rv$dic_user_raw,
        select = c(input$names_user_field, input$names_user_standard))
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
      rv$data_rename$name_rename <- bdDwC:::combine_old_new(rv$data_rename)
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

  # Create checkbox with current user names
  output$names_user <- shiny::renderUI({
    if (length(rv$names_user_after) == 0) {
      return(NULL)
    } else {
      shiny::radioButtons(
        "names_user_radio", "User Names", sort(rv$names_user_after)
      )
    }
  })
  # Create checkbox with current standard names
  output$names_standard <- shiny::renderUI({
    if (length(rv$names_standard_after) == 0) {
      return(NULL)
    } else {
      res <- shiny::radioButtons(
        "names_standard_radio", "Stand Names", sort(rv$names_standard_after)
      )
      # Adding unique ID so we can add info boxes with additional info
      for (i in sort(rv$names_standard_after)) {
        res <- gsub(paste0("<span>", i, "</span>"),
                    paste0("<span id=\"DWC_", i, "\">", i, "</span>"),
                    res
        )
      }
      shiny::HTML(res)
    }
  })
  output$names_renamed_manual <- shiny::renderUI({
    if (length(rv$data_rename$name_rename) == 0) {
      shiny::h5("Nothing was renamed")
    } else {
      foo <- subset(rv$data_rename, match_type == "Manual")$name_rename
      if (length(foo) > 0) {
        # Use rev to have newest on top
        shiny::checkboxGroupInput( "names_renamed_manual", NULL, rev(foo))
      } else {
        shiny::h5("Nothing was renamed")
      }
    }
  })
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


  # --------------------------
  # BUTTONS
  # --------------------------

  # RENAMED
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
    rv$data_rename$name_rename <- bdDwC:::combine_old_new(rv$data_rename)
    # Updated (remove name) from standard names
    rv$names_standard_after <- rv$names_standard[
      !rv$names_standard %in% rv$data_rename$name_new
    ]
    # Updated (remove name) from user names
    rv$names_user_after <- rv$names_user[
      !tolower(rv$names_user) %in% tolower(rv$data_rename$name_old)
    ]
  })

  # REMOVE
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

  # ROLLBACK
  # This is the same as part in Darwinize (should refactor)
  shiny::observeEvent(input$names_rollback, {
    if (nrow(rv$data_darwinized) > 0) {
      rv$data_rename <- rv$data_darwinized
      rv$data_rename$name_rename <- bdDwC:::combine_old_new(rv$data_rename)
      rv$names_standard_after <- rv$names_standard[
        !rv$names_standard %in% rv$data_rename$name_new
      ]
      rv$names_user_after <- rv$names_user[
        !tolower(rv$names_user) %in% tolower(rv$data_rename$name_old)
      ]
    }
  })

  # DONWLOAD
  output$download_data <- shiny::downloadHandler(
    filename = function() {
      format(Sys.time(), "darwinizedData_%Y_%b_%d_%X.RDS")
    },
    content = function(file) {
      saveRDS(bdDwC::rename_user_data(rv$data_user, rv$data_rename), file)
    }
  )


  # --------------------------
  # VALUE BOXES
  # --------------------------

  output$vb_all_names <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      length(rv$names_user), "Names Submitted", color = "light-blue"
    )
  })
  output$vb_dwc_names <- shinydashboard::renderValueBox({
    foo <- paste0(
      nrow(rv$data_rename),
      " (", round(nrow(rv$data_rename) * 100 / length(rv$names_user)), "%)"
    )
    shinydashboard::valueBox(foo, "Names Darwinized", color = "olive")
  })
  output$vb_dwc_ident <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      sum(rv$data_rename$match_type == "Identical"),
      "Darwinized: Identical", color = "green")
  })
  output$vb_dwc_match <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      sum(rv$data_rename$match_type == "Darwinized"),
      "Darwinized: Matched", color = "green")
  })
  output$vb_manual <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      sum(rv$data_rename$match_type == "Manual"),
      "Darwinized: Manually", color = "green")
  })


  # --------------------------
  # DARWIN CORE INFO
  # --------------------------

  output$names_standard_hover <- shiny::renderUI({
    result <- list()
    # For each name extract Darwin Core information
    for (i in rv$names_standard_after) {
      # Extract information
      info <- subset(rv$data_darwin_cloud_info, name == i)$definition
      if (length(info) == 0) {
          info <- NULL
      }
      # Append information as a tool tip
      result[[i]] <- shinyBS::bsTooltip(
        paste0("DWC_", i), info, "right", "hover"
      )
    }
    do.call(shiny::tagList, result)
  })


  # --------------------------
  # CITATION
  # --------------------------

  shiny::observeEvent(input$citation, {
    shiny::showModal(shiny::modalDialog(
      title = "Cite us",
      shiny::HTML(paste("bdverse will be published soon!")),
      easyClose = TRUE
      )
    )
  })

})