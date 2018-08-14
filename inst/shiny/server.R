shinyServer(function(input, output, session) {

    # --------------------------
    # MODAL
    # --------------------------

    showModal(modalDialog(
        title = h3("Welcome to Darwinizer!"),
        p("Darwinize Your Data"),
        img(src = "bdverse.png", align = "center", width = "570"),
        helpText("MIT License ©Tomer Gueta, Vijay Barve, Povilas Gibas, 
                  Thiloshon Nagarajah, Ashwin Agrawal and Carmel Yohay (2018).",
                 br(),
                 "bdDwC. R package version 1.0.0"
        ),
        helpText("Contribute: ",
                 a("https://github.com/bd-R/bdDwC", 
                   href = "https://github.com/bd-R/bdDwC"),
                 br(), 
                 "Join: ",
                 a("https://bd-r-group.slack.com", 
                   href = "https://bd-r-group.slack.com")
        ), 
        size = "m",
        easyClose = TRUE
    ))



    # --------------------------
    # REACTIVE VALUES
    # --------------------------
    # Showing all reactive values that are used in app

    rv <- reactiveValues(
        # User data used in Darwinizer
        # Uploaded by user (csv)
        data_User           = data.frame(), 
        # Darwinized data (created with darwinazeNames)
        data_Darwinized     = data.frame(),
        # Data that contains all renamings
        data_Rename         = data.frame(),
        # Darwin Cloud Information (used to display info when hover)
        data_DarwinCore     = data.frame(),
        # Original set of names in user data
        names_User          = c(),
        # Set of names in user data after renaming
        names_UserAfter     = c(),
        # Original set of Darwin Cloud names
        names_Standard      = c(),
        # Set of Darwin Cloud names after renaming
        names_StandardAfter = c(),
        # ------
        # USER DICTIONARY
        # User original dictionary 
        # Uploaded by user (csv)
        dic_UserRaw         = data.frame(),
        # Names in user original dictionary used to create radio buttons
        names_UserRaw       = c(),
        # Subset of users dictionary 
        # Subset made using column names specified by user
        dic_User            = data.frame()
    )



    # --------------------------
    # DISABLE BUTTONS
    # --------------------------

    # Disable Darwinize button if no user data uploaded
    observe({
        if (nrow(rv$data_User) == 0) {
            shinyjs::disable("submitToDarwinizer")
        } else {
            shinyjs::enable("submitToDarwinizer") 
        }
    })
    # Disable renaming when no names left
    observe({
        if ((length(rv$names_UserAfter) == 0 | 
            length(rv$names_StandardAfter) == 0) &
            nrow(rv$data_Rename > 0)) {
            shinyjs::disable("names_Rename") 
        }
        if (length(rv$names_UserAfter) > 0) {
            shinyjs::enable("names_Rename") 
        }
    })
    # Disable rollback when no nothing was darwinized
    observe({
        if (length(rv$data_Darwinized$nameOld) == 0) {
            shinyjs::disable("names_Rollback") 
        }
    })
    # Disable all other buttons if not submitted to Darwinizer
    observeEvent(input$submitToDarwinizer, {
        shinyjs::enable("names_Rename") 
        shinyjs::enable("names_Remove") 
        shinyjs::enable("names_Clean") 
        shinyjs::enable("names_Rollback") 
        shinyjs::enable("downloadData") 
    })



    # --------------------------
    # UPLOAD USER DATA
    # --------------------------

    # Upload local file
    observeEvent(input$pathInputData, {
        withProgress(message = paste("Loading", input$pathInputData, "..."), {
            if (is.null(input$pathInputData)) {
                return(NULL)
            }
            # Load user data
            rv$data_User <- data.table::fread(input$pathInputData$datapath)
        })
        # Get column names (used for Darwinizer)
        rv$names_User <- rv$names_UserAfter <- colnames(rv$data_User)
    })
    # Download from database
    observeEvent(input$queryDatabase, {
        withProgress(message = paste("Querying", input$queryDB, "..."), {
            rv$data_User <- spocc::occ(input$scientificName, input$queryDB,
                                       input$recordSize)[[input$queryDB]]$data[[1]]
        })
        # Get column names (used for Darwinizer)
        rv$names_User <- rv$names_UserAfter <- colnames(rv$data_User)
    })



    # --------------------------
    # DICTIONARY
    # --------------------------

    # Upload user dictionary
    observeEvent(input$pathInputDictionary, {
        # Dictionary
        rv$dic_UserRaw <- data.table::fread(input$pathInputDictionary$datapath)
        # Columns
        rv$names_UserRaw <- sort(colnames(rv$dic_UserRaw))
    })

    # Created radiobuttons for users field name column
    output$names_User_Field <- renderUI({
        # If data is uploaded
        if (nrow(rv$dic_UserRaw) == 0) {
            return(NULL)
        } else {
            # Main function to create radio buttons
            RAW <- radioButtons("names_User_Field", 
                                "Field Names",
                                rv$names_UserRaw,
                                rv$names_UserRaw[1])
            # For each name change ID
            # We need individual IDs so we can disable them with shinyjs
            # We need to disable them as same ID can't be field and standard
            for(i in rv$names_UserRaw) {
                RAW <- gsub(paste0('<span>', i, '</span>'), 
                            paste0('<span id="userField_', i, '">', i, '</span>'), 
                            RAW)
            }
            HTML(RAW)
        }
    })

    # Created radiobuttons for users standard name column
    output$names_User_Standard <- renderUI({
        # If data is uploaded
        if (nrow(rv$dic_UserRaw) == 0) {
            return(NULL)
        } else {
            # Main function to create radio buttons
            RAW <- radioButtons("names_User_Standard", 
                                "Standard Names",
                                rv$names_UserRaw,
                                rv$names_UserRaw[2])
            # For each name change ID
            # We need individual IDs so we can disable them with shinyjs
            # We need to disable them as same ID can't be field and standard
            for(i in rv$names_UserRaw) {
                RAW <- gsub(paste0('<span>', i, '</span>'), 
                            paste0('<span id="userStandard_', i, '">', i, '</span>'), 
                            RAW)
            }
            HTML(RAW)
        }
    })

    # If button in standard is marked
    observeEvent(input$names_User_Standard, {
        # Which button was marked
        result <- grepl(input$names_User_Standard, rv$names_UserRaw)
        # We need double action (PG: I don't know why)
        # Disable marked button in opposite box
        shinyjs::disable(selector = paste0("#names_User_Field .radio:nth-child(", 
                                           which(result),") label"))
        # Enable all non marked buttons in current box
        shinyjs::enable(selector = paste0("#names_User_Field .radio:nth-child(", 
                                           which(!result),") label"))

    })
    # If button in field is marked
    observeEvent(input$names_User_Field, {
        # Which button was marked
        result <- grepl(input$names_User_Field, rv$names_UserRaw)
        # We need double action (PG: I don't know why)
        # Disable marked button in opposite box
        shinyjs::disable(selector = paste0("#names_User_Standard .radio:nth-child(", 
                                           which(result),") label"))
        # Enable all non marked buttons in current box
        shinyjs::enable(selector = paste0("#names_User_Standard .radio:nth-child(", 
                                           which(!result),") label"))
    })



    # --------------------------
    # DARWINIZER
    # --------------------------
    # Run Darwinizer

    # When Darwinizer button is clicked
    observeEvent(input$submitToDarwinizer, {

        # Jump to Darwinizer tab
        updateTabItems(session, "myTabs", "darwinizer")

        # Download Darwin Core information
        rv$data_DarwinCore <- bdDwC:::getDarwinCoreInfo()

        # If user has uploaded dictionary
        if (nrow(rv$dic_UserRaw) > 0) {
            # Update reactive user dictionary
            rv$dic_User <- subset(rv$dic_UserRaw, select = c(input$names_User_Field, input$names_User_Standard))
            colnames(rv$dic_User) <- c("fieldname", "standard")
        }

        # Get all standard names
        rv$names_Standard <- unique(bdDwC:::dataDarwinCloud$data$standard)
        rv$names_StandardAfter <- unique(bdDwC:::dataDarwinCloud$data$standard)

        # Run Darwinizer with user and reference dictionary
        rv$data_Darwinized <- bdDwC:::darwinazeNames(
            rv$data_User, rbind(rv$dic_User, bdDwC:::dataDarwinCloud$data))

        # Checkboxes
        # Update if something was darwinized
        if (nrow(rv$data_Darwinized) > 0) {
            rv$data_Rename <- rv$data_Darwinized
            rv$data_Rename$nameRename <- bdDwC:::combineOldNew(rv$data_Rename)
            # Updated (remove name) from standard names
            rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
            # Updated (remove name) from user names
            rv$names_UserAfter <- rv$names_User[!tolower(rv$names_User) %in% tolower(rv$data_Rename$nameOld)]
        }
    })



    # --------------------------
    # CHECKBOXES
    # --------------------------

    # Create checkbox with current user names
    output$names_User <- renderUI({
        if (length(rv$names_UserAfter) == 0) {
            return(NULL)
        } else {
            radioButtons("names_User_radio", 
                               "User Names",
                               sort(rv$names_UserAfter))
        }
    })
    # Create checkbox with current standard names
    output$names_Standard <- renderUI({
        if (length(rv$names_StandardAfter) == 0) {
            return(NULL)
        } else {
            RAW <- radioButtons("names_Standard_radio", 
                               "Stand Names",
                               sort(rv$names_StandardAfter))
            # Adding unique ID so we can add info boxes with additional info
            for(i in sort(rv$names_StandardAfter)) {
                RAW <- gsub(paste0('<span>', i, '</span>'), 
                            paste0('<span id="DWC_', i, '">', i, '</span>'), 
                            RAW)
            }
            HTML(RAW)
        }
    })
    # Create renamed checkboxes - combination of 3 renaming types
    output$names_Renamed <- renderUI({
        if (length(rv$data_Rename$nameRename) == 0) {
            return(NULL)
        } else {
            res1 <- shinyBS::bsCollapsePanel("Manually Renamed",
                checkboxGroupInput("names_Renamed_Manual", 
                                   NULL,
                                   # Use rev to have newest on top
                                   rev(subset(rv$data_Rename, matchType == "Manual")$nameRename))
            )
            res2 <- shinyBS::bsCollapsePanel("Darwinized Names",
                checkboxGroupInput("names_Renamed_Darwinized", 
                                   NULL,
                                   # Use rev to have newest on top
                                   subset(rv$data_Rename, matchType == "Darwinized")$nameRename)
            )
            res3 <- shinyBS::bsCollapsePanel("Identical Matches",
                checkboxGroupInput("names_Renamed_Identical", 
                                   NULL,
                                   # Use rev to have newest on top
                                   subset(rv$data_Rename, matchType == "Identical")$nameRename)
            )

            shinyBS::bsCollapse(res1, res2, res3,
                                multiple = TRUE, open = c("Darwinized Names", 
                                                          "Manually Renamed"))
        }
    })



    # --------------------------
    # BUTTONS
    # --------------------------

    # RENAMED
    # This is very similar what happens with Darwinizer part
    # Should refactor this in the future
    observeEvent(input$names_Rename, {
        # Update renamed dataset
        rv$data_Rename$nameRename <- NULL
        rv$data_Rename <- rbind(rv$data_Rename,
                                data.frame(nameOld = input$names_User_radio, 
                                           nameNew = input$names_Standard_radio,
                                           matchType = "Manual",
                                           stringsAsFactors = FALSE))
        # Create (combine) renamed name
        rv$data_Rename$nameRename <- bdDwC:::combineOldNew(rv$data_Rename)
        # Updated (remove name) from standard names
        rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
        # Updated (remove name) from user names
        rv$names_UserAfter <- rv$names_User[!tolower(rv$names_User) %in% tolower(rv$data_Rename$nameOld)]
    })

    # REMOVE
    observeEvent(input$names_Remove, {
        rmNames <- c()
        if (length(input$names_Renamed_Manual) > 0) {
            rmNames <- c(rmNames, input$names_Renamed_Manual)
        }
        if (length(input$names_Renamed_Darwinized) > 0) {
            rmNames <- c(rmNames, input$names_Renamed_Darwinized)
        }
        if (length(input$names_Renamed_Identical) > 0) {
            rmNames <- c(rmNames, input$names_Renamed_Identical)
        }
        # Remove input from renamed names dataset
        rv$data_Rename <- rv$data_Rename[!rv$data_Rename$nameRename %in% rmNames, ]
        # Update standard names checkbox
        rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
        # Update user names checkbox
        rv$names_UserAfter <- rv$names_User[!tolower(rv$names_User) %in% tolower(rv$data_Rename$nameOld)]
    })

    # Clean all renamings
    observeEvent(input$names_Clean, {
        rv$data_Rename <- data.frame()
        rv$names_StandardAfter <- rv$names_Standard
        rv$names_UserAfter <- rv$names_User
    })

    # ROLLBACK
    # This is the same as part in Darwinize (should refactor)
    observeEvent(input$names_Rollback, {
        if (nrow(rv$data_Darwinized) > 0) {
            rv$data_Rename <- rv$data_Darwinized
            rv$data_Rename$nameRename <- bdDwC:::combineOldNew(rv$data_Rename)
            rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
            rv$names_UserAfter <- rv$names_User[!tolower(rv$names_User) %in% tolower(rv$data_Rename$nameOld)]
        }
    })

    # DONWLOAD
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0("Darwinized-", Sys.Date(), ".RDS")
        },
        content = function(con) {
            # Rename user data using current renaming dataset
            saveRDS(bdDwC:::renameUserData(rv$data_User, rv$data_Rename), con)
        }
    )



    # --------------------------
    # DARWIN CORE INFO
    # --------------------------

    output$names_Standard_Hover <- renderUI({
        result <- list()
        # For each name extract Darwin Core information
        for(i in rv$names_StandardAfter) {
            # Extract information
            info <- subset(rv$data_DarwinCore, name == i)$definition
            if (length(info) == 0) {
                info <- NULL
            }
            # Append information as a tool tip
            result[[i]] <- shinyBS::bsTooltip(paste0("DWC_", i), info, "right", "hover")
        }
        do.call(tagList, result)
    })



    # --------------------------
    # POP UP FOR DICTIONARY
    # --------------------------

    output$submitToDarwinizer_Pop <- renderUI({
        text <- paste("bdDwC uses references dictionary downloaded from the github.com/kurator-org/kurator-validation, last update at",
                      bdDwC:::dataDarwinCloud[[2]], ". But you can also add your own dictionary to the bdDwC using file input slot bellow.")
        bsPopover("submitToDarwinizer", title = "Add you own dictionary", text)
    })
})