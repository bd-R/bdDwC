# Not imported even specified though in DESCRIPTION
library(shinydashboard)
library(shinyBS)
library(bdDwC)
options(shiny.maxRequestSize = 50 * 1024 ^ 2)

shinyServer(function(input, output, session) {

    # Automatically stop a Shiny app when closing the browser tab
    session$onSessionEnded(stopApp)


    # --------------------------
    # MODAL
    # --------------------------

    showModal(modalDialog(
        title = h3("Welcome to Darwinizer!"),
        p("Darwinize Your Data"),
        img(src = "bdverse.png", align = "center", width = "570"),
        helpText("GPL-3 License ©Tomer Gueta, Vijay Barve, Povilas Gibas, 
                  Thiloshon Nagarajah, Ashwin Agrawal and Carmel Yohay (2018).",
                 br(),
                 "bdDwC. R package version 0.1.21"
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
        data_User            = data.frame(), 
        # Darwinized data (created with darwinizeNames)
        data_Darwinized      = data.frame(),
        # Data that contains all renamings
        data_Rename          = data.frame(),
        # Darwin Cloud Data (standard and fieldname)
        data_DarwinCloud     = bdDwC:::dataDarwinCloud$data,
        # Darwin Cloud Information (used to display info when hover)
        data_DarwinCloudInfo = data.frame(),
        # Original set of names in user data
        names_User           = c(),
        # Set of names in user data after renaming
        names_UserAfter      = c(),
        # Original set of Darwin Cloud names
        names_Standard       = c(),
        # Set of Darwin Cloud names after renaming
        names_StandardAfter  = c(),
        # ------
        # DC DICTIONARY 
        # ------
        info_DCdate          = bdDwC:::dataDarwinCloud$date,
        # USER DICTIONARY
        # User original dictionary 
        # Uploaded by user (csv)
        dic_UserRaw          = data.frame(),
        # Names in user original dictionary used to create radio buttons
        names_UserRaw        = c(),
        # Subset of users dictionary 
        # Subset made using column names specified by user
        dic_User             = data.frame()
    )



    # --------------------------
    # DISABLE BUTTONS
    # --------------------------

    # # Disable darwinizer tab
    shinyjs::addCssClass(selector = "a[data-value='darwinizer']", 
                         class = "inactiveLink")

    # Disable Darwinize button if no user data uploaded
    observe({
        if (nrow(rv$data_User) == 0) {
            shinyjs::disable("submitToDarwinizer")
        } else {
            shinyjs::enable("submitToDarwinizer") 
        }
    })
    # Disable all other buttons if not submitted to Darwinizer
    observeEvent(input$submitToDarwinizer, {
        shinyjs::removeCssClass(selector = "a[data-value='darwinizer']", 
                                class = "inactiveLink")
        shinyjs::enable("names_Rename") 
        shinyjs::enable("names_Remove") 
        shinyjs::enable("names_Clean") 
        shinyjs::enable("names_Rollback") 
        shinyjs::enable("downloadData") 
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



    # --------------------------
    # UPLOAD USER DATA
    # --------------------------

    # Upload local file
    observeEvent(input$pathInputData, {
        withProgress(message = paste("Reading", input$pathInputData$name, "..."), {
            if (is.null(input$pathInputData)) {
                return("No data to view")
            }
            if (grepl("zip", tolower(input$pathInputData$type))) {
                message("Reading DWCA ZIP...")
                rv$data_User <- finch::dwca_read(input$pathInputData$datapath, read = TRUE)$data[[1]]
            } else {
                rv$data_User <- data.table::fread(input$pathInputData$datapath, data.table = FALSE)
            }
        })
        rv$names_User <- rv$names_UserAfter <- colnames(rv$data_User)
    })

    # Download from database
    observeEvent(input$queryDatabase, {
        withProgress(message = paste("Querying", input$queryDB, "..."), {
            if (input$queryDB == "gbif") {
                rv$data_User <- rgbif::occ_search(
                        scientificName = input$scientificName,
                        limit = input$recordSize,
                        hasCoordinate = switch(input$hasCoords,
                                               "1" = TRUE,
                                               "2" = FALSE,
                                               "3" = NULL
                       )
                )$data
            } else {
                warnings <- capture.output(
                    data <- spocc::occ(
                                query = input$scientificName,
                                from = input$queryDB,
                                limit = input$recordSize,
                                has_coords = switch(input$hasCoords,
                                                    "1" = TRUE,
                                                    "2" = FALSE,
                                                    "3" = NULL
                                )
                            ),
                    type = "message"
                )
                if (length(warnings) > 0) {
                    showNotification(paste(warnings, collapse = " "),
                                     duration = 6)
                }
                rv$data_User <- data[[input$queryDB]]$data[[1]]
            }
        })
        # Get column names (used for Darwinizer)
        rv$names_User <- rv$names_UserAfter <- colnames(rv$data_User)
    })



    # --------------------------
    # USER DICTIONARY
    # --------------------------

    # Upload user dictionary
    observeEvent(input$pathInputDictionary, {
        # Dictionary
        rv$dic_UserRaw <- read.csv(input$pathInputDictionary$datapath)
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
            for (i in rv$names_UserRaw) {
                RAW <- gsub(paste0("<span>", i, "</span>"), 
                            paste0("<span id=\"userField_", i, "\">", i, "</span>"), 
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
            for (i in rv$names_UserRaw) {
                RAW <- gsub(paste0("<span>", i, "</span>"), 
                            paste0("<span id=\"userStandard_", i, "\">", i, "</span>"), 
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
                                           which(result), ") label"))
        # Enable all non marked buttons in current box
        shinyjs::enable(selector = paste0("#names_User_Field .radio:nth-child(", 
                                           which(!result), ") label"))

    })
    # If button in field is marked
    observeEvent(input$names_User_Field, {
        # Which button was marked
        result <- grepl(input$names_User_Field, rv$names_UserRaw)
        # We need double action (PG: I don't know why)
        # Disable marked button in opposite box
        shinyjs::disable(selector = paste0("#names_User_Standard .radio:nth-child(", 
                                           which(result), ") label"))
        # Enable all non marked buttons in current box
        shinyjs::enable(selector = paste0("#names_User_Standard .radio:nth-child(", 
                                           which(!result), ") label"))
    })



    # --------------------------
    # UPDATED DC DICTIONARY
    # --------------------------

    # Update DC dictionary
    observeEvent(input$updateDarwinCloud, {
        rv$data_DarwinCloud <- downloadCloudData()
        rv$info_DCdate <- Sys.Date()
    })
    # Information about dictionaries
    # This code is in server part because of mix of reactive and html text
    output$dicInfo <- renderUI({
        # Is user dictionary uploaded
        uploadDictionary <- !is.null(input$pathInputDictionary)
        # Select icon
        userDicIcon <- ifelse(uploadDictionary > 0, "check", "unchecked") 
        if (uploadDictionary) {
            # Get name for user dictionary
            userDicFile <- paste0("(",
                                 sub(".csv$", "", 
                                     basename(input$pathInputDictionary$name)),
                                  ")")

        } else {
            userDicFile <- NULL
        }
        res <- paste0(
            "<b>Used dictionaries:</b>
            <br/>
            <i class='glyphicon glyphicon-check fa-1x'></i>
            Darwin Cloud (version: ", format(rv$info_DCdate, "%d-%B-%Y"), ")

            <button class='btn btn-default action-button' id='popDC'
                    style='width: 1px; border-color: #ffffff; 
                           background-color: #ffffff; 
                           font-size:100%' type='button'>
                <i class='glyphicon glyphicon-question-sign'></i>
            </button>

            <br/>
            <i class='glyphicon glyphicon-", userDicIcon, " fa-1x'></i>
            Personal Dictionary ", userDicFile,
            "<button class='btn btn-default action-button' id='popDic'
                    style='width: 1px; border-color: #ffffff; 
                           background-color: #ffffff; 
                           font-size:100%' type='button'>
                <i class='glyphicon glyphicon-question-sign'></i>
            </button>"
        )
        HTML(res)
    })
    # Information about Darwin Cloud
    observeEvent(input$popDC, {
        showModal(modalDialog(
            title = h3("Darwin Cloud Data"),
            tags$p("bdDwC uses Darwin Core Dictionary which is stored on official",
                   tags$a(href = "https://github.com/kurator-org/kurator-validation", 
                          "Kurator's repository."),
                   br(),
                   "If you want to update Darwin Core version for your analysis click",
                   tags$b("Update DC"), "button bellow."),
            size = "m",
            easyClose = TRUE
        ))
    })
    # Information about User dictionary
    observeEvent(input$popDic, {
        showModal(modalDialog(
            title = h3("Personal Dictionary File"),
            tags$p("File with columns that contain fieldname and standard name"),
            size = "m",
            easyClose = TRUE
        ))
    })
    # Text that shows up if user uploaded dictionary
    output$userDicText <- renderUI({
        if (!is.null(input$pathInputDictionary)) {
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
    observeEvent(input$submitToDarwinizer, {

        # Jump to Darwinizer tab
        updateTabItems(session, "myTabs", "darwinizer")

        # Download Darwin Core information
        rv$data_DarwinCloudInfo <- bdDwC:::getDarwinCoreInfo()

        # If user has uploaded dictionary
        if (nrow(rv$dic_UserRaw) > 0) {
            # Update reactive user dictionary
            rv$dic_User <- subset(rv$dic_UserRaw, select = c(input$names_User_Field, input$names_User_Standard))
            colnames(rv$dic_User) <- c("fieldname", "standard")
        }

        # Get all standard names
        rv$names_Standard <- unique(rv$data_DarwinCloud$standard)
        rv$names_StandardAfter <- unique(rv$data_DarwinCloud$standard)

        # Run Darwinizer with user and reference dictionary
        rv$data_Darwinized <- darwinizeNames(rv$data_User, 
                                             rbind(rv$dic_User, rv$data_DarwinCloud))

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
            for (i in sort(rv$names_StandardAfter)) {
                RAW <- gsub(paste0("<span>", i, "</span>"), 
                            paste0("<span id=\"DWC_", i, "\">", i, "</span>"), 
                            RAW)
            }
            HTML(RAW)
        }
    })
    output$names_Renamed_Manual <- renderUI({
        if (length(rv$data_Rename$nameRename) == 0) {
            h5("Nothing was renamed")
        } else {
            foo <- subset(rv$data_Rename, matchType == "Manual")$nameRename
            if (length(foo) > 0) {
                checkboxGroupInput("names_Renamed_Manual", 
                                   NULL,
                                   # Use rev to have newest on top
                                   rev(foo)
                )
            } else {
                h5("Nothing was renamed")
            }
        }
    })
    output$names_Renamed_Darwinized <- renderUI({
        if (length(rv$data_Rename$nameRename) == 0) {
            h5("No names were Darwinized")
        } else {
            foo <- subset(rv$data_Rename, matchType == "Darwinized")$nameRename
            if (length(foo) > 0) {
                checkboxGroupInput("names_Renamed_Darwinized", 
                                   NULL,
                                   # Use rev to have newest on top
                                   foo
                )
            } else {
                h5("No names were Darwinized")
            }
        }
    })
    output$names_Renamed_Identical <- renderUI({
        if (length(rv$data_Rename$nameRename) == 0) {
            h5("No names were Identical")
        } else {
            foo <- subset(rv$data_Rename, matchType == "Identical")$nameRename
            if (length(foo) > 0) {
                checkboxGroupInput("names_Renamed_Identical", 
                                   NULL,
                                   # Use rev to have newest on top
                                   foo
                )
            } else {
                h5("No names were Identical")
            }
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
            format(Sys.time(), "darwinizedData_%Y_%b_%d_%X.RDS")
        },
        content = function(file) {
            saveRDS(renameUserData(rv$data_User, rv$data_Rename), file)
        }
    )


    # --------------------------
    # VALUE BOXES
    # --------------------------

    output$vb_allNames <- renderValueBox({
        valueBox(length(rv$names_User),
                 "Names Submitted", 
                 color = "light-blue")
    })
    output$vb_DWCNames <- renderValueBox({
        valueBox(paste0(nrow(rv$data_Rename), 
                        "  (", round(nrow(rv$data_Rename) * 100 / 
                                     length(rv$names_User)), "%)"),
                 "Names Darwinized", 
                 color = "olive")
    })
    output$vb_DWCident <- renderValueBox({
        valueBox(sum(rv$data_Rename$matchType == "Identical"), 
                 "Darwinized: Identical", 
                 color = "green")
    })
    output$vb_DWCmatch <- renderValueBox({
        valueBox(sum(rv$data_Rename$matchType == "Darwinized"), 
                 "Darwinized: Matched", 
                 color = "green")
    })
    output$vb_Manual <- renderValueBox({
        valueBox(sum(rv$data_Rename$matchType == "Manual"), 
                 "Darwinized: Manually", 
                 color = "green")
    })



    # --------------------------
    # DARWIN CORE INFO
    # --------------------------

    output$names_Standard_Hover <- renderUI({
        result <- list()
        # For each name extract Darwin Core information
        for (i in rv$names_StandardAfter) {
            # Extract information
            info <- subset(rv$data_DarwinCloudInfo, name == i)$definition
            if (length(info) == 0) {
                info <- NULL
            }
            # Append information as a tool tip
            result[[i]] <- shinyBS::bsTooltip(paste0("DWC_", i), info, "right", "hover")
        }
        do.call(tagList, result)
    })



    # --------------------------
    # CITATION
    # --------------------------

    observeEvent(input$citation, {
        showModal(modalDialog(
            title = "Cite us",
            HTML(paste("bdverse will be published soon!")),
            easyClose = TRUE
            )
        )
    })

})