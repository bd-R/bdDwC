shinyServer(function(input, output, session) {

    showModal(modalDialog(
        title = h3("Welcome to Darwinizer!"),
        p("Darwinize Your Data"),
        img(src = "bdverse.png", align = "center", width = "570"),
        helpText(
            "MIT License ©Tomer Gueta, Vijay Barve, Povilas Gibas, Thiloshon Nagarajah, Ashwin Agrawal and Carmel Yohay (2018).",
            br(),
            "darwinizer. R package version 1.0.0."
        ),
        helpText(
            "Contribute: ",
            a("https://github.com/bd-R/bdDwC", href = "https://github.com/bd-R/bdDwC"),
            br(), "Join: ",
            a("https://bd-r-group.slack.com", href = "https://bd-r-group.slack.com")
        ), 
        size = "m",
        easyClose = TRUE
    ))


    rv <- reactiveValues(
        data_User           = data.frame(),
        data_Darwinized     = data.frame(),
        names_Standard      = c(),
        names_StandardAfter = c(),
        names_User          = c(),
        names_UserAfter     = c(),
        names_Renamed       = c(),
        names_UserRaw       = c(),
        dic_UserRaw         = data.frame(),
        dic_User            = data.frame(),
        dic_Darwinizer      = data.frame(),
        dic_Rename          = data.frame()
    )

    # User data main
    observeEvent(input$pathInputData, {
        withProgress(message = paste("Loading", input$pathInputData, "..."), {
            if (is.null(input$pathInputData)) {
                return(NULL)
            }
            rv$data_User <- data.table::fread(input$pathInputData$datapath)
        })
        rv$names_User <- rv$names_UserAfter <- colnames(rv$data_User)
    })

    output$submitToDarwinizer_Pop <- renderUI({
        text <- paste("bdDwC uses references dictionary downloaded from the github.com/kurator-org/kurator-validation, last update at",
                      bdDwC:::dataDarwinCloud[[2]], ". But you can also add your own dictionary to the bdDwC using file input slot bellow.")
        bsPopover("submitToDarwinizer", title = "Add you own dictionary", text)
    })

    observe({
        if (nrow(rv$data_User) > 0) {
            shinyjs::enable("submitToDarwinizer") 
        } else {
            shinyjs::disable("submitToDarwinizer")
        }
    })
    observeEvent(input$submitToDarwinizer, {
        shinyjs::enable("names_Rename") 
        shinyjs::enable("names_Remove") 
        shinyjs::enable("names_Clean") 
        shinyjs::enable("names_Rollback") 
        shinyjs::enable("downloadData") 
    })

    observeEvent(input$pathInputDictionary, {
        rv$dic_UserRaw <- data.table::fread(input$pathInputDictionary$datapath)
        rv$names_UserRaw <- sort(colnames(rv$dic_UserRaw))
    })

    output$names_User_Field <- renderUI({
        if (nrow(rv$dic_UserRaw) == 0) {
            return(NULL)
        } else {
            RAW <- radioButtons("names_User_Field", 
                                "Field Names",
                                rv$names_UserRaw,
                                1)
            for(i in rv$names_UserRaw) {
                RAW <- gsub(paste0('<span>', i, '</span>'), 
                            paste0('<span id="userField_', i, '">', i, '</span>'), 
                            RAW)
            }
            HTML(RAW)
        }
    })

    output$names_User_Standard <- renderUI({
        if (nrow(rv$dic_UserRaw) == 0) {
            return(NULL)
        } else {
            RAW <- radioButtons("names_User_Standard", 
                                "Standard Names",
                                rv$names_UserRaw,
                                2)
            for(i in rv$names_UserRaw) {
                RAW <- gsub(paste0('<span>', i, '</span>'), 
                            paste0('<span id="userStandard_', i, '">', i, '</span>'), 
                            RAW)
            }
            HTML(RAW)
        }
    })

    observeEvent(input$names_User_Standard, {
        result <- grepl(input$names_User_Standard, rv$names_UserRaw)
        shinyjs::disable(selector = paste0("#names_User_Field .radio:nth-child(", which(result),") label"))
        shinyjs::enable(selector = paste0("#names_User_Field .radio:nth-child(", which(!result),") label"))
    })
    observeEvent(input$names_User_Field, {
        result <- grepl(input$names_User_Field, rv$names_UserRaw)
        shinyjs::disable(selector = paste0("#names_User_Standard .radio:nth-child(", which(result),") label"))
        shinyjs::enable(selector = paste0("#names_User_Standard .radio:nth-child(", which(!result),") label"))
    })


    observeEvent(input$submitToDarwinizer, {

        if (nrow(rv$dic_UserRaw) > 0) {
            rv$dic_User <- subset(rv$dic_UserRaw, select = c(input$names_User_Field, input$names_User_Standard))
            colnames(rv$dic_User) <- c("fieldname", "standard")
        }

        rv$dic_Darwinizer <- bdDwC:::dataDarwinCloud$data
        rv$dic_Darwinizer <- subset(rv$dic_Darwinizer, standard != "")

        rv$names_Standard <- unique(rv$dic_Darwinizer$standard)
        rv$names_StandardAfter <- unique(rv$dic_Darwinizer$standard)

        rv$dic_Rename <- rbind(rv$dic_User, 
                               rv$dic_Darwinizer[, c("fieldname", "standard")])
        rv$data_Darwinized <- bdDwC:::darwinazeNames(rv$data_User, rv$dic_Rename)

        # Chechboxes
        # Update if something was darwinized
        if (nrow(rv$data_Darwinized) > 0) {
            rv$data_Rename <- data.frame(nameOld = rv$data_Darwinized$fieldname,
                                         nameNew = rv$data_Darwinized$standard,
                                         nameRename = NA,
                                         stringsAsFactors = FALSE)
            rv$data_Rename$nameRename <- as.character(apply(rv$data_Rename[, 1:2], 1, paste, collapse = "\n"))
            rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
            rv$names_UserAfter <- rv$names_User[!rv$names_User %in% rv$data_Rename$nameOld]
        }
    })


    output$names_User <- renderUI({
        if (length(rv$names_UserAfter) == 0) {
            return(NULL)
        } else {
            radioButtons("names_User_radio", 
                               "User Names",
                               sort(rv$names_UserAfter))
        }
    })
    output$names_Standard <- renderUI({
        if (length(rv$names_StandardAfter) == 0) {
            return(NULL)
        } else {
            RAW <- radioButtons("names_Standard_radio", 
                               "Stand Names",
                               sort(rv$names_StandardAfter))
            for(i in sort(rv$names_StandardAfter)) {
                RAW <- gsub(paste0('<span>', i, '</span>'), 
                            paste0('<span id="DWC_', i, '">', i, '</span>'), 
                            RAW)
            }
            HTML(RAW)
        }
    })
    output$names_Renamed <- renderUI({
        if (length(rv$data_Rename$nameRename) == 0) {
            return(NULL)
        } else {
            checkboxGroupInput("names_Renamed", 
                               "Renamed",
                               # Use rev to have newest on top
                               rev(rv$data_Rename$nameRename))
        }
    })

   observeEvent(input$names_Rename, {
        rv$data_Rename <- rbind(rv$data_Rename,
              data.frame(nameOld = input$names_User_radio, 
                        nameNew = input$names_Standard_radio,
                        nameRename = NA,
                        stringsAsFactors = FALSE))
        rv$data_Rename$nameRename <- as.character(apply(rv$data_Rename[, 1:2], 1, paste, collapse = "\n"))
        rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
        rv$names_UserAfter <- rv$names_User[!rv$names_User %in% rv$data_Rename$nameOld]
    })
    observeEvent(input$names_Remove, {
        foo <- !rv$data_Rename$nameRename %in% input$names_Renamed
        rv$data_Rename <- rv$data_Rename[foo, ]
        rv$data_Rename$nameRename <- as.character(apply(rv$data_Rename[, 1:2], 1, paste, collapse = "\n"))
        rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
        rv$names_UserAfter <- rv$names_User[!rv$names_User %in% rv$data_Rename$nameOld]
    })
   observeEvent(input$names_Clean, {
        rv$data_Rename <- data.frame()
        rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
        rv$names_UserAfter <- rv$names_User[!rv$names_User %in% rv$data_Rename$nameOld]
    })
   observeEvent(input$names_Rollback, {
        if (nrow(rv$data_Darwinized) > 0) {
            rv$data_Rename <- data.frame(nameOld = rv$data_Darwinized$fieldname,
                                         nameNew = rv$data_Darwinized$standard,
                                         nameRename = NA,
                                         stringsAsFactors = FALSE)
            rv$data_Rename$nameRename <- as.character(apply(rv$data_Rename[, 1:2], 1, paste, collapse = "\n"))
            rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
            rv$names_UserAfter <- rv$names_User[!rv$names_User %in% rv$data_Rename$nameOld]
        }
    })


    darwinizeInfo <- bdDwC:::getDarwinzeInfo()
    output$names_Standard_Hover <- renderUI({
        result <- list()
        for(i in sort(rv$names_StandardAfter)) {
            info <- subset(darwinizeInfo, name == i)$definition
            if (length(info) == 0) {
                info <- "NO"
            }
            result[[i]] <- bsTooltip(paste0("DWC_", i), info, "right", "hover")
        }
        do.call(tagList, result)
    })

    output$downloadData <- downloadHandler(
        filename = function() {
            paste0("Darwinized-", Sys.Date(), ".csv")
        },
        content = function(con) {
            write.csv(bdDwC:::renameUserData(rv$data_User, rv$data_Rename), 
                      con, row.names = FALSE, col.names = TRUE)
        }
    )
})