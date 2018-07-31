library(shiny)
library(shinyBS)
shinyServer(function(input, output, session) {

    showModal(modalDialog(
        title = h3("Welcome to Darwinizer!"),
        p("Darwinize Your Data"),
        img(src = "www/bdverse.png", align = "center"),
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
            rv$data_User <- data.table::fread(input$pathInputData$datapath,
                                              data.table = FALSE)
        })
        rv$names_User <- rv$names_UserAfter <- colnames(rv$data_User)
    })

    observeEvent(input$pathInputDictonary, {
        if (is.null(input$pathInputDictonary)) {
            return(NULL)
        }
        d <- data.table::fread(input$pathInputDictonary$datapath, 
                               data.table = FALSE)
        if (ncol(d) == 2 & 
            any(grepl("fieldname", colnames(d), ignore.case = TRUE)) &
            any(grepl("standard", colnames(d), ignore.case = TRUE))) {
            rv$dic_User <- d
        } else {
            warning("Data was wrong format")
        }
    })

    observeEvent(input$submitToDarwinizer, {
        library(data.table)
        rv$dic_Darwinizer <- data.table::fread("/Users/pogibas/work/bdDwC/data/dw.csv")
        rv$dic_Darwinizer <- rv$dic_Darwinizer[standard != ""]
        rv$dic_Rename <- rbind(rv$dic_User, rv$dic_Darwinizer[, c("fieldname", "standard")])
        rv$names_Standard <- unique(rv$dic_Darwinizer$standard)
        rv$names_StandardAfter <- unique(rv$dic_Darwinizer$standard)
        rv$data_Darwinized <- darwinazeNames(rv$data_User, rv$dic_Rename)

        # Chechboxes
        rv$data_Rename <- data.frame(nameOld = rv$data_Darwinized$fieldname,
                        nameNew = rv$data_Darwinized$standard,
                        nameRename = NA)
        rv$data_Rename$nameRename <- as.character(apply(rv$data_Rename[, 1:2], 1, paste, collapse = "  "))
        rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
        rv$names_UserAfter <- rv$names_User[!rv$names_User %in% rv$data_Rename$nameOld]

    })


    output$names_User <- renderUI({
        radioButtons("names_User_radio", 
                           "User Names",
                           sort(rv$names_UserAfter))
    })
    output$names_Standard <- renderUI({
        RAW <- radioButtons("names_Standard_radio", 
                           "Stand Names",
                           sort(rv$names_StandardAfter))
        for(i in sort(rv$names_StandardAfter)) {
            RAW <- gsub(paste0('<span>', i, '</span>'), 
                        paste0('<span id="DWC_', i, '">', i, '</span>'), 
                        RAW)
        }
        HTML(RAW)
    })
    output$names_Renamed <- renderUI({
        checkboxGroupInput("names_Renamed", 
                           "Renamed",
                           # Use rev to have newest on top
                           rev(rv$data_Rename$nameRename))
    })

   observeEvent(input$names_Rename, {
        rv$data_Rename <- rbind(rv$data_Rename,
              data.frame(nameOld = input$names_User_radio, 
                        nameNew = input$names_Standard_radio,
                        nameRename = NA))
        rv$data_Rename$nameRename <- as.character(apply(rv$data_Rename[, 1:2], 1, paste, collapse = "  "))
        rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
        rv$names_UserAfter <- rv$names_User[!rv$names_User %in% rv$data_Rename$nameOld]
    })
    observeEvent(input$names_Remove, {
        foo <- !rv$data_Rename$nameRename %in% input$names_Renamed
        rv$data_Rename <- rv$data_Rename[foo, ]
        rv$data_Rename$nameRename <- as.character(apply(rv$data_Rename[, 1:2], 1, paste, collapse = "  "))
        rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
        rv$names_UserAfter <- rv$names_User[!rv$names_User %in% rv$data_Rename$nameOld]
    })
   observeEvent(input$names_Clean, {
        rv$data_Rename <- data.frame()
        rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
        rv$names_UserAfter <- rv$names_User[!rv$names_User %in% rv$data_Rename$nameOld]
    })
   observeEvent(input$names_Reverse, {
        rv$data_Rename <- data.frame(nameOld = rv$data_Darwinized$fieldname,
                                     nameNew = rv$data_Darwinized$standard,
                                     nameRename = NA)
        rv$data_Rename$nameRename <- as.character(apply(rv$data_Rename[, 1:2], 1, paste, collapse = "  "))
        rv$names_StandardAfter <- rv$names_Standard[!rv$names_Standard %in% rv$data_Rename$nameNew]
        rv$names_UserAfter <- rv$names_User[!rv$names_User %in% rv$data_Rename$nameOld]
    })


    darwinizeInfo <- getDarwinzeInfo()
    output$names_Standard_Hover <- renderUI({
        result <- list()
        for(i in sort(rv$names_StandardAfter)) {
            bar <- subset(darwinizeInfo, name == i)$definition
            if (length(bar) == 0) {
                bar <- "NO"
            }
            result[[i]] <- bsTooltip(paste0("DWC_", i), bar, "right", "hover")
        }
        do.call(tagList, result)
    })
})