fluidPage(title = "Darwinizer",

    includeCSS("www/style.css"),

    sidebarLayout(


        sidebarPanel(
            fileInput("pathInputData", "Choose dataset",
                      multiple = FALSE,
                      c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
            shinyjs::useShinyjs(),
            actionButton("submitToDarwinizer", "Submit to Darwinizer", width = 210,
                         style = "color: #000000; background-color: #71a879; border-color: #091520"),
            uiOutput("submitToDarwinizer_Pop"),
            br(), br(),
            shinyBS::bsCollapsePanel("Upload Dictionary",
                fileInput("pathInputDictionary", "Choose dictionary file",
                          multiple = FALSE,
                          c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
                splitLayout(uiOutput("names_User_Field"), 
                            uiOutput("names_User_Standard"),
                            cellWidths = 200,
                            cellArgs = list(style = "padding: 6px"))
            ),

            # Restart shiny session
            tags$a(href = "javascript:history.go(0)", 
                   popify(tags$i(class = "fa fa-refresh fa-1x"),
                   title = NULL, 
                   content = "Click here to restart bdDwC shiny session",
                   placement = "right"))
        ),

        mainPanel(
            fluidRow(column(2, 
                shinyjs::disabled(actionButton("names_Rename", "Rename",
                                  incon = icon("arrow-circle-right"), width = 210,                                     
                                  style = "color: #000000; background-color: #71a879; border-color: #091520")), 
                offset = 2)
            ),
            column(2, uiOutput("names_User")),
            column(2, uiOutput("names_Standard"), 
                      uiOutput("names_Standard_Hover"), offset = 1),
            column(2, uiOutput("names_Renamed"), offset = 1),
            column(2, verticalLayout(
                shinyjs::disabled(actionButton("names_Remove", "Remove selected rename",
                                               icon = icon("times"), width = 210,
                                               style = "color: #000000; background-color: #a188bd; border-color: #091520")), 
                br(),
                shinyjs::disabled(actionButton("names_Clean", "Remove all renames",
                                              icon = icon("times"), width = 210,
                                              style = "color: #000000; background-color: #a188bd; border-color: #091520")), 
                br(),
                shinyjs::disabled(actionButton("names_Rollback", "Rollback to Darwinizer",
                                               icon = icon("fast-backward"), width = 210,
                                               style = "color: #000000; background-color: #c4cc6d; border-color: #091520")), 
                br(),
                shinyjs::disabled(downloadButton("downloadData", "Download Darwinized data",
                                                 with = 210,
                                                 style = "color: #000000; background-color: #71a879; border-color: #091520"))),
                offset = 1
            )
        )
    )
)