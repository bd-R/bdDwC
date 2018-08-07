fluidPage(title = "Darwinizer",
    sidebarLayout(

        sidebarPanel(
            fileInput("pathInputData", "Choose dataset",
                      multiple = FALSE,
                      c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
            shinyjs::useShinyjs(),
            actionButton("submitToDarwinizer", "Submit to Darwinizer",
                         style = "color: #000000; background-color: #71a879; border-color: #091520"),
            uiOutput("submitToDarwinizer_Pop"),
            br(), br(),
            fileInput("pathInputDictionary", "Choose dictionary",
                      multiple = FALSE,
                      c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
            uiOutput("names_User_Field"), uiOutput("names_User_Standard")
        ),

        mainPanel(
            fluidRow(column(2, 
                shinyjs::disabled(actionButton("names_Rename", "Rename",
                             icon("arrow-circle-right"),                                                 
                             style = "color: #000000; background-color: #71a879; border-color: #091520")), 
                offset = 2)
            ),
            column(2, uiOutput("names_User")),
            column(2, uiOutput("names_Standard"), 
                      uiOutput("names_Standard_Hover"), offset = 1),
            column(2, uiOutput("names_Renamed"), offset = 1),
            column(2, verticalLayout(
                shinyjs::disabled(actionButton("names_Remove", "Remove selected rename",
                             icon("times"),
                             style = "color: #000000; background-color: #a188bd; border-color: #091520")), 
                br(),
                shinyjs::disabled(actionButton("names_Clean", "Remove all renames",
                             icon("times"),
                             style = "color: #000000; background-color: #a188bd; border-color: #091520")), 
                br(),
                shinyjs::disabled(actionButton("names_Rollback", "Rollback to Darwinizer",
                             icon("fast-backward"),
                             style = "color: #000000; background-color: #c4cc6d; border-color: #091520")), 
                br(),
                shinyjs::disabled(downloadButton("downloadData", "Download Darwinized data",
                               style = "color: #000000; background-color: #71a879; border-color: #091520"))),
                offset = 1
            )
        )
    )
)