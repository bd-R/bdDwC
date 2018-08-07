fluidPage(title = "Darwinizer",
    sidebarLayout(

        sidebarPanel(
            fileInput("pathInputData", "Choose dataset",
                      multiple = FALSE,
                      c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
            fileInput("pathInputDictionary", "Choose dictionary",
                      multiple = FALSE,
                      c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
            uiOutput("names_User_Field"), uiOutput("names_User_Standard"),
            actionButton("submitToDarwinizer", "Submit to Darwinizer")
        ),

        mainPanel(
            fluidRow(column(2, 
                actionButton("names_Rename", "Rename",
                             icon("arrow-circle-right"),                                                 
                             style = "color: #000000; background-color: #66c2a5; border-color: #091520"), 
                offset = 2)
            ),
            column(2, uiOutput("names_User")),
            column(2, uiOutput("names_Standard"), 
                      uiOutput("names_Standard_Hover"), offset = 1),
            column(2, uiOutput("names_Renamed"), offset = 1),
            column(2, verticalLayout(
                actionButton("names_Remove", "Remove selected rename",
                             icon("times"),
                             style = "color: #000000; background-color: #fbb4ae; border-color: #091520"), 
                actionButton("names_Clean", "Remove all renames",
                             icon("times"),
                             style = "color: #000000; background-color: #e41a1c; border-color: #091520"), 
                actionButton("names_Rollback", "Rollback to Darwinizer",
                             icon("fast-backward"),
                             style = "color: #000000; background-color: #ffffb3; border-color: #091520"), 
                downloadButton("downloadData", "Download Darwinized data",
                               style = "color: #000000; background-color: #66c2a5; border-color: #091520")),
                offset = 1
            )
        )
    )
)