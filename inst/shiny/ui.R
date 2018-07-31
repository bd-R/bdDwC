library(shiny)
library(shinyBS)
fluidPage(

    titlePanel("Darwinizer"),

    tabsetPanel(type = "pills",

        tabPanel("Upload Data",
            fileInput("pathInputData", "Choose dataset",
                      multiple = FALSE,
                      c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
            fileInput("pathInputDictionary", "Choose dictionary",
                      multiple = FALSE,
                      c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
            actionButton("submitToDarwinizer", "Submit to Darwinizer")
        ),

        tabPanel("Darwinizer",
            fluidRow(
              actionButton("names_Rename", "Rename"),
              actionButton("names_Remove", "Remove selected rename"),
              actionButton("names_Clean", "Remove all renames"),
              actionButton("names_Rollback", "Rollback to Darwinizer")),
            column(2, uiOutput("names_User")),
            column(2, uiOutput("names_Standard"), 
                      uiOutput("names_Standard_Hover"), offset = 1),
            column(2, uiOutput("names_Renamed"), offset = 1)
        ),

        tabPanel("Results",
            downloadButton("downloadData", "Download Darwinized data"))

    )
)