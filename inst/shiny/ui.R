library(shiny)
library(shinyBS)
fluidPage(

    titlePanel("Darwinizer"),

    tabsetPanel(type = "pills",

        tabPanel("Upload Data",
            fileInput("pathInputData",
                      "Choose a csv file",
                      multiple = FALSE,
                      c("text/csv", ".csv",
                        "text/comma-separated-values,text/plain")),
            fileInput("pathInputDictonary",
                      "Choose a csv file",
                      multiple = FALSE,
                      c("text/csv", ".csv",
                        "text/comma-separated-values,text/plain")),
            actionButton("submitToDarwinizer", 
                         "Submit to Darwinizer")
        ),

        tabPanel("Darwinizer",
            column(2, uiOutput("names_User")),
            column(2, uiOutput("names_Standard"), 
                      uiOutput("names_Standard_Hover")),
            actionButton("names_Rename", "RENAME"),
            actionButton("names_Remove", "REMOVE"),
            actionButton("names_Clean", "CLEAN"),
            actionButton("names_Reverse", "REVERSE"),
            column(2, uiOutput("names_Renamed"))
        ),

        tabPanel("Results")

    )
)