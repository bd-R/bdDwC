dashboardPage(

    # --------------------------
    # HEADER
    # --------------------------

    dashboardHeader(title = "bdDwC"),



    # --------------------------
    # SIDEBAR
    # --------------------------

    dashboardSidebar(
        sidebarMenu(
            id = "myTabs",
            tags$head(tags$style(".inactiveLink {
                                  pointer-events: none;
                                  cursor: default;}"
            )),
            menuItem("Upload Data", tabName = "upload"),
            menuItem("Darwinizer", tabName = "darwinizer"),
            # Restart shiny session
            tags$a(href = "javascript:history.go(0)", 
                   popify(tags$i(class = "fa fa-refresh fa-1x"),
                   title = NULL, 
                   content = "Click here to restart bdDwC shiny session",
                   placement = "right")),
            # Citation
            actionButton("citation", "Cite us", style = "border-color: #091520")
        )
    ),



    # --------------------------
    # BODY
    # --------------------------

    dashboardBody(
        shinyjs::useShinyjs(),
        includeCSS("www/style.css"),

        tabItems(

            # --------------------------
            # UPLOAD DATA
            # --------------------------

            tabItem("upload",
                fluidRow(
                    # Upload user data
                    box(title = "Upload Data", status = "warning", width = 5,
                        bsCollapse(multiple = FALSE, open = "From a Local File",
                               bsCollapsePanel("From a Local File", 
                                               fileInput("pathInputData",
                                                         "Choose a csv file",
                                                         multiple = FALSE,
                                                         c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")
                                               ),
                                               style = "info"

                               ),
                               bsCollapsePanel("From a Database",
                                               textInput("scientificName",
                                                         h3("Scientific Name:"),
                                                         "Puma concolor"),
                                               sliderInput("recordSize",
                                                           h3("Record Size:"),
                                                           500,
                                                           min = 0, max = 50000),
                                               radioButtons("queryDB",
                                                            h3("Online Database:"),
                                                            list("GBIF" = "gbif",
                                                                 "Bison" = "bison",
                                                                 "Inat" = "inat",
                                                                 "eBird" = "ebird",
                                                                 "Ecoengine" = "ecoengine",
                                                                 "Vertnet" = "vertnet"),
                                                            "gbif"
                                               ),
                                               br(),
                                               div(id = "queryDatabaseDiv",
                                                   class = "activeButton",
                                                   actionButton("queryDatabase", 
                                                                "Query Database", 
                                                                icon("download"))), 
                                               style = "success"
                               )
                        )
                    ),

                    # Upload user dictionary
                    box(title = "Upload Dictionary", width = 3,
                        fileInput("pathInputDictionary", "Choose a dictionary file",
                                  multiple = FALSE,
                                  c("text/csv", ".csv", "text/comma-separated-values,text/plain")
                        ),
                        shinyBS::bsCollapsePanel(
                            "Select field and standard names",
                            splitLayout(uiOutput("names_User_Field"), 
                                        uiOutput("names_User_Standard"),
                                        cellWidths = 200,
                                        cellArgs = list(style = "padding: 6px")
                            )
                        )
                    )
                ),

                actionButton("submitToDarwinizer", "Submit to Darwinizer", width = 250,
                             style = "background: url('Darwin.svg'); background-position: left center; 
                                      background-repeat: no-repeat; background-color: #ffffff;
                                      color: #000000; border-color: #091520;
                                      padding:10px; font-size:120%"),

                uiOutput("submitToDarwinizer_Pop")
            ),



            # --------------------------
            # DARWINIZER
            # --------------------------

            tabItem("darwinizer",
                fluidRow(
                    fluidRow(
                        column(12, 
                            valueBoxOutput("vb_allNames", width = 2), 
                            valueBoxOutput("vb_DWCNames", width = 2), 
                            valueBoxOutput("vb_DWCmatch", width = 2), 
                            valueBoxOutput("vb_Manual",   width = 2),
                            valueBoxOutput("vb_DWCident", width = 2),
                            offset = 1),
                        # Adds lines belowe value boxes
                        column(12, style = "margin-bottom:10px; border-bottom:2px solid")
                    ),
                    fluidRow(
                        column(2, 
                            br(), br(),
                            shinyjs::disabled(actionButton("names_Rename", "Rename",
                                              icon = icon("arrow-circle-right"), 
                                              width = 210,
                                              style = "color: #000000; background-color: #71a879; border-color: #091520;
                                                       padding:10px; font-size:120%")), 
                            offset = 1),
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
                                                           style = "color: #000000; background-color: #c4cc6d; border-color: #091520"))), 
                            offset = 2
                        ),
                        column(2,
                            shinyjs::disabled(downloadButton("downloadData", "Download Darwinized data",
                                                             with = 210,
                                                             style = "color: #000000; background-color: #71a879; border-color: #091520")),
                            offset = 0
                        ),
                        style = "margin-bottom:30px; border-bottom:2px solid; padding: 20px"
                    ),
                    br(), br(),
                    column(2, uiOutput("names_User")),
                    column(2, uiOutput("names_Standard"), 
                              uiOutput("names_Standard_Hover"), offset = 1),
                    box(title = "Darwinized Names", width = 2, 
                        status = "success", collapsible = TRUE, 
                        solidHeader = TRUE,
                        uiOutput("names_Renamed_Darwinized")),
                    box(title = "Manually Renamed", width = 2, 
                        status = "success", collapsible = TRUE, 
                        solidHeader = TRUE,
                        uiOutput("names_Renamed_Manual")),
                    box(title = "Identical matches", width = 2, 
                        status = "success", collapsible = TRUE, 
                        solidHeader = TRUE,
                        uiOutput("names_Renamed_Identical"))
                )
            )
        )
    ),
    title = "bdDwC"
)