upload_local_file <- c(
  "text/csv",
  "text/comma-separated-values,text/plain",
  ".csv",
  ".zip",
  "application/zip"
)

online_databases <- list(
  "GBIF (Global Biodiversity Information Facility)" = "gbif",
  "iDigBio (Integrated Digitized Biocollections)" = "idigbio",
  "EcoEngine (Berkeley Ecoinformatics Engine)" = "ecoengine",
  "Vertnet (Vertebrate Network)" = "vertnet",
  "BISON (Biodiversity Information Serving Our Nation)" = "bison",
  "iNaturalist" = "inat",
  "ALA (Atlas of Living Australia)" = "ala",
  "OBIS (Ocean Biogeographic Information System)" = "obis",
  "AntWeb" = "antweb"
)

shinydashboard::dashboardPage(

  # --------------------------
  # HEADER
  # --------------------------

  shinydashboard::dashboardHeader(
    title = span(img(src = "bdDwC.png", width = 20), "bdDwC")
  ),

  # --------------------------
  # SIDEBAR
  # --------------------------

  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "my_tabs",
      shiny::tags$head(shiny::tags$style(
        ".inactiveLink {pointer-events: none;cursor: default;}"
      )),
      shinydashboard::menuItem("Upload Data", tabName = "upload"),
      shinydashboard::menuItem("Darwinizer", tabName = "darwinizer"),
      # Horizontal line
      shiny::tags$hr(style = "border-color: #bfbfbf;"),
      # Citation
      shiny::actionButton(
        "citation", "Cite us", style = "border-color: #091520;
                                        background-color: #e5e5e5"
      )
    )
  ),

  # --------------------------
  # BODY
  # --------------------------

  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shiny::includeCSS("www/style.css"),

    shinydashboard::tabItems(

    # --------------------------
    # UPLOAD DATA
    # --------------------------

    shinydashboard::tabItem("upload",
      shiny::fluidRow(
        # Upload user data
        shinydashboard::box(
          title = "Upload Data",
          status = "warning",
          width = 5,
          # Using shinyBS collapse as we want ONLY
          # one selection
          shinyBS::bsCollapse(
            multiple = FALSE,
            open = "From a Local File",
            # USER FILE
            shinyBS::bsCollapsePanel("Upload Local File",
              shiny::fileInput("path_input_data",
                shiny::h3("CSV / DWCA ZIP file input"),
                FALSE,
                accept = upload_local_file
              ),
              style = "info"
            ),
            # QUERY FROM A DATABASE
            shinyBS::bsCollapsePanel(
              "Query Data From a Database",
              shiny::textInput(
                "scientific_name",
                shiny::h3("Scientific Name:"),
                "Puma concolor"),
              shiny::numericInput(
                "record_size",
                shiny::h3("Record Size:"),
                500),
              shiny::selectInput(
                "has_coords",
                shiny::h3("Records Filter:"),
                list("With Coordinates" = "1",
                     "Without Coordinates" = "2",
                     "No Filter" = "3"
                ),
                3
              ),
              shiny::radioButtons(
                "query_db",
                shiny::h3("Online Database:"),
                online_databases,
                "gbif"
              ),
              shiny::br(),
              shiny::div(id = "query_database_div",
                class = "activeButton",
                shiny::actionButton(
                  "query_database", "Query Database", shiny::icon("download")
                )
              ),
              style = "success"
            )
          )
        ),

        # Upload dictionaries
        shinydashboard::box(
          title = "Dictionaries",
          status = "warning",
          width = 5,
          # Dictionary information render in server
          # Because of HTML and reactive object mix
          shiny::uiOutput("dic_info"),
          shiny::br(),
          # Darwin Cloud dictionary
          shiny::tags$b("Update Darwin Cloud dictionary"),
          shiny::br(),
          shiny::actionButton("update_darwin_cloud", "Update DC"),
          shiny::br(),
          shiny::br(),
          # Upload user dictionary
          shiny::fileInput(
            "path_input_dictionary",
            "Choose a personal dictionary file",
            accept = c(
              "text/csv", ".csv", "text/comma-separated-values,text/plain"
            )
          ),
          # Text that tells to select columns if dictionary added
          shiny::uiOutput("user_dic_text"),
          shiny::splitLayout(
            shiny::uiOutput("names_user_field"),
            shiny::uiOutput("names_user_ftandard"),
            cellWidths = 200,
            cellArgs = list(style = "padding: 6px")
          )
        )
      ),
      shiny::actionButton(
        "submit_to_darwinizer",
        "Submit to Darwinizer",
        width = 250,
        style = "background: url('Darwin.svg');
                 background-position: left center;
                 background-repeat: no-repeat;
                 background-color: #ffffff;
                 color: #000000;
                 border-color: #091520;
                 padding:10px;
                 font-size:120%"
      )
    ),

    # --------------------------
    # DARWINIZER
    # --------------------------

    shinydashboard::tabItem("darwinizer",
      shiny::fluidRow(
        shiny::fluidRow(
          shiny::column(12,
            shinydashboard::valueBoxOutput("vb_all_names", width = 2),
            shinydashboard::valueBoxOutput("vb_dwc_names", width = 2),
            shinydashboard::valueBoxOutput("vb_dwc_match", width = 2),
            shinydashboard::valueBoxOutput("vb_manual", width = 2),
            shinydashboard::valueBoxOutput("vb_dwc_ident", width = 2),
            offset = 1
          ),
          # Adds lines belowe value boxes
          shiny::column(12,
            style = "margin-bottom:10px; border-bottom:2px solid"
          )
        ),
        shiny::fluidRow(
          shiny::column(2,
            shiny::br(), shiny::br(),
            shinyjs::disabled(
              shiny::actionButton(
                "names_rename", "Rename",
                icon = shiny::icon("arrow-circle-right"), width = 210,
                style = "color: #000000;
                         background-color: #71a879;
                         border-color: #091520;
                         padding:10px;
                         font-size:120%"
              )
            ),
            offset = 1
          ),
          shiny::column(2,
            shiny::verticalLayout(
              shinyjs::disabled(
                shiny::actionButton(
                    "names_remove", "Remove selected rename",
                    icon = shiny::icon("times"), width = 210,
                    style = "color: #000000;
                             background-color: #a188bd;
                             border-color: #091520"
                )
              ),
              shiny::br(),
              shinyjs::disabled(
                shiny::actionButton(
                  "names_clean", "Remove all renames",
                  icon = shiny::icon("times"), width = 210,
                  style = "color: #000000;
                           background-color: #a188bd;
                           border-color: #091520"
                )
              ),
              shiny::br(),
              shinyjs::disabled(
                shiny::actionButton(
                  "names_rollback",
                  "Rollback to Darwinizer",
                  icon = shiny::icon("fast-backward"), width = 210,
                  style = "color: #000000;
                           background-color: #c4cc6d;
                           border-color: #091520"
                )
              )
            ),
            offset = 2
          ),
          shiny::column(2,
            shinyjs::disabled(
              shiny::downloadButton(
                "download_data", "Download final data",
                icon = shiny::icon("check"), width = 210,
                style = "color: #000000;
                         background-color: #71a879;
                         border-color: #091520;
                         padding:10px;
                         font-size:120%"
              )
            ),
            offset = 0
          ),
          style = "margin-bottom:30px;
                   border-bottom:2px solid;
                   padding: 20px"
        ),
        shiny::br(), shiny::br(),
        shiny::column(2, shiny::uiOutput("names_user")),
        shiny::column(2,
          shiny::uiOutput("names_standard"),
          shiny::uiOutput("names_standard_hover"), offset = 1),
        shinydashboard::box(title = "Darwinized Names",
          width = 2, status = "success", collapsible = TRUE, solidHeader = TRUE,
          shiny::uiOutput("names_renamed_darwinized")
        ),
        shinydashboard::box(title = "Manually Renamed",
          width = 2, status = "success", collapsible = TRUE, solidHeader = TRUE,
          shiny::uiOutput("names_renamed_manual")
        ),
        shinydashboard::box(title = "Identical Matches",
          width = 2, status = "success", collapsible = TRUE, solidHeader = TRUE,
          shiny::uiOutput("names_renamed_identical"))
        )
      )
    )
  ),
  title = "bdDwC"
)