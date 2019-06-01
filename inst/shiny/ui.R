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
      bdDwC:::module_server_modalsUI("main")
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
              open = "Upload Local File",
              # USER FILE
              shinyBS::bsCollapsePanel(
                "Upload Local File",
                bdDwC:::module_server_upload_localInput("main"),
                style = "info"
              ),
              # QUERY FROM A DATABASE
              shinyBS::bsCollapsePanel(
                "Query Data From a Database",
                bdDwC:::module_server_upload_databaseInput("main"),
                style = "success"
              )
            )
          ),

          # Dictionaries
          shinydashboard::box(
            title = "Dictionaries",
            status = "warning",
            width = 5,
            # Display dictionary information
            bdDwC:::module_ui_dictionaryUI("main"),
            # Buttons for field and standard names
            bdDwC:::module_ui_dictionary_radiobuttons_fieldOutput(
              "main"
            )
          )
        ),
        bdDwC:::module_server_darwinizerInput("main")
      ),

      # --------------------------
      # DARWINIZER
      # --------------------------

      shinydashboard::tabItem("darwinizer",
        shiny::fluidRow(
          shiny::fluidRow(
            shiny::column(12,
              bdDwC:::module_ui_valueboxOutput("main"),
              offset = 1
            ),
            # Adds lines belowe value boxes
            shiny::column(12,
              style = "margin-bottom:10px; border-bottom:2px solid"
            )
          ),
          bdDwC:::module_ui_buttonsUI("main"),
          shiny::br(), shiny::br(),
          bdDwC:::module_ui_checkboxUI("main")
        )
      )
    )
  ),
  title = "bdDwC"
)