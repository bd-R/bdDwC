#' Show shiny welcoming modal
#'
#' Show shiny modal with specified title, subtitle and other details
#'
#' @param title a character string passed as `shiny::modalDialog` title 
#' @param subtitle a character string shown withing `shiny::modalDialog`
#' 
#' @import shiny
#' @importFrom utils packageVersion
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_server_modal_welcome <- function(
  title = "Welcome to Darwinizer!",
  subtitle = "Darwinize Your Data",
  package = "bdDwC"
) {
  shiny::showModal(shiny::modalDialog(
    title = shiny::h3(title),
    shiny::p(subtitle),
    shiny::img(src = "bdverse.png", align = "center", width = "570"),
    shiny::helpText(
      "GPL-3 License Tomer Gueta, Vijay Barve, Povilas Gibas,
       Thiloshon Nagarajah, Ashwin Agrawal and Carmel Yohay",
       "(", format(Sys.Date(), "%Y"), ").",
       shiny::br(),
       "Package version", as.character(utils::packageVersion("bdDwC"))
    ),
    shiny::helpText(
      "Contribute: ",
      shiny::a(paste0("https://github.com/bd-R/", package),
               href = paste0("https://github.com/bd-R/", package)),
      shiny::br(), "Join: ",
      shiny::a("https://bd-r-group.slack.com",
               href = "https://bd-r-group.slack.com")
    ),
    size = "m",
    easyClose = TRUE
  ))
}

#' Show shiny citation modal
#'
#' @import shiny
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_server_modal_citation <- function() {
  shiny::showModal(shiny::modalDialog(
    title = "Cite us",
    shiny::HTML(paste("bdverse will be published soon!")),
    easyClose = TRUE
  ))
}

#' Show darwin cloud data modal
#'
#' @import shiny
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_server_modal_cloud <- function() {
  shiny::showModal(shiny::modalDialog(
    title = shiny::h3("Darwin Cloud Data"),
    tags$p(
      "bdDwC uses Darwin Core Dictionary (stored on official",
      tags$a(
        href = "https://github.com/kurator-org/kurator-validation",
        "Kurator's repository)."
      ),
      shiny::br(),
      "Update Darwin Core version for your analysis by clicking",
      tags$b("Update DC"), "button bellow."
    ),
    size = "m",
    easyClose = TRUE
  ))
}

#' Show dictionary modal
#'
#' @import shiny
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_server_modal_dictionary <- function() {
  shiny::showModal(shiny::modalDialog(
    title = shiny::h3("Personal Dictionary File"),
    tags$p("File with columns fieldname and standard name"),
    size = "m",
    easyClose = TRUE
  ))
}

#' Upload local users data
#'
#' @param path_input path to local file from `shiny::fileInput`
#' 
#' @importFrom data.table fread
#' @importFrom finch dwca_read
#' @importFrom shiny showNotification
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_server_upload_local <- function(path_input = NULL) {
  if (is.null(path_input)) {
    warning("No local file specified")
    return(NULL)
  }

  shiny::showNotification(
    "Started uploading data",
    closeButton = FALSE,
    type = "message"
  )

  if (grepl("zip", tolower(path_input$type))) {
    result <- finch::dwca_read(path_input$datapath, TRUE)$data[[1]]
  } else {
    result <- data.table::fread(path_input$datapath, data.table = FALSE)
  }

  if (nrow(result) > 0) {
    shiny::showNotification(
      "Data successfully uploaded",
      closeButton = FALSE,
      type = "message"
    )
  }

  return(result)
}

#' Query data from the remote database
#'
#' @param scientific_name species scientific name
#' @param record_size numeric value (positive integer) of how many records to
#' get
#' @param query_db database to use
#' @param has_coords should records have coordinates
#' 
#' @importFrom rgbif occ_search
#' @importFrom spocc occ
#' @importFrom shiny showNotification
#' @importFrom utils capture.output
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_server_upload_database <- function(
  scientific_name,
  record_size,
  query_db,
  has_coords
) {
  # Check if user entered valid value
  if (trimws(scientific_name) == "") {
    foo <- paste("Please enter a valid scientific name")
    shiny::showNotification(foo, type = "error")
  }
  if (record_size <= 0) {
    foo <- paste("Please enter a valid number of records")
    shiny::showNotification(foo, type = "error")
  }
  shiny::showNotification(
    "Started downloading data",
    closeButton = FALSE,
    type = "message"
  )
  if (query_db == "gbif") {
    result <- rgbif::occ_search(
      scientificName = scientific_name,
      limit = record_size,
      hasCoordinate = switch(
        has_coords,
        "1" = TRUE, "2" = FALSE, "3" = NULL
      )
    )$data
  } else {
    warnings <- utils::capture.output(
      data <- spocc::occ(
        query = scientific_name,
        from = query_db,
        limit = record_size,
        has_coords = switch(
          has_coords,
          "1" = TRUE, "2" = FALSE, "3" = NULL
        )
      ),
      type = "message"
    )
    if (length(warnings) > 0) {
      shiny::showNotification(
        paste(warnings, collapse = " "), duration = 5
      )
    }
    result <- data[[query_db]]$data[[1]]
  }
  if (is.null(result)) {
    result <- data.frame()
    foo <- paste(
      "There are no entries with",
      scientific_name,
      "scientific name. Please try another one"
    )
    shiny::showNotification(foo, type = "error")
  } else {
    shiny::showNotification(
      "Data successfully downloaded",
      closeButton = FALSE,
      type = "message"
    )
  }
  return(as.data.frame(result))
}

#' Extract darwin core definitions and return as ui element
#'
#' @param input a list with arguments for quering data from the databse
#' 
#' @importFrom shinyBS bsTooltip
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_ui_darwin_core_definition <- function() {
  apply(
    data_darwin_core_info, 1,
    function(x) {
      shinyBS::bsTooltip(paste0("DWC_", x[1]), x[2], "right")
    }
  )
}

#' Create shiny value boxes
#'
#' @param value value passed to `shinydashboard::valueBox` value argument
#' @param subtitle value passed to `shinydashboard::valueBox` subtitle argument
#' @param color value passed to `shinydashboard::valueBox` color argument
#' 
#' @importFrom shinydashboard renderValueBox valueBox
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_ui_valuebox <- function(value, subtitle, color) {
  shinydashboard::renderValueBox({
    shinydashboard::valueBox(value, subtitle, color = color)
  })
}

#' Create radio buttons for user dictionary
#'
#' @param names_dic a vector that contains column names of user dictionary
#' @param inputid value passed as `inputId` to `shiny::radioButtons`
#' @param label value passed as `label` to `shiny::radioButtons`
#' @param spanid value used to change span id in shiny::radioButtons
#' @param selected selected name in radio buttons
#' 
#' @importFrom shiny HTML radioButtons
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_ui_dictionary_radiobuttons <- function(
  names_dic = NULL,
  inputid,
  label,
  spanid,
  selected
) {
  # Main function to create radio buttons
  result <- shiny::radioButtons(inputid, label, names_dic, selected)
  # For each name change ID
  # We need individual IDs so we can disable them with shinyjs
  # We need to disable them as same ID can't be field and standard
  for (i in names_dic) {
    result <- sub(
      paste0("<span>", i, "</span>"),
      paste0("<span id=\"", spanid, "_", i, "\">", i, "</span>"),
      result
    )
  }
  return(shiny::HTML(result))
}

#' Create information about dictionaries
#'
#' @param path_dictionary a character string with path to users dictionary
#' @param date_dictionary a value with date of darwin cloud dictionary date
#' 
#' @importFrom shiny HTML renderUI
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_ui_dictionary <- function(path_dictionary = NULL, date_dictionary) {
  shiny::renderUI({
    # Select icon
    user_dic_icon <- ifelse(is.null(path_dictionary), "unchecked", "check")
    if (is.null(path_dictionary)) {
      user_dic_file <- NULL
    } else {
      user_dic_file <- paste0(
        "(", sub("\\.txt$|\\.csv$", "", basename(path_dictionary)), ")"
      )
    }
    result <- paste0(
      "<b>Used dictionaries:</b>
      <br/>
      <i class='glyphicon glyphicon-check fa-1x'></i>
      Darwin Cloud (version: ", format(date_dictionary, "%d-%B-%Y"), ")

      <button class='btn btn-default action-button' id='pop_dc'
              style='width: 1px; border-color: #ffffff;
                     background-color: #ffffff;
                     font-size:100%' type='button'>
          <i class='glyphicon glyphicon-question-sign'></i>
      </button>

      <br/>
      <i class='glyphicon glyphicon-", user_dic_icon, " fa-1x'></i>
      Personal Dictionary ", user_dic_file,
      "<button class='btn btn-default action-button' id='pop_dic'
              style='width: 1px; border-color: #ffffff;
                     background-color: #ffffff;
                     font-size:100%' type='button'>
          <i class='glyphicon glyphicon-question-sign'></i>
      </button>"
    )
    return(shiny::HTML(result))
  })
}