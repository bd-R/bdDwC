#' Create reactive values used within app
#' 
#' @return An object that stores reactive values
#' (created with {shiny::reactiveValues})
#'
#' @importFrom shiny reactiveValues
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_server_reactivevalues <- function()  {
  rv <- shiny::reactiveValues(
    # User data used in Darwinizer
    data_user = data.frame(),
    # Darwinized data (created with darwinize_names)
    data_darwinized = data.frame(),
    # Data that contains all renamings
    data_rename = data.frame(),
    # Darwin Cloud Data (standard and fieldname)
    data_darwin_cloud = data_darwin_cloud$data,
    # Original set of names in user data
    names_user = c(),
    # Set of names in user data after renaming
    names_user_after = c(),
    # Original set of Darwin Cloud names
    names_standard = c(),
    # Set of Darwin Cloud names after renaming
    names_standard_after = c(),
    # Dictionary version (date)
    info_dc_date = data_darwin_cloud$date,
    # User original dictionary
    dic_user_raw = data.frame(),
    # Names in user original dictionary used to create radio buttons
    names_user_raw = c(),
    # Subset of users dictionary
    # Subset made using column names specified by user
    dic_user = data.frame()
  )
  return(rv)
}

#' Show shiny modal
#'
#' Show shiny modal with specified title, subtitle and other details
#'
#' @param title an object string passed as `shiny::modalDialog` title 
#' @param body an object shown withing `shiny::modalDialog`
#' @param size a value that specifies size of the dialog box
#' @param easyClose logical value to specify in box is easy to close
#' 
#' @import shiny
#' @importFrom utils packageVersion
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_server_modal <- function(
  title = "foo",
  body = "bar",
  size = "m",
  easyClose = TRUE
) {
  shiny::showModal(shiny::modalDialog(
    body,
    title = title,
    size = size,
    easyClose = easyClose
  ))
}

#' Upload local users data
#'
#' @param path_input path to a local file from `shiny::fileInput`
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
  shiny::showNotification("Started uploading data", type = "message")

  if (grepl("zip", path_input$type, ignore.case = TRUE)) {
    result <- finch::dwca_read(path_input$datapath, TRUE)$data[[1]]
  } else {
    result <- data.table::fread(path_input$datapath, data.table = FALSE)
  }

  foo <- nrow(result)
  if (foo > 0) {
    shiny::showNotification(
      paste0("Data successfully uploaded (", foo, " records)"),
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
#' @param ns namespace created with {shiny::NS}
#' 
#' @importFrom shiny HTML renderUI
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_ui_dictionary <- function(path_dictionary = NULL, date_dictionary, ns) {
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

      <button class='btn btn-default action-button' id='", ns("pop_dc"), "'
              style='width: 1px; border-color: #ffffff;
                     background-color: #ffffff;
                     font-size:100%' type='button'>
          <i class='glyphicon glyphicon-question-sign'></i>
      </button>

      <br/>
      <i class='glyphicon glyphicon-", user_dic_icon, " fa-1x'></i>
      Personal Dictionary ", user_dic_file,
      "<button class='btn btn-default action-button' id='", ns("pop_dic"), "'
              style='width: 1px; border-color: #ffffff;
                     background-color: #ffffff;
                     font-size:100%' type='button'>
          <i class='glyphicon glyphicon-question-sign'></i>
      </button>"
    )
    return(shiny::HTML(result))
  })
}

#' Update activity (enable/disable) of darwinizer tab
#' 
#' @param data_user a data.frame of submitted records to be darwnized
#'
#' @importFrom shinyjs addCssClass removeCssClass
#'
#' @family shiny
#'
#' @keywords shiny internal
#'
shiny_server_tab_darwinizer <- function(data_user) {
  if (nrow(data_user) == 0) {
    shinyjs::addCssClass(
      selector = "a[data-value='darwinizer']",
      class = "inactiveLink"
    )
  } else {
    shinyjs::removeCssClass(
      selector = "a[data-value='darwinizer']",
      class = "inactiveLink"
    )
  }
}