#' Darwinize names
#' 
#' `darwinize_names()` is a function to darwinize given names using reference 
#' (ie, Darwin Cloud) dataset. It returns $Old$ and $New$ ($fieldname$ and $standard$)
#' name (ie, name from given user dataset that had a match in reference dataset).
#' 
#' @param data_user data.frame with user data
#' @param data_dwc data.frame with Darwin Cloud data
#' 
#' @return data.frame of darwinized user names.
#' 
#' @examples
#' darwinize_names(data_user = bdDwC:::data_reptiles, 
#'                 data_dwc  = bdDwC:::data_darwin_cloud$data)
#' 
#' @export
#' 
darwinize_names <- function(data_user, data_dwc) {

  # Prepare reference data
  data_dwc$fieldname_orig <- data_dwc$fieldname
  data_dwc$fieldname_low  <- tolower(data_dwc$fieldname)

  data_user <- data.frame(
    fieldname_low = tolower(names(data_user)),
    fieldname_orig  = names(data_user),
    stringsAsFactors = FALSE
  )

  # Find identical matches
  # Given user fieldname matches Darwin standard name
  match_identical <- merge(
    data_user, data_dwc,
    by.x = "fieldname_orig", by.y = "standard"
  )
  if (nrow(match_identical) > 0) {
    match_identical <- data.frame(
      fieldname = unique(match_identical$fieldname_orig),
      standard = unique(match_identical$fieldname_orig),
      match_type = "Identical",
      stringsAsFactors = FALSE
    )
  }

  # Subset data for further filtering
  data_user_sub <- data_user[
    !data_user$fieldname_orig %in% match_identical$fieldname,
  ]
  data_dwc_sub  <- data_dwc[
    !data_dwc$standard %in% match_identical$standard,
  ]

  # Match user lower cases
  match_lower <- merge(data_user_sub, data_dwc_sub, "fieldname_low")
  if (nrow(match_lower) > 0) {
    match_lower <- data.frame(
      fieldname = match_lower$fieldname_orig.x,
      standard = match_lower$standard,
      match_type = "Darwinized",
      stringsAsFactors = FALSE
    )
  }

  result <- data.frame(
    name_old = c(match_identical$fieldname, match_lower$fieldname),
    name_new = c(match_identical$standard, match_lower$standard),
    match_type = c(match_identical$match_type, match_lower$match_type),
    stringsAsFactors = FALSE)
  return(result)
}

#' Combine old/new name for checkboxes
#' 
#' `combine_old_new()` is a function that combines (`paste`) 
#' fieldname and standard
#' names with unicode characters to present in checkboxes.
#' 
#' @param data data.frame of matched names (`darwinize_names()` output)
#' @param symbol_arrow character string for symbol that is used in `paste0` 
#' to connect old and new name
#' @param symbol_space character string for symbol used instead of white space 
#' 
#' @return data.frame of darwinized user names.
#' 
combine_old_new <- function(data, symbol_arrow = "->", symbol_space  = " ") {
  result <- apply(data, 1, function(x) {
    paste0(x[1], symbol_space, symbol_arrow, "\n", x[2])
  })
  result <- as.character(result)
  return(result)
}

#' Launch bdDwC Shiny Application
#'
#' `run_dwc` is a function that starts bdverse Darwin Cloud cleaning `shiny` app.
#' 
#' @return `shiny::runApp()` result within browser.
#' 
#' @import shinydashboard
#' @import shiny
#' @import shinyBS
#' @importFrom data.table fread
#' @importFrom finch dwca_read
#' @importFrom rgbif occ_search
#' @importFrom shinyjs addCssClass disable disabled enable useShinyjs removeCssClass
#' @importFrom spocc occ
#' 
#' @export
#' 
run_dwc <- function() {
  path_app <- system.file("shiny", package = "bdDwC")
  return(shiny::runApp(path_app, launch.browser = TRUE))
}

#' Rename Dataset According Darwinized Names
#' 
#' `rename_user_data()` is a function used to rename given dataset
#' (usually given user data) according previously Darwinized Names.
#' 
#' @param data_user data.frame of be renamed.
#' @param data_renamed data.frame that was created using 
#' `bdDwC::darwinize_names()` function using same `data_user`.
#' 
#' @return data.frame of submitted user data, but renamed according given 
#' renames data.frame.
#' 
#' @examples
#' result <- darwinize_names(data_user = bdDwC:::data_reptiles, 
#'                           data_dwc  = bdDwC:::data_darwin_cloud$data)
#' rename_user_data(bdDwC:::data_reptiles, result)
#' 
#' @export
#'
rename_user_data <- function(data_user, data_renamed) {
  # Extract user names (we don't need all data just to rename)
  names_user <- data.frame(
    name_old = colnames(data_user), stringsAsFactors = FALSE
  )
  # merge gives us names that were in renamed dataset
  result <- merge(names_user, data_renamed, "name_old", all.x = TRUE)
  # However, merge reorders data - we need to sort it back
  result <- result[match(names_user$name_old, result$name_old), ]
  colnames(data_user) <- ifelse(
    is.na(result$name_new), result$name_old, result$name_new
  )
  return(as.data.frame(data_user))
}

#' Retrieve Information about Darwin Core Terms
#' 
#' `get_darwin_core_info()` is a function (not exported) to download
#' Darwin Core Terms. This information is displayed when using manually renaming.
#' 
#' @param path_darwin_cloud Path to Darwin Cloud data.
#' 
#' @return data.frame that contains name and definition for each available
#' Darwin Cloud term.
#' 
get_darwin_core_info <- function(
  path_darwin_cloud = "http://tdwg.github.io/dwc/terms/") {
  # Catching error as if there's no internet connection app wouldn't run
  data <- tryCatch({
      readLines(path_darwin_cloud, warn = FALSE)
  }, error = function(cond) {
      return(NA)
  })

  if (all(is.na(data))) {
    result_definition <- NA
    result_name       <- NA
  } else {
    data <- grep("Term Name:", data, value = TRUE)

    # Not very elegant, but works in base R
    # Name
    result_name <- gsub(".*Term Name: dcterms:([A-z]+).*", "\\1", data)
    result_name <- gsub(".*Term Name: ([A-z]+).*", "\\1", result_name)
    # Definition
    result_definition <- gsub(
      ".*<TD>Definition:</TD><TD>(.*)</TD></TR>.*<TR><TD>Comment:.*",
      "\\1", data
    )
    result_definition <- gsub("'|\"", "", result_definition)
  }
  result <- data.frame(
    name = result_name,
    definition = result_definition,
    stringsAsFactors = FALSE
  )
  return(result)
}

#' Download Darwin Cloud Data
#' 
#' `download_cloud_data()` is a function used to download Darwin Cloud 
#' data. First it downloads data, then it subsets informative columns and 
#' renames them so they could be used in `shiny` app.
#' 
#' @param path_remote Path to remote repository (Kurator in Github).
#' @param path_github Path within given repository.
#' @param path_file Name of a file.
#' @param column_field Name of the column that contains field information.
#' @param column_stand Name of the column that contains standard information.
#' 
#' @return data.frame of Darwin Cloud data.
#' 
#' @examples
#' download_cloud_data()
#' 
#' @importFrom utils read.csv
#' 
#' @export
#' 
download_cloud_data <- function(
  path_remote  = "https://raw.githubusercontent.com/kurator-org",
  path_github  = "/kurator-validation/master/packages/kurator_dwca/data/vocabularies/",
  path_file    = "darwin_cloud.txt",
  column_field = "fieldname",
  column_stand = "standard") {
  path_cloud <- paste0(path_remote, path_github, path_file)
  data <- read.csv(path_cloud, sep = "\t")
  # Subset only used columns
  data <- subset(data, select = c(column_field, column_stand))
  # Rename to match names in shiny server
  colnames(data) <- c(column_field, column_stand)
  # Add values for missing standard
  return(data)
}