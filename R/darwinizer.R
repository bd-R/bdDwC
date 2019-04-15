#' Darwinize names
#'
#' `darwinize_names()` is a function to darwinize given names using a reference
#' (ie, Darwin Cloud) dataset. It returns $Old$ and $New$ ($fieldname$ and
#' $standard$) name (ie, name from a given user dataset that had a match in
#' reference dataset).
#'
#' @param data_user data.frame with user data
#' @param data_dwc data.frame with Darwin Cloud data that contains at least two
#' columns (fieldname and standard)
#'
#' @return data.frame of darwinized user names.
#'
#' @examples
#' darwinize_names(
#'   data_user = bdDwC:::data_reptiles,
#'   data_dwc = bdDwC:::data_darwin_cloud$data
#' )
#' @export
#'
darwinize_names <- function(data_user = NULL, data_dwc = NULL) {

  # Test for a valid user data
  if (is.null(data_user)) {
    stop("Specify data_user")
  } else if (class(data_user) != "data.frame") {
    stop("data_user should be a data.frame")
  } else if (nrow(data_user) < 1 | ncol(data_user) < 1) {
    stop("data_user should contain at least one column and one row")
  } else if (length(colnames(data_user)) == 0) {
    stop("data_user dataset contains no names (nothing to Darwinize)")
  } else if (any(duplicated(data_user))) {
    warning("Some of the names in data_user are duplicated")
  }

  # Test for a valid dictionary data
  if (is.null(data_dwc)) {
    stop("Specify data_dwc")
  } else if (class(data_dwc) != "data.frame") {
    stop("data_dwc should be a data.frame")
  } else if (nrow(data_dwc) < 1 | ncol(data_dwc) < 2) {
    stop("data_dwc should contain at least two columns and one row")
  } else if (!all(c("fieldname", "standard") %in% colnames(data_dwc))) {
    stop("Darwin Cloud data should contain fieldname and standard columns")
  } else if (sum(colnames(data_dwc) %in% "fieldname") > 1) {
    stop("There is more than one column named fieldname in data_dwc")
  } else if (sum(colnames(data_dwc) %in% "standard") > 1) {
    stop("There is more than one column named standard in data_dwc")
  }

  # Subset data_dwc
  data_dwc <- data_dwc[, colnames(data_dwc) %in% c("fieldname", "standard")]
  if (ncol(data_dwc) != 2) {
    warning(
      "Something is wrong with Darwin Cloud data, ", 
      "please check column names"
    )
  }
  # Filter out missing fields in the reference dictionary
  row_idx <- rowSums(apply(data_dwc, 2, function(x) x != "" & !is.na(x))) == 2
  data_dwc <- data_dwc[row_idx, ]
  if (nrow(data_dwc) == 0) {
    stop("Darwin Cloud data contained only missing fields")
  }

  # Prepare user data
  # Extract names to be Darwinized
  data_user_name <- data.frame(
    fieldname = trimws(unique(colnames(data_user))),
    stringsAsFactors = FALSE
  )

  # Create object to store final result
  result <- list()

  # First Darwinization: Find identical matches
  # Given that user fieldname matches Darwin standard name
  result[["identical_raw"]] <- merge(
    data_user_name, data_dwc,
    by.x = "fieldname", by.y = "standard"
  )
  if (nrow(result[["identical_raw"]]) > 0) {
    result[["identical_clean"]] <- data.frame(
      # This is identical match so assigning twice
      name_old = unique(result[["identical_raw"]]$fieldname),
      name_new = unique(result[["identical_raw"]]$fieldname),
      match_type = "Identical",
      stringsAsFactors = FALSE
    )
  } else {
    warning("No names had identical matches")
    result[["identical_clean"]] <- data.frame()
  }
  if (nrow(result[["identical_clean"]]) == nrow(data_user_name)) {
    # Return result if all fields were matched
    message("All names had identical matches")
    return(result[["identical_clean"]])
  }

  # Subset data for further filtering
  # These user columns weren't matched
  data_user_name_sub <- data_user_name[
    !data_user_name$fieldname %in% result[["identical_clean"]]$name_new,
  ]
  # Transform names into lowercase
  data_user_name_sub <- data.frame(
    fieldname_low = tolower(data_user_name_sub),
    fieldname_orig = data_user_name_sub,
    stringsAsFactors = FALSE
  )

  # These reference fields weren't used
  data_dwc_sub <- data_dwc[
    !data_dwc$standard %in% result[["identical_clean"]]$name_new,
  ]
  data_dwc_sub$fieldname_low <- tolower(data_dwc$fieldname)

  # Match using lowecase
  result[["lower_raw"]] <- merge(
    data_user_name_sub,
    data_dwc_sub,
    "fieldname_low"
  )
  if (nrow(result[["lower_raw"]]) > 0) {
    result[["lower_clean"]] <- data.frame(
      name_old = result[["lower_raw"]]$fieldname_orig,
      name_new = result[["lower_raw"]]$standard,
      match_type = "Darwinized",
      stringsAsFactors = FALSE
    )
  } else {
    warning("No names had lower case matches")
    result[["lower_clean"]] <- data.frame()
  }
  result[["final"]] <- rbind(
    result[["lower_clean"]],
    result[["identical_clean"]]
  )
  if (nrow(result[["final"]]) == 0) {
    warning("No names were Darwinized")
  }
  return(result[["final"]])
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
#' result <- darwinize_names(
#'   data_user = bdDwC:::data_reptiles,
#'   data_dwc = bdDwC:::data_darwin_cloud$data
#' )
#' rename_user_data(bdDwC:::data_reptiles, result)
#' @export
#'
rename_user_data <- function(data_user = NULL, data_renamed = NULL) {
  if (is.null(data_user) | is.null(data_renamed)) {
    warning("Please provide two data.frames")
    return(NULL)
  }
  if (ncol(data_user) < 1) {
    warning("data_user data.frame should contain at least one column")
    return(NULL)
  }
  if (!"name_old" %in% colnames(data_renamed)) {
    warning("name_old column not present in data_renamed data.frame")
    return(NULL)
  }
  # Extract user names (we don't need all data, just to rename)
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
#' @importFrom utils read.csv
#'
#' @export
#'
download_cloud_data <- function(
  path_remote = "https://raw.githubusercontent.com/kurator-org/",
  path_github = "kurator-validation/master/packages/kurator_dwca/",
  path_file = "data/vocabularies/darwin_cloud.txt",
  column_field = "fieldname",
  column_stand = "standard") {

  # Create path to remote file
  path_cloud <- paste0(path_remote, path_github, path_file)

  # Read in remote file
  # Catching error as if there's no internet connection app wouldn't run
  data <- tryCatch({
    read.csv(path_cloud, sep = "\t", stringsAsFactors = FALSE)
  }, error = function(cond) {
    return(NULL)
  })
  if (is.null(data)) {
    return(NULL)
  }

  # Check if wanted columns exist
  foo <- column_field %in% colnames(data)
  bar <- column_stand %in% colnames(data)
  if (!foo) {
    warning(column_field, " column is not present in Darwin Cloud data")
  }
  if (!bar) {
    warning(column_field, " column is not present in Darwin Cloud data")
  }
  if (all(foo, bar)) {
    # Subset only used columns
    result <- subset(data, select = c(column_field, column_stand))
    # Rename to match names in shiny server
    colnames(result) <- c(column_field, column_stand)
  }
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
combine_old_new <- function(data, symbol_arrow = "->", symbol_space = " ") {
  result <- apply(data, 1, function(x) {
    paste0(x[1], symbol_space, symbol_arrow, "\n", x[2])
  })
  result <- as.character(result)
  return(result)
}

#' Retrieve Information about Darwin Core Terms
#'
#' `get_darwin_core_info()` is a function (not exported) to download
#' Darwin Core Terms. This information is displayed when using manually
#' renaming.
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
    result_name <- NA
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

#' Launch bdDwC Shiny Application
#'
#' `run_dwc` is a function that starts bdverse Darwin Cloud cleaning `shiny` app.
#'
#' @return `shiny::runApp()` result within browser.
#'
#' @import shinydashboard
#' @import shiny
#' @import shinyBS
#' @importFrom data.table fread fwrite
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
