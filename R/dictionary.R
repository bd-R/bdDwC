#' Download Darwin Cloud Data
#'
#' `download_cloud_data()` is a function used to download Darwin Cloud
#' data. First it downloads data, then it subsets informative columns and
#' renames them so they could be used in `shiny` app.
#'
#' @param path_remote a character string that specifies path to remote
#' repository (Kurator in Github)
#' @param path_github a character string that specifies path
#' within given repository
#' @param path_file a character string that specifies name of a dictionary file
#' @param column_field a character string that specifies name of the column
#' that contains field information
#' @param column_stand a character string that specifies name of the column
#' that contains standard information
#'
#' @return data.frame of Darwin Cloud data.
#'
#' @examples
#' download_cloud_data()
#' 
#' @importFrom data.table fread
#'
#' @family dictionary functions
#'
#' @export
#'
download_cloud_data <- function(
  path_remote = "https://raw.githubusercontent.com/kurator-org/",
  path_github = "kurator-validation/master/packages/kurator_dwca/",
  path_file = "data/vocabularies/darwin_cloud.txt",
  column_field = "fieldname",
  column_stand = "standard") {

  # Test if path and columns are good
  lapply(
    list(path_remote, path_github, path_file, column_field, column_stand),
    test_cloud
  )
  # Test if columns are good
  test_columns_cloud(c(column_field, column_stand))

  # Create path to remote file
  path_cloud <- paste0(path_remote, path_github, path_file)
  # Read in remote file
  # Catch error if there's no internet connection
  data <- tryCatch({
    data.table::fread(
      path_cloud,
      sep = "\t", showProgress = FALSE, data.table = FALSE
    )
  }, error = function(cond) {
    return(NULL)
  })
  if (is.null(data)) {
    warning(
      "Cloud data wasn't downloaded, ",
      "probably due to the wrong path or internet connection."
    )
    return(NULL)
  }

  # Test if downloaded data is valid
  test_data_dwc(data)
  # Prepare dictionary data
  # Subset and remove missing fields
  result <- clean_dwc(data, column_field, column_stand)
  return(result)
}

#' Clean Dictionary Data
#'
#' Clean dictionary from unnecessary or empty fields
#'
#' @param data a data.frame with dictionary data
#' @param column_field a character string that specifies name of the column
#' that contains field information
#' @param column_stand a character string that specifies name of the column
#' that contains standard information
#'
#' @return a data.frame of cleaned dictionary data
#'
#' @family dictionary functions
#'
#' @keywords internal
#'
clean_dwc <- function(
  data = NULL,
  column_field = "fieldname",
  column_stand = "standard") {

  # Subset dictionary data only for needed columns
  data <- data[, colnames(data) %in% c(column_field, column_stand)]
  if (ncol(data) != 2) {
    stop(
      "Something is wrong with provided dictionary, ",
      "please check column names"
    )
  }
  # Filter out missing fields in the reference dictionary
  row_idx <- rowSums(apply(data, 2, function(x) x != "" & !is.na(x))) == 2
  data <- data[row_idx, ]
  if (nrow(data) == 0) {
    stop("Dictionary data contained only missing fields")
  }
  return(data)
}

#' Retrieve Information about Darwin Core Terms
#'
#' `get_darwin_core_info()` is a function (not exported) to download
#' Darwin Core Terms. This information is displayed when using manually
#' renaming.
#'
#' @param path_darwin_cloud a character string that specifies path to Darwin
#' @param regex_term a character string with regular expression to find
#' positions of Darwin Core Terms
#' @param regex_name a character string with regular expression to extract term
#' @param regex_defition a character string with regular expression to extract
#' terms definition
#' @param name_to_def a single numeric value specifying in how many rows from
#' the term name it's definition is stored
#'
#' @return a data.frame that contains name and definition for each available
#' Darwin Cloud term
#'
#' @family dictionary functions
#'
#' @keywords internal
#'
get_darwin_core_info <- function(
  path_darwin_cloud = "http://tdwg.github.io/dwc/terms/",
  regex_term = "table-secondary",
  regex_name = ".*>(.*)\\s<span.*",
  regex_definition = ".*</td><td>(.*)</td></tr>.*",
  name_to_def = 2) {

  # Test if path is good
  test_cloud(path_darwin_cloud)

  # Catch error if there's no internet connection
  # Return NA as it will be used if no result provided
  data <- tryCatch({
    readLines(path_darwin_cloud, warn = FALSE)
  }, error = function(cond) {
    warning(
      "Darwin core data wasn't downloaded, ",
      "probably due to the wrong path or internet connection."
    )
    return(NA)
  })

  if (all(is.na(data))) {
    result <- data.frame(
      name = NA,
      definition = NA,
      stringsAsFactors = FALSE
    )
    return(result)
  } else {
    idx <- grep(regex_term, data)
  }

  if (length(idx) == 0) {
    result_name <- NA
    result_definition <- NA
  } else {
    result_name <- trimws(sub(regex_name, "\\1", data[idx]))
    result_definition <- sub(regex_definition, "\\1", data[idx + name_to_def])
    result_definition <- trimws(gsub("'|\"", "", result_definition))
  }
  result <- data.frame(
    name = result_name,
    definition = result_definition,
    stringsAsFactors = FALSE
  )
  return(unique(result))
}