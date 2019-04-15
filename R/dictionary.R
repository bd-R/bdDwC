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
#' @family dictionary functions
#'
#' @keywords internal
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

#' Clean Dictionary Data
#'
#' Clean dictionary from unnecessary or empty fields
#'
#' @param data a data.frame with dictionary data
#'
#' @return a data.frame of cleaned dictionary data
#'
#' @family dictionary functions
#' 
#' @keywords internal
#' 
clean_dwc <- function(data) {
  # Subset dictionary data only for needed columns
  data <- data[, colnames(data) %in% c("fieldname", "standard")]
  if (ncol(data) != 2) {
    warning(
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