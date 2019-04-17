#' Test If Provided User Data is Valid
#'
#' @param data a data.frame with user data
#' 
#' @return error or nothing
#' 
#' @family test functions
#' 
#' @keywords internal
#' 
test_data_user <- function(data = NULL) {
  if (is.null(data)) {
    stop("Specify data_user")
  } else if (class(data) != "data.frame") {
    stop("data_user should be a data.frame")
  } else if (nrow(data) < 1 | ncol(data) < 1) {
    stop("data_user should contain at least one column and one row")
  } else if (length(colnames(data)) == 0) {
    stop("data_user dataset contains no names (nothing to Darwinize)")
  } else if (any(duplicated(colnames(data)))) {
    warning("Some of the names in data_user are duplicated")
  }
}

#' Test If Provided Dictionary Data is Valid
#'
#' @param data a data.frame with dictionary data
#' @param column_field a character string that specifies name of the column
#' that contains field information
#' @param column_stand a character string that specifies name of the column
#' that contains standard information
#' 
#' @return error or nothing
#' 
#' @family test functions
#' 
#' @keywords internal
#' 
test_data_dwc <- function(
  data = NULL,
  column_field = "fieldname",
  column_stand = "standard") {
  if (is.null(data)) {
    stop("Specify data_dwc")
  } else if (class(data) != "data.frame") {
    stop("data_dwc should be a data.frame")
  } else if (nrow(data) < 1 | ncol(data) < 2) {
    stop("data_dwc should contain at least two columns and one row")
  } else if (!all(c(column_field, column_stand) %in% colnames(data))) {
    stop("Darwin Cloud data should contain fieldname and standard columns")
  } else if (sum(colnames(data) %in% column_field) > 1) {
    stop("There is more than one column named fieldname in data_dwc")
  } else if (sum(colnames(data) %in% column_stand) > 1) {
    stop("There is more than one column named standard in data_dwc")
  }
}

#' Test If Provided Renaming Data is Valid
#'
#' @param data a data.frame with renaming data
#' 
#' @return error or nothing
#' 
#' @family test functions
#' 
#' @keywords internal
#' 
test_data_renamed <- function(data = NULL) {
  if (is.null(data)) {
    stop("Specify data_renamed")
  } else if (class(data) != "data.frame") {
    stop("data_renamed should be a data.frame")
  } else if (nrow(data) == 0) {
    stop("data_renamed should contain at least one row")
  } else if (ncol(data) < 2) {
    stop("data_renamed should contain at least two colums")
  } else if (!all(c("name_old", "name_new") %in% colnames(data))) {
    stop("Renaming data should contain name_new and name_old columns")
  } else if (sum(colnames(data) %in% "name_new") > 1) {
    stop("There is more than one column named name_new in data_renamed")
  } else if (sum(colnames(data) %in% "name_old") > 1) {
    stop("There is more than one column named name_old in data_renamed")
  }
}

#' Test If Provided Path to Cloud Data is Valid
#'
#' @param string a character string with path
#' 
#' @return error or nothing
#' 
#' @family test functions
#' 
#' @keywords internal
#' 
test_cloud <- function(string = NULL) {
  if (is.null(string)) {
    stop("Specify correct cloud path/columns")
  } else if (!is.character(string)) {
    stop("Cloud path/columns should be a character string")
  } else if (length(string) != 1) {
    stop("Path/Columns should be single string, not a vector")
  }
}

#' Test If Provided Columns of Cloud Data are Valid
#'
#' @param string a vector that containts two character strings
#' 
#' @return error or nothing
#' 
#' @family test functions
#' 
#' @keywords internal
#' 
test_columns_cloud <- function(strings = NULL) {
  # Test if there are really two strings
  if (length(strings) != 2) {
    stop("There should be two cloud columns provided")
  } else if (unique(sapply(strings, class)) != "character") {
    stop("Provided strings should be character class")
  } else if (strings[1] == strings[2]) {
    stop("Cloud data column names should be different")
  }
}