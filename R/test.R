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
test_data_user <- function(data) {
  if (is.null(data)) {
    stop("Specify data_user")
  } else if (class(data) != "data.frame") {
    stop("data_user should be a data.frame")
  } else if (nrow(data) < 1 | ncol(data) < 1) {
    stop("data_user should contain at least one column and one row")
  } else if (length(colnames(data)) == 0) {
    stop("data_user dataset contains no names (nothing to Darwinize)")
  } else if (any(duplicated(data))) {
    warning("Some of the names in data_user are duplicated")
  }
}

#' Test If Provided Dictionary Data is Valid
#'
#' @param data a data.frame with dictionary data
#' 
#' @return error or nothing
#' 
#' @family test functions
#' 
#' @keywords internal
#' 
test_data_dwc <- function(data) {
  if (is.null(data)) {
    stop("Specify data_dwc")
  } else if (class(data) != "data.frame") {
    stop("data_dwc should be a data.frame")
  } else if (nrow(data) < 1 | ncol(data) < 2) {
    stop("data_dwc should contain at least two columns and one row")
  } else if (!all(c("fieldname", "standard") %in% colnames(data))) {
    stop("Darwin Cloud data should contain fieldname and standard columns")
  } else if (sum(colnames(data) %in% "fieldname") > 1) {
    stop("There is more than one column named fieldname in data_dwc")
  } else if (sum(colnames(data) %in% "standard") > 1) {
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
test_data_renamed <- function(data) {
  if (is.null(data)) {
    stop("Specify data_renamed")
  } else if (class(data) != "data.frame") {
    stop("data_renamed should be a data.frame")
  } else if (ncol(data) < 2) {
    stop("data_renamed should contain at least three colums")
  } else if (nrow(data) == 0) {
    stop("data_renamed should contain at least one row")
  } else if (!all(c("name_old", "name_new") %in% colnames(data))) {
    stop("Renaming data should contain name_new and name_old columns")
  } else if (sum(colnames(data) %in% "name_new") > 1) {
    stop("There is more than one column named name_new in data_renamed")
  } else if (sum(colnames(data) %in% "name_old") > 1) {
    stop("There is more than one column named name_old in data_renamed")
  }
}