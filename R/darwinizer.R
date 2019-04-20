#' Darwinize names
#'
#' `darwinize_names()` is a function to darwinize given names using a reference
#' (ie, Darwin Cloud) dataset. It returns $Old$ and $New$ ($fieldname$ and
#' $standard$) name (ie, name from a given user dataset that had a match in
#' reference dataset).
#'
#' @param data_user a data.frame with user data
#' @param data_dwc a data.frame with Darwin Cloud data that contains at least two
#' columns (fieldname and standard)
#'
#' @return data.frame of darwinized user names
#'
#' @examples
#' darwinize_names(
#'   data_user = bdDwC:::data_reptiles,
#'   data_dwc = bdDwC:::data_darwin_cloud$data
#' )
#' @family darwinizer functions
#'
#' @export
#'
darwinize_names <- function(data_user = NULL, data_dwc = NULL) {

  # ~~~~~~~~~~~~~~~~~~~~~
  # Data preparation
  # ~~~~~~~~~~~~~~~~~~~~~

  # Test if user data is good
  test_data_user(data_user)
  # Test if dictionary data is good
  test_data_dwc(data_dwc)

  # Prepare user data
  # Extract names to be Darwinized
  data_user_name <- data.frame(
    fieldname = trimws(unique(colnames(data_user))),
    stringsAsFactors = FALSE
  )

  # Prepare dictionary data
  # Subset and remove missing fields
  data_dwc <- clean_dwc(data_dwc)

  # Create object to store final result
  result <- list()

  # ~~~~~~~~~~~~~~~~~~~~~
  # Identical matching
  # ~~~~~~~~~~~~~~~~~~~~~

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

  # ~~~~~~~~~~~~~~~~~~~~~
  # Lowercase matching
  # ~~~~~~~~~~~~~~~~~~~~~

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
  data_dwc_sub$fieldname_low <- tolower(data_dwc_sub$fieldname)

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

  # ~~~~~~~~~~~~~~~~~~~~~
  # Output
  # ~~~~~~~~~~~~~~~~~~~~~

  result[["final"]] <- rbind(
    result[["lower_clean"]],
    result[["identical_clean"]]
  )
  if (nrow(result[["final"]]) == 0) {
    warning("No names were Darwinized")
  }
  return(result[["final"]])
}

#' Rename Dataset According to Darwinized Names
#'
#' `rename_user_data()` is a function used to rename given dataset
#' (usually given user data) according previously Darwinized Names.
#'
#' @param data_user a data.frame to be renamed
#' @param data_renamed a data.frame that was created using
#' `bdDwC::darwinize_names()` function using same `data_user`
#'
#' @return data.frame of submitted user data, but renamed according given
#' renames data.frame
#'
#' @examples
#' result <- darwinize_names(
#'   data_user = bdDwC:::data_reptiles,
#'   data_dwc = bdDwC:::data_darwin_cloud$data
#' )
#' rename_user_data(bdDwC:::data_reptiles, result)
#' @family darwinizer functions
#'
#' @export
#'
rename_user_data <- function(data_user = NULL, data_renamed = NULL) {

  # ~~~~~~~~~~~~~~~~~~~~~
  # Data preparation
  # ~~~~~~~~~~~~~~~~~~~~~

  # Test if user data is good
  test_data_user(data_user)
  # Test if dictionary data is good
  test_data_renamed(data_renamed)

  # ~~~~~~~~~~~~~~~~~~~~~
  # Renaming
  # ~~~~~~~~~~~~~~~~~~~~~

  # Extract user names (we don't need all data)
  names_user <- data.frame(
    name_old = trimws(colnames(data_user)), stringsAsFactors = FALSE
  )

  # merge gives us names that were in renamed dataset
  result <- merge(names_user, data_renamed, "name_old", all.x = TRUE)

  if (all(is.na(result$name_new))) {
    warning("No renaming could be made")
    return(NULL)
  } else {
    # merge reorders data - we need to sort it back
    result <- result[match(names_user$name_old, result$name_old), ]
    colnames(data_user) <- ifelse(
      is.na(result$name_new), result$name_old, result$name_new
    )
    return(data_user)
  }
}

#' Link Old/New Name for Checkboxes
#'
#' `combine_old_new()` is a function that combines (`paste`) fieldname and
#' standard names with unicode characters to present in checkboxes.
#'
#' @param data a data.frame of matched names (`darwinize_names()` output)
#' @param linker a character string for symbol that is used in `paste0`
#' to connect old and new name
#'
#' @return a data.frame of darwinized user names
#'
#' @family darwinizer functions
#'
#' @keywords internal
#'
link_old_new <- function(data = NULL, linker = "->") {
  # Test if dictionary data is good
  test_data_renamed(data)
  if (!is.character(linker)) {
    stop("Linker should be a character string")
  }
  result <- apply(data, 1, function(x) {
    paste(x[1], linker, "\n", x[2])
  })
  result <- as.character(result)
  return(result)
}
