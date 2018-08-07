#' Darwinize names
#' 
#' `darwinizeNames()` is a function to darwinize given names using reference 
#' (ie, Darwin Cloud) dataset. It returns $Old$ and $New$ ($fieldname$ and $standard$)
#' name (ie, name from given user dataset that had a match in reference dataset).
#' 
#' @param dataUser data.frame with user data
#' @param dataReference data.frame with Darwin Cloud data

#' @return data.frame of darwinized user names.
#' 
#' @examples
#' darwinazeNames(dataUser, bdDwC:::dataDarwinCloud[[1]])
#' 
darwinazeNames <- function(dataUser, dataReference) {
    dataReference <- data.frame(apply(dataReference, 2, tolower))
    namesUser     <- data.frame(fieldname = tolower(names(dataUser)))
    resultCombine <- merge(namesUser, dataReference, "fieldname")
    return(resultCombine)
}