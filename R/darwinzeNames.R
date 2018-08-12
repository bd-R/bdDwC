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
    dataReference$fieldname <- tolower(dataReference$fieldname)
    namesUser     <- data.frame(fieldname      = tolower(names(dataUser)),
                                fieldnameOrig  = names(dataUser))
    result <- merge(namesUser, dataReference, "fieldname")
    result <- subset(result, select = c("fieldnameOrig", "standard"))
    colnames(result) <- c("fieldname", "standard")
    return(result)
}