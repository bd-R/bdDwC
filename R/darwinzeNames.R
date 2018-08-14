#' Darwinize names
#' 
#' `darwinizeNames()` is a function to darwinize given names using reference 
#' (ie, Darwin Cloud) dataset. It returns $Old$ and $New$ ($fieldname$ and $standard$)
#' name (ie, name from given user dataset that had a match in reference dataset).
#' 
#' @param dataUser data.frame with user data
#' @param dataDWC data.frame with Darwin Cloud data
#' 
#' @return data.frame of darwinized user names.
#' 
#' @examples \dontrun {
#'     darwinazeNames(dataUser, bdDwC:::dataDarwinCloud[[1]])
#' }
#' 
darwinazeNames <- function(dataUser, dataDWC) {

    # Prepare reference data
    dataDWC$fieldnameOrig <- dataDWC$fieldname
    dataDWC$fieldnameLow  <- tolower(dataDWC$fieldname)

    dataUser  <- data.frame(fieldnameLow = tolower(names(dataUser)),
                            fieldnameOrig  = names(dataUser),
                            stringsAsFactors = FALSE)

    # Find identical matches
    # Given user fieldname matches Darwin standard name
    matchIdentical <- merge(dataUser, dataDWC, by.x = "fieldnameOrig", by.y = "standard")
    if (nrow(matchIdentical) > 0) {
        matchIdentical <- data.frame(fieldname = unique(matchIdentical$fieldnameOrig), 
                                     standard = unique(matchIdentical$fieldnameOrig),
                                     matchType = "Identical",
                                     stringsAsFactors = FALSE)
    }

    # Subset data for further filtering
    dataUserSub <- subset(dataUser, !fieldnameOrig %in% matchIdentical$fieldname)
    dataDWCSub  <- subset(dataDWC, !standard %in% matchIdentical$standard)

    # Match user lower cases
    matchLower <- merge(dataUserSub, dataDWCSub, "fieldnameLow")
    if (nrow(matchLower) > 0) {
        matchLower <- data.frame(fieldname = matchLower$fieldnameOrig.x, 
                                 standard = matchLower$standard,
                                 matchType = "Darwinized",
                                 stringsAsFactors = FALSE)
    }

    result <- data.frame(nameOld = c(matchIdentical$fieldname, matchLower$fieldname),
                         nameNew = c(matchIdentical$standard, matchLower$standard),
                         matchType = c(matchIdentical$matchType, matchLower$matchType),
                         stringsAsFactors = FALSE)
    return(result)
}

#' Combine old/new name for checkboxes
#' 
#' `combineOldNew()` is a function that combines (`paste`) fieldname and standard
#' names with unicode characters to present in checkboxes.
#' 
combineOldNew <- function(data, 
                          symbolEqual  = "\U2971", 
                          symbolRename = "\U21A0", 
                          symbolManual = "\U2192", 
                          symbolSpace  = "\U00A0") {
    data$SYMBOL <- 
        ifelse(data$matchType == "Identical", symbolEqual,
               ifelse(data$matchType == "Manual", symbolManual, symbolRename))
    result <- apply(data, 1, function(x) paste0(x[1], symbolSpace, x[4], "\n", x[2]))
    return(as.character(result))
}
