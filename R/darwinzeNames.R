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
    dataUserSub <- dataUser[!dataUser$fieldnameOrig %in% matchIdentical$fieldname, ]
    dataDWCSub  <- dataDWC[!dataDWC$standard %in% matchIdentical$standard, ]

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
#' @param data data.frame of matched names (`darwinazeNames()` output)
#' @param symbolArrow character string for symbol that is used in `paste0` to connect
#' old and new name
#' @param symbolSpace character string for symbol used instead of white space 
#' 
#' @return data.frame of darwinized user names.
#' 
combineOldNew <- function(data, symbolArrow = "->", symbolSpace  = " ") {
    result <- apply(data, 1, function(x) paste0(x[1], symbolSpace, symbolArrow, "\n", x[2]))
    result <- as.character(result)
    return(result)
}
