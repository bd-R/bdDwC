#' Rename Dataset According Darwinized Names
#' 
#' `renameUserData()` is a function (not exported) to rename given dataset
#' (usually given user data) according previously Darwinized Names.
#' 
#' @param dataUser data.frame of be renamed.
#' @param dataRenamed data.frame that was created using `bdDwC:::darwinazeNames()`
#' function using same `dataUser`.
#' 
#' @return data.frame of submitted user data, but renamed according given 
#' renames data.frame.
#' 
renameUserData <- function(dataUser, dataRenamed) {
    # Extract user names (we don't need all data just to rename)
    namesUser <- data.frame(nameOld = colnames(dataUser), 
                            stringsAsFactors = FALSE)
    # merge gives us names that were in renamed dataset
    result <- merge(namesUser, dataRenamed, "nameOld", all.x = TRUE)
    # However, merge reorders data - we need to sort it back 
    result <- result[match(namesUser$nameOld, result$nameOld), ]
    colnames(dataUser) <- ifelse(is.na(result$nameNew),
                                 result$nameOld, result$nameNew)
    return(as.data.frame(dataUser))
}