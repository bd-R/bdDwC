renameUserData <- function(dataUser, dataRenamed) {
    namesUser <- data.frame(nameOld = tolower(colnames(dataUser)), 
                            stringsAsFactors = FALSE)
    result <- merge(namesUser, dataRenamed, "nameOld", all.x = TRUE)
    result <- result[match(namesUser$nameOld, result$nameOld), ]
    colnames(dataUser) <- ifelse(is.na(result$nameNew),
                                 result$nameOld, result$nameNew)
    return(dataUser)
}