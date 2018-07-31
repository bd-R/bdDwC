darwinazeNames <- function(dataUser, dataReference) {
    dataReference <- data.frame(apply(dataReference, 2, tolower), 
                             stringsAsFactors = FALSE)
    dNames <- data.frame(fieldname = tolower(names(dataUser)), 
                         stringsAsFactors = FALSE)
    dOverlaps <- merge(dNames, dataReference, by = "fieldname", all.x = TRUE)
    dOverlaps <- dOverlaps[match(dNames$fieldname, dOverlaps$fieldname), ]
    stopifnot(all(dOverlaps$fieldname == dNames$fieldname))
    subset(na.omit(dOverlaps), fieldname != standard)
}
