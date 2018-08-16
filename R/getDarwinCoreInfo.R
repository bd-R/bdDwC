#' Retrieve Information about Darwin Core Terms
#' 
#' `getDarwinCoreInfo()` is a function (not exported) to download
#' Darwin Core Terms. This information is displayed when using manually renaming.
#' 
#' @param pathDarwinCloud Path to Darwin Cloud data.
#' 
#' @return data.frame that contains name and definition for each available
#' Darwin Cloud term.
#' 
getDarwinCoreInfo <- function(pathDarwinCloud = "http://tdwg.github.io/dwc/terms/") {
    # Catching error as if there's no internet connection app wouldn't run
    data <- tryCatch({
        readLines(pathDarwinCloud, warn = FALSE)
    }, error = function(cond) {return(NA)})

    if (all(is.na(data))) {
        resultName       <- NA
        resultDefinition <- NA
    } else {
        data <- grep("Term Name:", data, value = TRUE)

        # Not very elegant, but works in base R
        # Name
        resultName <- gsub(".*Term Name: dcterms:([A-z]+).*", "\\1", data)
        resultName <- gsub(".*Term Name: ([A-z]+).*", "\\1", resultName)
        # Definition
        resultDefinition <- gsub(".*<TD>Definition:</TD><TD>(.*)</TD></TR>.*<TR><TD>Comment:.*", "\\1", data)
        resultDefinition <- gsub("'|\"", "", resultDefinition)
    }
    result <- data.frame(name = resultName, 
                         definition = resultDefinition, 
                         stringsAsFactors = FALSE)
    return(result)
}