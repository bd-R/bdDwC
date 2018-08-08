#' Retrieve Information about Darwin Core Terms
#' 
#' `getDarwinCoreInfo()` is a function (not exported) to download
#' Darwin Core Terms. This information is displayed when using manually renaming.
#' 
#' @param pathCloud Path to Darwin Cloud data.
#' 
#' @return data.frame that contains name and definition for each available
#' Darwin Cloud term.
#' 
#' @examples
#' getDarwinCoreInfo()
#' 
getDarwinCoreInfo <- function(pathDarwinCloud = "http://tdwg.github.io/dwc/terms/") {
    data <- readLines(pathDarwinCloud)
    data <- grep("Term Name:", data, value = TRUE)

    # Not very elegant, but works in base R
    # Name
    resultName <- gsub(".*Term Name: dcterms:([A-z]+).*", "\\1", data)
    resultName <- gsub(".*Term Name: ([A-z]+).*", "\\1", resultName)
    # Definition
    resultDefinition <- gsub(".*<TD>Definition:</TD><TD>(.*)</TD></TR>.*<TR><TD>Comment:.*", "\\1", data)
    resultDefinition <- gsub('"', "", resultDefinition)
    resultDefinition <- gsub("'", "", resultDefinition)
    # Final result
    result <- data.frame(name = resultName, 
                         definition = resultDefinition, 
                         stringsAsFactors = FALSE)
    return(result)
}