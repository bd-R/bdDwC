#' Download Darwin Cloud Data
#' 
#' `downloadCloudData()` is a function used to download Darwin Cloud 
#' data. First it downloads data, then it subsets informative columns and 
#' renames them so they could be used in `shiny` app.
#' 
#' @param pathRemote Path to remote repository (Kurator in Github).
#' @param pathGithub Path within given repository.
#' @param pathFile Name of a file.
#' @param columnField Name of the column that contains field information.
#' @param columnStand Name of the column that contains standard information.
#' 
#' @return data.frame of Darwin Cloud data.
#' 
#' @examples
#' downloadCloudData()
#' 
#' @importFrom utils read.csv
#' 
#' @export
#' 
downloadCloudData <- function(
    pathRemote  = "https://raw.githubusercontent.com/kurator-org",
    pathGithub  = "/kurator-validation/master/packages/kurator_dwca/data/vocabularies/",
    pathFile    = "darwin_cloud.txt",
    columnField = "fieldname",
    columnStand = "standard") {
    pathCloud <- paste0(pathRemote, pathGithub, pathFile)
    data <- read.csv(pathCloud, sep = "\t")
    # Subset only used columns
    data <- subset(data, select = c(columnField, columnStand))
    # Rename to match names in shiny server
    colnames(data) <- c(columnField, columnStand)

    # Add values for missing standard
    return(data)
}