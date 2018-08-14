#' Retrieve Darwin Cloud Data
#' 
#' `getCloudData()` is a function (not exported) to download and save
#' Darwin Cloud data. This data is used to rename user data with function
#' `darwinazeNames()`. Additionally this function saves date of the download so
#' that this information could be shown in a `shiny` app. Therefore we create 
#' a list (Darwind Cloud data and time stamp (`Sys.Date()`)).
#' 
#' @param pathCloud Path to Darwin Cloud data.
#' 
#' @return NULL.
#' 
#' @examples \dontrun{
#'     getCloudData()
#' }
#' 
getCloudData <- function(pathCloud = "https://raw.githubusercontent.com/kurator-org/kurator-validation/master/packages/kurator_dwca/data/vocabularies/darwin_cloud.txt") {
    dataDarwinCloud <- list(data = downloadCloudData(pathCloud), date = Sys.Date())
    saveRDS(dataDarwinCloud, "dataDarwinCloud.RDS")
    return(NULL)
}

#' Download Darwin Cloud Data
#' 
#' `downloadCloudData()` is a function (not exported) to download Darwin Cloud 
#' data. First it downloads data, then it subsets informative columns and 
#' renames them so they could be used in `shiny` app.
#' 
#' @param pathCloud Path to Darwin Cloud data.
#' @param columnField Name of the column that contains field information.
#' @param columnStandard Name of the column that contains standard information.
#' 
#' @return data.frame of Darwin Cloud data.
#' 
downloadCloudData <- function(pathCloud = NULL, columnField = "fieldname", columnStandard = "standard") {
    # Using data.table::fread for speed
    data <- data.table::fread(pathCloud, 
                              data.table = FALSE,
                              verbose = FALSE,
                              showProgress = FALSE)
    # Subset only used columns
    data <- subset(data, select = c(columnField, columnStandard))
    # Rename to match names in shiny server 
    colnames(data) <- c(columnField, columnStandard)

    # Add values for missing standard
    # Don't know why they are not there
    # Had to go through the list manually check them
    # how many missing values:
    # sum(data$stand == "")
    # data
    return(data)
}