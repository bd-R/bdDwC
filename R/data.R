#' Darwin Cloud Data
#'
#' Darwin Cloud data used to Darwinize. Darwin Cloud data was retrieved using
#' `bdDwC::download_cloud_data()` function. This dataset contains Darwin Cloud
#' data and a download date.
#'
#' @format A list of length two. First entry contains a data frame with
#' 403 rows and 2 columns. Second entry contains a `Date` class object.
#'
"data_darwin_cloud"

#' Darwin Core Info
#'
#' Darwin Core Info dataset stores standard names/terms and their definitions.
#' This object was created using `bdDwC::get_darwin_core_info()` function.
#'
#' @format A data frame with 203 rows and 2 columns.
#'
"data_darwin_core_info"


#' Indian Reptiles
#'
#' Dataset that contains subsample of Indian reptile observations
#' from the iNaturalist platform.
#'
#' @format A data frame with 50 rows and 41 columns.
#'
"data_reptiles"