#' Get Recode List
#'
#' Get a recode list.
#'
#' @param filePath Path to the `.xlsx` file in which the data base is stored.
#' @param name Name of the specific recode list.
#'
#' @return A recode list.
#'
#'
#' @examples
#' # Create recode list data base using `createRecodeDB`
#' recodeListList <- list( Europe = data.frame(
#'                                   id = 1:4,
#'                                   oldValues = c("Berlin", "Copenhagen", "Rome", "Madrid"),
#'                                   newValues = c("Germany", "Denmark", "Italy" , "Spain")),
#'                         Asia = data.frame(
#'                                   id = 1:4,
#'                                   oldValues = c("Baku", "Tokyo", "Kathmandu", "Singapore"),
#'                                   newValues = c("Azerbaijan", "Japan", "Nepal" , "Singapore")))
#' print(recodeListList)
#' filePath <- paste0(tempdir(),"\\recodeListList.xlsx")
#' createRecodeDB(recodeListList = recodeListList, filePath = filePath)
#' # Import lists from the data base
#' getRecodeList(filePath = filePath, name = "Europe")
#' getRecodeList(filePath = filePath, name = "Asia")
#'
#' @export
getRecodeList <- function(filePath, name) {
  as.data.frame(readxl::read_xlsx(filePath, sheet = name))
}

#' Get Recode Data Base
#'
#' Get a complete recode data base.
#'
#' @param filePath Path to the \code{.xlsx} file in which the data base is stored.
#'
#' @return A recode list.
#'
#'
#' @examples
#' # tbd
#'
#' @export
getRecodeDB <- function(filePath) {
  sheet_names <- readxl::excel_sheets(filePath)
  names(sheet_names) <- sheet_names
  recodeListList <- lapply(sheet_names, function(sheet_name) {
    as.data.frame(readxl::read_xlsx(filePath, sheet = sheet_name))
  })

  recodeListList
}


#' Get Names of Recode Data Base
#'
#' Get the names of the individual recode lists within a recode data base.
#'
#' @param filePath Path to the \code{.xlsx} file in which the data base is stored.
#'
#' @return A recode list.
#'
#'
#' @examples
#' # tbd
#'
#' @export
namesRecodeDB <- function(filePath) {
  readxl::excel_sheets(filePath)
}
