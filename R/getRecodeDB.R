#' Get Recode List
#'
#' Import a recode list from an Excel file containing a data base of different `recodeLists`.
#'
#' @param filePath Path to the `.xlsx` file in which the data base is stored.
#' @param name Name of the specific recode list (Excel sheet) to be imported.
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
#' recodeListList
#' filePath_temp <- tempfile(fileext = ".xlsx")
#' createRecodeDB(recodeListList = recodeListList, filePath = filePath_temp)
#' # Import lists from the data base
#' getRecodeList(filePath = filePath_temp, name = "Europe")
#' getRecodeList(filePath = filePath_temp, name = "Asia")
#'
#' @export
getRecodeList <- function(filePath, name) {
  as.data.frame(readxl::read_xlsx(filePath, sheet = name))
}

#' Get Recode Data Base
#'
#' Import a complete recode data base containing multiple `recodeLists`.
#'
#' @inheritParams getRecodeList
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
#' recodeListList
#' filePath_temp <- tempfile(fileext = ".xlsx")
#' createRecodeDB(recodeListList = recodeListList, filePath = filePath_temp)
#' # Import data base
#' getRecodeDB(filePath = filePath_temp)
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
#' @inheritParams getRecodeList
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
#' recodeListList
#' filePath_temp <- tempfile(fileext = ".xlsx")
#' createRecodeDB(recodeListList = recodeListList, filePath = filePath_temp)
#' # Import data base
#' namesRecodeDB(filePath = filePath_temp)
#'
#' @export
namesRecodeDB <- function(filePath) {
  readxl::excel_sheets(filePath)
}
