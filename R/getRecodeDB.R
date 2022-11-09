#' Get Recode List
#'
#' Get a recode list.
#'
#'@param filePath Path to the \code{.xlsx} file in which the data base is stored.
#'@param name Name of the specific recode list.
#'
#'@return A recode list.
#'
#'
#'@examples
#'# tbd
#'
#'@export
getRecodeList <- function(filePath, name) {
  as.data.frame(readxl::read_xlsx(filePath, sheet = name))
}

#' Get Recode Data Base
#'
#' Get a complete recode data base.
#'
#'@param filePath Path to the \code{.xlsx} file in which the data base is stored.
#'
#'@return A recode list.
#'
#'
#'@examples
#'# tbd
#'
#'@export
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
#'@param filePath Path to the \code{.xlsx} file in which the data base is stored.
#'
#'@return A recode list.
#'
#'
#'@examples
#'# tbd
#'
#'@export
namesRecodeDB <- function(filePath) {
  readxl::excel_sheets(filePath)
}


