#' Get Recode List
#'
#' Import a recode list from an Excel file containing a data base of different `recodeLists`.
#'
#' @param directory Path to the directory where the data base is stored.
#' @param DBname Name of the database.
#' @param ListName Name of the specific recode list to be imported.
#' @param fileType "csv2" (default), "csv", "xlsx"
#'
#' @return A recode list.
#'
#' @examples
#' Countries <- list( Europe = data.frame(
#'                                   id = 1:4,
#'                                   oldValues = c("Berlin", "Copenhagen", "Rome", "Madrid"),
#'                                   newValues = c("Germany", "Denmark", "Italy" , "Spain")),
#'                    Asia = data.frame(
#'                                   id = 1:4,
#'                                   oldValues = c("Baku", "Tokyo", "Kathmandu", "Singapore"),
#'                                   newValues = c("Azerbaijan", "Japan", "Nepal" , "Singapore")))
#' Countries
#' directory <- tempdir()
#' createRecodeDB(recodeListList = Countries,
#'                directory = directory,
#'                DBname = "Countries",
#'                fileType = "csv2")
#' # Import lists from the data base
#' getRecodeList(directory = directory, DBname = "Countries", ListName = "Europe", fileType = "csv2")
#'
#' @export
getRecodeList <- function(directory = getwd(), DBname, ListName, fileType = "csv2") {
  if(fileType == "xlsx") {
    List <- as.data.frame(readxl::read_xlsx(paste0(directory,"\\",DBname,".xlsx"), sheet = ListName))
  } else if (fileType == "csv") {
    List <- utils::read.csv(paste0(directory,"\\",DBname, "\\", ListName, ".csv"))
  } else {
    List <- utils::read.csv2(paste0(directory,"\\",DBname, "\\", ListName, ".csv"))
  }
  return(List)
}

#' Get Recode Data Base
#'
#' Import a complete recode data base containing multiple `recodeLists`.
#'
#' @inheritParams getRecodeList
#'
#' @examples
#' # Create recode list data base using `createRecodeDB`
#' Countries <- list( Europe = data.frame(
#'                                   id = 1:4,
#'                                   oldValues = c("Berlin", "Copenhagen", "Rome", "Madrid"),
#'                                   newValues = c("Germany", "Denmark", "Italy" , "Spain")),
#'                    Asia = data.frame(
#'                                   id = 1:4,
#'                                   oldValues = c("Baku", "Tokyo", "Kathmandu", "Singapore"),
#'                                   newValues = c("Azerbaijan", "Japan", "Nepal" , "Singapore")))
#' Countries
#' directory <- tempdir()
#' createRecodeDB(recodeListList = Countries,
#'                directory = directory,
#'                DBname = "Countries",
#'                fileType = "csv")
#' # Import data base
#' getRecodeDB(filePath = filePath_temp)
#'
#' @export
getRecodeDB <- function(directory = getwd(), DBname, fileType = "csv2") {

  # xlsx files -----------------------------------------------------------------
  if(fileType == "xlsx") {
    sheet_names <- readxl::excel_sheets(paste0(directory,"\\",DBname,".xlsx"))
    names(sheet_names) <- sheet_names
    recodeListList <- lapply(sheet_names, function(sheet_name) {
      as.data.frame(readxl::read_xlsx(paste0(directory,"\\",DBname,".xlsx"), sheet = sheet_name))
    })
    return(recodeListList) }

  # csv files ------------------------------------------------------------------
  file_names <- list.files(path = paste0(directory,"\\",DBname), pattern = ".csv")
  names(file_names) <- sub(".csv", "", file_names)

  if(fileType == "csv") {
    recodeListList <- lapply(file_names, function(file_name) {
      utils::read.csv(paste0(directory,"\\",DBname, "\\", file_name))
    })
  } else {
    recodeListList <- lapply(file_names, function(file_name) {
      utils::read.csv2(paste0(directory,"\\",DBname, "\\", file_name))
    })
  }
  return(recodeListList)
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
#' createRecodeDB(recodeListList = recodeListList, filePath = filePath_temp, fileType = "xlsx")
#' # Import data base
#' namesRecodeDB(filePath = filePath_temp)
#'
#' @export
namesRecodeDB <- function(directory = getwd(), DBname, fileType = "csv2") {
  if(fileType == "xlsx") {
    readxl::excel_sheets(paste0(directory,"\\",DBname,".xlsx"))
  } else {
    sub(".csv", "", list.files(path = paste0(directory,"\\",DBname), pattern = ".csv"))
  }
}
