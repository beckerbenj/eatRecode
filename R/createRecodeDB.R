#' Create Recode Data Base
#'
#' Create and store a recode data base in an Excel (`.csv` or `.xlsx`) file.
#' For each recodeList, a separate `.csv` file will be generated.
#' The `.xlsx` file will have sheets corresponding to each recodeList name.
#'
#' @param recodeListList A named list of `recodeLists`.
#' @param directory path of the directory where the data base will be saved
#' @param DBname name of the database (will be used as filename)
#' @param fileType "csv2" (default), "csv", "xlsx"
#'
#' @return NULL
#'
#'
#' @examples
#' # create a named list of data frames
#' recodeListList <- list( Europe = data.frame(
#'                                   id = 1:4,
#'                                   oldValues = c("Berlin", "Copenhagen", "Rome", "Madrid"),
#'                                   newValues = c("Germany", "Denmark", "Italy" , "Spain")),
#'                         Asia = data.frame(
#'                                   id = 1:4,
#'                                   oldValues = c("Baku", "Tokyo", "Kathmandu", "Singapore"),
#'                                   newValues = c("Azerbaijan", "Japan", "Nepal" , "Singapore")))
#' recodeListList
#' # create .csv file with comma as decimal point (default)
#' directory <- tempdir()
#' createRecodeDB(recodeListList = recodeListList, directory = directory, DBname = "Countries", fileType = "csv2")
#'
#' createRecodeDB(recodeListList = recodeListList, filePath = filePath_temp)
#' # create .csv file
#' createRecodeDB(recodeListList = recodeListList, filePath = filePath_temp, fileType = "csv")
#' # create .xlsx file
#' filePath_temp <- tempfile(fileext = ".xlsx")
#' createRecodeDB(recodeListList = recodeListList, filePath = filePath_temp, fileType = "xlsx")
#'
#' @export
createRecodeDB <- function(recodeListList, directory = getwd(), DBname, fileType = "csv") {

  # checks ---------------------------------------------------------------------
  if (!is.list(recodeListList)) stop("'recodeListList' must be a named list of data.frames.")
  if (!is.data.frame(recodeListList[[1]])) stop("'recodeListList' must be named a list of data.frames.")
  if (is.null(names(recodeListList)) || any(is.na(names(recodeListList)))) stop("'recodeListList' must be named a list of data.frames.")
  lapply(recodeListList, checkRecodeList)

  # xlsx files -----------------------------------------------------------------
  if(fileType == "xlsx") {
    writexl::write_xlsx(recodeListList,
                        path = paste0(directory,"\\",DBname,".xlsx"),
                        col_names = TRUE)
    return(NULL) }

  # csv files ------------------------------------------------------------------
  dirpath <- file.path(directory,
                       DBname,
                       fsep = "\\")
  unlink(dirpath, recursive = TRUE) # delete existing file ("overwrite")
  dir.create(dirpath, showWarnings = FALSE) # create data base directory

  if(fileType == "csv") {
    for(i in 1:length(recodeListList)){
    utils::write.csv(recodeListList[[i]], file = paste0(dirpath, "\\", names(recodeListList)[i], ".csv"))
    }
  } else {
    for(i in 1:length(recodeListList)){
      utils::write.csv2(recodeListList[[i]], file = paste0(dirpath, "\\", names(recodeListList)[i], ".csv"))
    }
  }

  NULL
}
