#' Create Recode Data Base
#'
#' Create and store a recode data base in an Excel (`.csv` or `.xlsx`) file.
#' For each recodeList, a separate `.csv` file will be generated.
#' The `.xlsx` file will have sheets corresponding to each recodeList name.
#'
#'
#' @param recodeListList A named list of `recodeLists`.
#' @param directory Path of the directory where the data base will be saved.
#' @param DBname Name of the database (will be used as filename).
#' @param fileType `csv2` (default), `csv`, `xlsx`.
#' @param overwrite If there already is a database on the specified path, should it be overwritten?
#'
#' @return NULL
#'
#' @examples
#' # create a named list of data frames
#' Countries <- list( Europe = data.frame(
#'                                   id = 1:4,
#'                                   oldValues = c("Berlin", "Copenhagen", "Rome", "Madrid"),
#'                                   newValues = c("Germany", "Denmark", "Italy" , "Spain")),
#'                         Asia = data.frame(
#'                                   id = 1:4,
#'                                   oldValues = c("Baku", "Tokyo", "Kathmandu", "Singapore"),
#'                                   newValues = c("Azerbaijan", "Japan", "Nepal" , "Singapore")))
#' Countries
#' directory <- tempdir()
#' # create .csv file with comma as decimal point (default)
#' createRecodeDB(recodeListList = Countries,
#'                directory = directory,
#'                DBname = "Countries",
#'                fileType = "csv2",
#'                overwrite = TRUE)
#' # create .csv file
#' createRecodeDB(recodeListList = Countries,
#'                directory = directory,
#'                DBname = "Countries",
#'                fileType = "csv",
#'                overwrite = TRUE)
#' # create .xlsx file
#' createRecodeDB(recodeListList = Countries,
#'                directory = directory,
#'                DBname = "Countries",
#'                fileType = "xlsx",
#'                overwrite = TRUE)
#'
#' @export
createRecodeDB <- function(recodeListList, directory, DBname, fileType = "csv2", overwrite = FALSE) {

  # checks ---------------------------------------------------------------------
  if (!is.list(recodeListList)) stop("'recodeListList' must be a named list of data.frames.")
  if (!is.data.frame(recodeListList[[1]])) stop("'recodeListList' must be named a list of data.frames.")
  if (is.null(names(recodeListList)) || any(is.na(names(recodeListList)))) stop("'recodeListList' must be named a list of data.frames.")
  lapply(recodeListList, checkRecodeList)

  # xlsx files -----------------------------------------------------------------
  if(fileType == "xlsx") {
    if(file.exists(paste0(directory,"/",DBname,".xlsx")) & !overwrite) {stop("There already is a database on your path. Please rename or move.")}
    writexl::write_xlsx(recodeListList,
                        path = paste0(directory,"/",DBname,".xlsx"),
                        col_names = TRUE)
   return(paste0("Successfully created ", DBname, ".xlsx")) }

  # csv files ------------------------------------------------------------------
  dirpath <- file.path(directory,
                       DBname,
                       fsep = "/")
  if(dir.exists(dirpath) & !overwrite){stop("There already is a database on your path. Please rename or move.")}
  if(overwrite) {unlink(dirpath, recursive = TRUE)} # delete existing file
  dir.create(dirpath, showWarnings = FALSE) # create data base directory

  if(fileType == "csv") {
    for(i in 1:length(recodeListList)){
    utils::write.csv(recodeListList[[i]], file = paste0(dirpath, "/", names(recodeListList)[i], ".csv"), row.names=FALSE)
    }
  } else if (fileType == "csv2") {
    for(i in 1:length(recodeListList)){
      utils::write.csv2(recodeListList[[i]], file = paste0(dirpath, "/", names(recodeListList)[i], ".csv"), row.names=FALSE)
    }
  } else {return("FileType must be `csv2`, `csv`, or `xlsx`.")}

  return(paste0("Successfully created ", DBname, ".csv"))
}
