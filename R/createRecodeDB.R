#' Create Recode Data Base
#'
#' Create a recode data base.
#'
#' @param recodeListList A named list of `recodeLists`.
#' @param filePath Path to the `.xlsx` file in which the data base should be stored.
#'
#' @return NULL
#'
#'
#' @examples
#' # create a named list of data frames
#' recodeListList <- list( Europe = data.frame(id = 1:4,
#'                                             oldValues = c("Berlin", "Copenhagen", "Rome", "Madrid"),
#'                                             newValues = c("Germany", "Denmark", "Italy" , "Spain")),
#'                         Asia = data.frame(id = 1:4,
#'                                           oldValues = c("Baku", "Tokyo", "Kathmandu", "Singapore"),
#'                                           newValues = c("Azerbaijan", "Japan", "Nepal" , "Singapore")))
#' print(recodeListList)
#' filePath <- paste0(tempdir(),"\\recodeListList.xlsx")
#' # create the Excel-file
#' createRecodeDB(recodeListList = recodeListList, filePath = filePath)
#'
#' @export
#'
createRecodeDB <- function(recodeListList, filePath) {
  if (!is.list(recodeListList)) stop("'recodeListList' must be a named list of data.frames.")
  if (!is.data.frame(recodeListList[[1]])) stop("'recodeListList' must be named a list of data.frames.")
  if (is.null(names(recodeListList)) || any(is.na(names(recodeListList)))) stop("'recodeListList' must be named a list of data.frames.")
  lapply(recodeListList, checkRecodeList)

  writexl::write_xlsx(recodeListList, path = filePath, col_names = TRUE)

  NULL
}
