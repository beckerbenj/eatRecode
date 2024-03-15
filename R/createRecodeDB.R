#' Create Recode Data Base
#'
#' Create and store a recode data base in an Excel file.
#' The Excel file will have sheets corresponding to each recodeList name.
#'
#' @param recodeListList A named list of `recodeLists`.
#' @param filePath Path to the `.xlsx` file in which the data base should be stored.
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
#' # create csv-file with comma as decimal point (default)
#' filePath_temp <- tempfile(fileext = ".csv")
#' createRecodeDB(recodeListList = recodeListList, filePath = filePath_temp, fileType = "csv2")
#' createRecodeDB(recodeListList = recodeListList, filePath = filePath_temp)
#' # create csv-file
#' createRecodeDB(recodeListList = recodeListList, filePath = filePath_temp, fileType = "csv")
#' # create xlsx-file
#' filePath_temp <- tempfile(fileext = ".xlsx")
#' createRecodeDB(recodeListList = recodeListList, filePath = filePath_temp, fileType = "xlsx")
#'
#' @export
createRecodeDB <- function(recodeListList, filePath, fileType = "csv") {
  if (!is.list(recodeListList)) stop("'recodeListList' must be a named list of data.frames.")
  if (!is.data.frame(recodeListList[[1]])) stop("'recodeListList' must be named a list of data.frames.")
  if (is.null(names(recodeListList)) || any(is.na(names(recodeListList)))) stop("'recodeListList' must be named a list of data.frames.")
  lapply(recodeListList, checkRecodeList)

  switch(fileType,
         "xlsx" = {
           writexl::write_xlsx(recodeListList, path = filePath, col_names = TRUE)},
         "csv" = {
           for(i in 1:length(recodeListList)){
            write.csv(recodeListList[i], file = sub(".csv", paste0("_", names(recodeListList)[i],".csv"), filePath))
           }},
         "csv2" = {
           for(i in 1:length(recodeListList)){
             write.csv2(recodeListList[i], file = sub(".csv", paste0("_", names(recodeListList)[i],".csv"), filePath))
           }})

  NULL
}
