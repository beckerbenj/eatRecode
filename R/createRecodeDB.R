#' Create Recode Data Base
#'
#' Create a recode data base.
#'
#'@param recodeListList A named list of \code{recodeLists}.
#'@param filePath Path to the \code{.xlsx} file in which the data base should be stored.
#'
#'@return NULL
#'
#'
#'@examples
#'# tbd
#'
#'@export
createRecodeDB <- function(recodeListList, filePath) {
  if(!is.list(recodeListList)) stop("'recodeListList' must be a named list of data.frames.")
  if(!is.data.frame(recodeListList[[1]])) stop("'recodeListList' must be named a list of data.frames.")
  if(is.null(names(recodeListList)) || any(is.na(names(recodeListList)))) stop("'recodeListList' must be named a list of data.frames.")
  lapply(recodeListList, checkRecodeList)

  writexl::write_xlsx(recodeListList, path = filePath, col_names = TRUE)

  NULL
}
