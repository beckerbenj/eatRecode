#' Check Recode List
#'
#' Check Recode List.
#'
#' @param recodeList A recode list.
#'
#' @return NULL
#'
#'
#' @examples
#' # tbd
#'
#' @export
checkRecodeList <- function(recodeList) {
  if (!is.data.frame(recodeList)) stop("'recodeList' must be a data.frame.")
  # names of df
  # uniqueness of rows (now duplicates in 'to recode')

  invisible(NULL)
}
