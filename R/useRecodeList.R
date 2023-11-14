#' Use Recode List
#'
#' Use Recode List.
#'
#' @param df A data.frame.
#' @param oldCol Column in `df` which should be recoded.
#' @param newCol Column name in `df` which should contain the recoded values.
#' @param recodeList A recode list.
#'
#' @return The \code{df} with a recoded \code{newCol}.
#'
#'
#' @examples
#' # tbd
#'
#' @export
useRecodeList <- function(df, oldCol, newCol, recodeList) {
  checkRecodeList(recodeList)
  if (!is.data.frame(df)) stop("'df' must be a data.frame.")
  if (!newCol %in% names(df)) stop("'oldCol' must be a variable in 'df'.")
  # newCol in df?

  # browser()

  df[, newCol] <- eatTools::recodeLookup(df[, oldCol], lookup = recodeList)
  df[!df[, oldCol] %in% recodeList[, 1], newCol] <- NA

  df
}
