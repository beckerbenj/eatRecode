#' Use Recode List
#'
#' Use Recode List.
#'
#' @param df A data.frame.
#' @param oldCol Column in `df` which should be recoded.
#' @param recodeList A recode list.
#'
#' @return The \code{df} with a recoded \code{newValues}.
#'
#'
#' @examples
#' # tbd
#'
#' @export
useRecodeList <- function(df, oldCol, recodeList) {
  checkRecodeList(recodeList)
  if (!is.data.frame(df)) stop("'df' must be a data.frame.")
  if (!(oldCol %in% colnames(df))) stop(paste("The column", oldCol, "is not part of the 'df'."))

  df$oldValues <- df[, oldCol]
  df$newValues <- eatTools::recodeLookup(df[, oldCol], lookup = recodeList)
  df[!df[, oldCol] %in% recodeList[, 1], "newValues"] <- NA

  return(df)
}
