#' Use Recode List
#'
#' Use Recode List.
#'
#' @param df A data.frame.
#' @param varName Column in `df` which should be recoded.
#' @param new_varName Column in which the recoded values will be stored.
#' @param recodeList A recode list.
#'
#' @return The \code{df} with a recoded \code{newValues}.
#'
#'
#' @examples
#' # tbd
#'
#' @export
useRecodeList <- function(df, varName, new_varName, recodeList) {
  checkRecodeList(recodeList)
  if (!is.data.frame(df)) stop("'df' must be a data.frame.")
  if (!(varName %in% colnames(df))) stop(paste("The column", varName, "is not part of the 'df'."))

  df[ , new_varName] <- eatTools::recodeLookup(df[, varName], lookup = recodeList)
  df[!df[, varName] %in% recodeList[, 1], new_varName] <- NA

  return(df)
}
