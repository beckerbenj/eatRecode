#' Use Recode List
#'
#' Add a new column to a data frame containing recoded values from a chosen column in that data frame.
#'
#' @param df A data.frame.
#' @param varName Column in `df` which should be recoded.
#' @param new_varName Column in which the recoded values will be stored.
#' @param recodeList A recode list: a data frame with the columns "oldValues" and "newValues".
#'
#' @return The `df` with a recoded `newValues`.
#'
#'
#' @examples
#' # example data frame
#' df <- data.frame(id = 1:4, country = c("Berlin", "Kairo", "Englant", "Schottland"))
#' # example recode list
#' recodeList <- data.frame(oldValues = c("Berlin", "Kairo", "Englant"),
#'                          newValues = c("Deutschland", "Ägypten", "England"))
#'
#' useRecodeList(df, varName = "country", new_varName = "r_country", recodeList)
#'
#' @export
useRecodeList <- function(df, varName, new_varName, recodeList) {
  checkRecodeList(recodeList)
  checkmate::assert_data_frame(df)
  checkmate::assert_subset(varName, choices = colnames(df))

  df[ , new_varName] <- eatTools::recodeLookup(df[, varName], lookup = recodeList)
  df[!df[, varName] %in% recodeList[, 1], new_varName] <- NA

  return(df)
}
