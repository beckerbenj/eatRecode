#' Get manual recodes
#'
#' @param recodedList A data frame with the automatically recoded values.
#' @param varName Character string for the column containing the automatically recoded values.
#' @return Returns a data frame which only includes the values that have to be recoded manually. It can be saved to Excel for manual recoding.
#'
#' @examples # tbd
#' @export
extractManualRecode <- function(recodedList, varName) {
  # Check input object type -------------------------------------------------
  checkmate::assert_data_frame(recodedList)

  # Check column names ------------------------------------------------------
  checkmate::assert_subset(c(varName), choices = colnames(recodedList))

  missing_recodes <- recodedList[is.na(recodedList[, varName]), ]
  missing_recodes$newValues <- missing_recodes[, varName]
  missing_recodes[, varName] <- NULL
  return(missing_recodes)
}
