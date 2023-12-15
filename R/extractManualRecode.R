#' Get manual recodes
#'
#' @param recodedList A data frame with the automatically recoded values.
#' @return Returns a data frame wich only includes the values that have to be recoded manually. This can be saved to excel for manual recoding.
#'
#' @examples # tbd
extractManualRecode <- function(recodedList) {
  # Check input object type -------------------------------------------------
  checkmate::assert_data_frame(recodedList)

  # Check column names ------------------------------------------------------
  checkmate::assert_subset(c("newValues"), choices = colnames(recodedList))

  recodedList[is.na(recodedList$newValues), ]
}
