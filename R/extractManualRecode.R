#' Get manual recodes
#'
#' @param recodedList A data frame with the automatically recoded values.
#' @return Returns a data frame wich only includes the values that have to be recoded manually. This can be saved to excel for manual recoding.
#'
#' @examples # tbd
extractManualRecode <- function(recodedList) {
  checkRecodeList(recodedList)
  recodedList[is.na(recodedList$newValues), ]
}
