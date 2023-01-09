#' Get manual recodes
#'
#' @param recodedList A data frame with the automatically recoded values.
#' @return Returns a data frame wich only includes the values that have to be recoded manually. This can be saved to excel for manual recoding.
#'
#' @examples # tbd

manualRecode <- function(recodedList){

  if(!is.data.frame(recodedList)) stop("'df' must be a data.frame.")

  recodedList[is.na(recodedList$newValues), ]

}
