#' Check Recode List
#'
#' Checks whether a data frame called 'recodeList' has columns 'oldValues', containing the values to be recoded, and 'newValues', containing the new values which replace the old ones.
#' @param recodeList A recode list.
#'
#' @return NULL
#'
#' @export
#' @examples
#' recodeList <- data.frame(oldValues = c("Berlin", "Kairo", "England", "Schottland"), newValues = c("Deutschland", "Ägypten", "England", "Schottland"))
#' checkRecodeList(recodeList)
checkRecodeList <- function(recodeList) {

  # Check input object type -------------------------------------------------
  checkmate::assert_data_frame(recodeList)

  # Check column names ------------------------------------------------------
  checkmate::assert_subset(c("oldValues", "newValues"), choices = colnames(recodeList))

  # Check NA ----------------------------------------------------------------
  checkmate::assert_character(recodeList$oldValues, any.missing = FALSE)


  # Check for contradicting entries in 'newValues' --------------------------
  recodeList_unique_rows <- unique(recodeList[, c("oldValues", "newValues")])

  ## Duplicates still remaining in oldValues have contradicting values in newValues:
  duplicated_oldValues <- recodeList_unique_rows[duplicated(recodeList_unique_rows$oldValues), "oldValues"]
  recodeList_newValues_diff <- recodeList_unique_rows[recodeList_unique_rows$oldValues %in% duplicated_oldValues, ]

  if (nrow(recodeList_newValues_diff) > 0) {
    recodeList_newValues_diff_sorted <- recodeList_newValues_diff[order(recodeList_newValues_diff$oldValues), ]

    stop(
      "There are contradicting entrys in 'newValues': \n \n",
      print_and_capture(recodeList_newValues_diff_sorted)
    )
  }
}

## Simple function for printing a data.frame within an error message
print_and_capture <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}

rename_column <- function(df, oldName, newName){
  colnames(df)[colnames(df) == oldName] <- newName
  return(df)
}
