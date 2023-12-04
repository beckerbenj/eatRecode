#' Check Recode List
#'
#' @param recodeList A recode list.
#'
#' @return NULL
#'
#' @export
#' @examples
#' # tbd
checkRecode <- function(recodeList) {

  # Check input object type -------------------------------------------------
  if (!is.data.frame(recodeList)) stop("'recodeList' must be a data.frame.")

  # Check column names ------------------------------------------------------
  for (i in c("oldValues", "newValues")) {
    if (!(i %in% colnames(recodeList))) stop(paste0("'recodeList' must contain the column '", i, "'."))
  }

  # Check NA ----------------------------------------------------------------
  if (any(is.na(recodeList[, "oldValues"]))) stop("Please check the NAs in 'oldValues'.")

  # Check for contradicting entries in 'newValues' --------------------------
  recodeList_unique_rows <- unique(recodeList[, c("oldValues", "newValues")])

  ## Duplicates still remaining in oldValues have contradicting values in newValues:
  duplicated_oldValues <- recodeList_unique_rows[duplicated(recodeList_unique_rows$oldValues), "oldValues"]
  recodeList_newValues_diff <- recodeList_unique_rows[recodeList_unique_rows$oldValues %in% duplicated_oldValues, ]

  if (nrow(recodeList_newValues_diff) > 0) {
    recodeList_newValues_diff_sorted <- recodeList_newValues_diff[order(recodeList_newValues_diff$oldValues), ]

    stop(
      "There are contradicting entrys in 'newValues': \n \n",
      print_and_capture(recodeList_newValues_diff_sorted),
      call. = FALSE
    )
  }
}

## Simple function for printing a data.frame within an error message
print_and_capture <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}
