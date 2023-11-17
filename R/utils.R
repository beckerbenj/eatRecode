#' Check Recode List
#'
#' @param recodeList A recode list.
#' @noRd
#'
#' @return NULL
#'
#' @examples
#' # tbd
checkRecode <- function(dat) {
  # Check input object type -------------------------------------------------
  if (!is.data.frame(dat)) stop("'dat' must be a data.frame.")

  # Check column names ------------------------------------------------------
  for (i in c("newValues", "oldValues")) {
    if (!(i %in% colnames(dat))) stop(paste0("'dat' must contain the column '", i, "'."))
  }

  # Check NA ----------------------------------------------------------------
  if (any(is.na(dat[, "oldValues"]))) stop("Please check the NAs in 'oldValues'.")

  # Check for contradicting entries in 'newValues' --------------------------
  dat_unique_rows <- unique(dat[, c("oldValues", "newValues")])

  ## Duplicates still remaining in oldValues have contradicting values in newValues:
  duplicated_oldValues <- dat_unique_rows[duplicated(dat_unique_rows$oldValues), "oldValues"]
  dat_newValues_diff <- dat_unique_rows[dat_unique_rows$oldValues %in% duplicated_oldValues, ]

  if (nrow(dat_newValues_diff) > 0) {
    dat_newValues_diff_sorted <- dat_newValues_diff[order(dat_newValues_diff$oldValues), ]

    stop(
      paste(
        "There are contradicting entrys in 'newValues': \n \n",
        print_and_capture(dat_newValues_diff_sorted)
      ),
      call. = FALSE
    )
  }
}

## Simple function for printing a data.frame within an error message
print_and_capture <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}
