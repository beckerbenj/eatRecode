#' Get manual recodes
#'
#' @param recodedDf A data frame with the automatically recoded values.
#' @param varName Character string for the column containing the automatically recoded values.
#' @return Returns a data frame which only includes the values that have to be recoded manually. It can be saved to Excel for manual recoding.
#'
#' @examples
#' # example data frame
#' df <- data.frame(id = 1:4,
#'              country = c("Berlin", "Skotland", "Sweden" , "Cairo"),
#'              country_recoded = c("Germany", NA, "Sweden", NA))
#' # extract values to recode manually
#' manual_recodes <- extractManualRecode(recodedDf = df, varName = "country_recoded")
#' manual_recodes
#' # export to Excel, edit, import
#' filePath_temp <- tempfile(fileext = ".xlsx")
#' writexl::write_xlsx(manual_recodes, path = filePath_temp)
#' readxl::read_xlsx(filePath_temp)
#'
#' @export
extractManualRecode <- function(recodedDf, varName) {
  # Check input object type -------------------------------------------------
  checkmate::assert_data_frame(recodedDf)

  # Check column names ------------------------------------------------------
  checkmate::assert_subset(c(varName), choices = colnames(recodedDf))

  missing_recodes <- recodedDf[is.na(recodedDf[, varName]), ]
  missing_recodes$newValues <- missing_recodes[, varName]
  missing_recodes[, varName] <- NULL
  return(missing_recodes)
}
