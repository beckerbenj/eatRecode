#' Get manual recodes
#'
#' @param recodedList A data frame with the automatically recoded values.
#' @param varName Character string for the column containing the automatically recoded values.
#' @return Returns a data frame which only includes the values that have to be recoded manually. It can be saved to Excel for manual recoding.
#'
#' @examples
#' # example data frame
#' df <- data.frame(id = 1:4,
#'              country = c("Berlin", "Skotland", "Sweden" , "Cairo"),
#'              country_recoded = c("Germany", "NA", "Sweden", NA))
#' # extract values to recode manually
#' manual_recodes <- extractManualRecode(recodedList = df, varName = "country_recoded")
#' print(manual_recodes)
#' # export to Excel, edit, import
#' filePath <- paste0(tempdir(),"\\manual_recodes.xlsx")
#' writexl::write_xlsx(manual_recodes, path = filePath)
#' readxl::read_xlsx(filePath)
#'
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
