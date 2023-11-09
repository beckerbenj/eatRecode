#' Update Recode Data Base
#'
#' Update a recode data base.
#'
#'@param newRecodes A \code{data.frame} containing new recode information.
#'@param recodeDBPath Path to the \code{.xlsx} file in which the data base is stored.
#'@param name Name of the specific recode list.
#'
#'@return NULL
#'
#'
#'@examples
#'# tbd
#'
#'@export
updateRecodeDB <- function(newRecodes, recodeDBPath, name) {

  # writexl does not allow overwriting a single sheet in an existing .xlsx => this could be done via openxlsx
  # thereby it could be avoided to load the complete db
  recode_db <- getRecodeDB(filePath = recodeDBPath)
  old_recode_list <- recode_db[[name]]
  checkRecodeList(old_recode_list)

  # extract only new recodes, make unique
  newRecodes_manual_only <- unique(newRecodes[!newRecodes$oldValues %in% old_recode_list$oldValues, ])
  # tbd: check if there are any contradicting recodes? should this be a universal check in other functions?

  updated_recode_list <- rbind(old_recode_list, newRecodes_manual_only)

  # order recode list? so far, no ordering has been performed? should be implemented in createRecodeDB as well?
  recode_db[[name]] <- updated_recode_list[order(updated_recode_list$oldValues), ]

  # overwrite existing excel sheet
  writexl::write_xlsx(recode_db, path = recodeDBPath, col_names = TRUE)

  NULL
}
# (update should be possible via test_lookup_manual and test_lookup_autoRec!)



