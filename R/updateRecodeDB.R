#' Update Recode Data Base
#'
#' Update a recode data base.
#'
#'@param newRecodes A \code{data.frame} containing new recode information.
#'@param recodeDBPath Path to the \code{.xlsx} file in which the data base is stored.
#'@param newRecodeDBPath Path to the \code{.xlsx} file in which the updated data base should be stored.
#'@param name Name of the specific recode list.
#'@param override Logical of length 1. Should existing recode pairs be overwritten when conflicting newer recode pairs
#'are present in the \code{newRecodes}?
#'
#'@return NULL
#'
#'
#'@examples
#'# tbd
#'
#'@export
updateRecodeDB <- function(newRecodes, recodeDBPath, newRecodeDBPath, name, override = FALSE) {
  recode_db <- getRecodeDB(filePath = recodeDBPath)
  old_recode_list <- recode_db[[name]]
  checkRecodeList(old_recode_list)

  oldValues_conflicts <- character()
  if(override) {
    newRecodes_manual <- unique(newRecodes)
    oldValues_conflicts <- newRecodes_manual[newRecodes_manual$oldValues %in% old_recode_list$oldValues, "oldValues"]
    if(length(oldValues_conflicts) > 0) {
      message("The recodes for the following oldValues in the existing data base in sheet '", name, "' will be overwritten: ",
              paste(oldValues_conflicts, collapse = ", "))
      #browser()
    }
  } else{
    newRecodes_manual <- unique(newRecodes[!newRecodes$oldValues %in% old_recode_list$oldValues, ])
  }

  ## if necessary, override & order
  updated_recode_list <- rbind(old_recode_list[!old_recode_list$oldValues %in% oldValues_conflicts, ], newRecodes_manual)
  updated_recode_list <- updated_recode_list[order(updated_recode_list$oldValues), ]
  recode_db[[name]] <- updated_recode_list

  # overwrite existing excel sheet
  writexl::write_xlsx(recode_db, path = newRecodeDBPath, col_names = TRUE)

  NULL
}


