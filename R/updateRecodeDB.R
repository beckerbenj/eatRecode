#' Update Recode Data Base
#'
#' Update a recode data base.
#'
#' @param newRecodes A `data.frame` containing new recode information.
#' @param recodeDBPath Path to the `.xlsx` file in which the data base is stored.
#' @param newRecodeDBPath Path to the `.xlsx` file in which the updated data base should be stored.
#' @param name Name of the specific recode list.
#' @param override Logical of length 1. Should existing recode pairs be overwritten when conflicting newer recode pairs
#' are present in the `newRecodes`?
#'
#' @return NULL
#'
#'
#' @examples
#' # tbd
#'
#' @export
updateRecodeDB <- function(newRecodes, recodeDBPath, newRecodeDBPath, name, override = FALSE) {
  recode_db <- getRecodeDB(filePath = recodeDBPath)
  old_recode_list <- recode_db[[name]]
  checkRecodeList(old_recode_list)

  oldValues_conflicts <- data.frame(oldValues = character(), newValues = character())
  if (override) {
    newRecodes_manual <- unique(newRecodes)
    oldValues_conflicts <- newRecodes_manual[newRecodes_manual$oldValues %in% old_recode_list$oldValues, ]

    # recodes identical to the existing ones in the data base should not be reported
    number_of_conflicts <- nrow(oldValues_conflicts)
    for(i in eatTools::seq2(from = 1, to = number_of_conflicts)) {
      oldValues_conflicts[i, "old_newValues"] <- old_recode_list[old_recode_list$oldValues ==
                                                                    oldValues_conflicts[i, "oldValues"], "newValues"]
    }
    oldValues_conflicts_filtered <- oldValues_conflicts[oldValues_conflicts$newValues != oldValues_conflicts$old_newValues, ]

    number_of_actual_conflicts <- nrow(oldValues_conflicts_filtered)
    if (number_of_actual_conflicts > 0) {
      conflict_details <- paste0(oldValues_conflicts_filtered$oldValues, " -> ",
                                 oldValues_conflicts_filtered$old_newValues, "; now: ",
                                 oldValues_conflicts_filtered$newValues)

      message(
        "The following recode pairs in the existing data base in sheet '",
        name, "' will be overwritten:\n",
        paste(conflict_details, collapse = "\n")
      )
    }
  } else {
    newRecodes_manual <- unique(newRecodes[!newRecodes$oldValues %in% old_recode_list$oldValues, ])
  }

  ## if necessary, override & order
  updated_recode_list <- rbind(old_recode_list[!old_recode_list$oldValues %in% oldValues_conflicts$oldValues, ], newRecodes_manual)
  updated_recode_list <- updated_recode_list[order(updated_recode_list$oldValues), ]
  recode_db[[name]] <- updated_recode_list

  # overwrite existing excel sheet
  writexl::write_xlsx(recode_db, path = newRecodeDBPath, col_names = TRUE)

  NULL
}
