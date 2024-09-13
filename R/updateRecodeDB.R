#' Update Recode Data Base
#'
#' Update a recode data base stored in an Excel file.
#' Takes new recode information, compares it to the existing recode database, and updates the database accordingly.
#'
#' @inheritParams getRecodeList
#' @param newRecodes A `data.frame` containing new recode information.
#' @param oldValues A character string of the column name containing the old values in the `newRecodes` data.frame.
#' @param newValues A character string of the column name containing the newly recoded values in the `newRecodes data.frame`.
#' @param replace Logical of length 1. Should existing recode pairs be overwritten when conflicting newer recode pairs.
#' @param newDirectory If the updated data base should be stored in a different directory, specify its path here.
#' @param newDBname If the updated data base should be stored under a different name, specify it here.
#' are present in the `newRecodes`?
#'
#' @return NULL
#'
#' @examples
#' # example data base
#' oldDatabase <- list( Europe = data.frame(oldValues = c("Berlin", "Copenhagen", "Rome", "Madrid"),
#'                                          newValues = c("France", "Denmark", "Italy" , "Spain")),
#'                      Asia = data.frame(oldValues = c("Baku", "Tokyo", "Kathmandu", "Singapore"),
#'                                        newValues = c("Azerbaijan", "Japan", "Nepal" , "Singapore")))
#' oldDatabase
#' directory <- tempdir()
#' createRecodeDB(recodeListList = oldDatabase,
#'                directory = directory,
#'                DBname = "countries",
#'                overwrite = TRUE)
#' newRecodes <- data.frame( city = c("Berlin", "Paris", "Athens"),
#'                           country = c("Germany", "France", "Greece"))
#' # update the data base without overwriting old information
#' # (the row containing "Berlin - France" keeps it's old value)
#' updateRecodeDB(newRecodes = newRecodes,
#'                oldValues = "city",
#'                newValues = "country",
#'                directory = directory,
#'                DBname = "countries",
#'                ListName = "Europe",
#'                replace = FALSE)
#' getRecodeDB(directory, "countries")
#' # update the data base, overwriting old information
#' # (the row containing "Berlin - France" get's updated)
#' updateRecodeDB(newRecodes = newRecodes,
#'                oldValues = "city",
#'                newValues = "country",
#'                directory = directory,
#'                DBname = "countries",
#'                ListName = "Europe",
#'                replace = TRUE)
#' getRecodeDB(directory, "countries")
#' @export
updateRecodeDB <- function(newRecodes, oldValues = "oldValues", newValues = "newValues",
                           directory, newDirectory = directory,
                           DBname, newDBname = DBname, ListName,
                           fileType = "csv2", replace = FALSE) {
  # checks
  checkmate::assert_subset(c(oldValues, newValues), choices = colnames(newRecodes))

  if(!fileType %in% c("xlsx","csv","csv2")) {stop("FileType must be `csv2`, `csv`, or `xlsx`.")}

  recode_db <- getRecodeDB(directory, DBname, fileType)
  newRecodes <- prep_newRecodes(newRecodes, oldValues, newValues)

  old_recode_list <- recode_db[[ListName]]
  checkRecodeList(old_recode_list)
  checkRecodeList(newRecodes)

  oldValues_conflicts <- data.frame(oldValues = character(), newValues = character())
  if (replace) {
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
        ListName, "' will be overwritten:\n",
        paste(conflict_details, collapse = "\n")
      )
    }
  } else {
    newRecodes_manual <- unique(newRecodes[!newRecodes$oldValues %in% old_recode_list$oldValues, ])
  }

  ## if necessary, replace & order
  updated_recode_list <- rbind(old_recode_list[!old_recode_list$oldValues %in% oldValues_conflicts$oldValues, ], newRecodes_manual)
  updated_recode_list <- updated_recode_list[order(updated_recode_list$oldValues), ]
  recode_db[[ListName]] <- updated_recode_list

  createRecodeDB(recodeListList = recode_db, directory = newDirectory, DBname = newDBname, fileType = fileType, overwrite = TRUE)

  if(fileType == "xlsx"){ return(paste0("Successfully updated ", DBname, ".xlsx"))
  } else { return(paste0("Successfully updated ", DBname, ".csv")) }
}

prep_newRecodes <- function(newRecodes, oldValues, newValues){
  newRecodes <- rename_column(newRecodes, oldName = oldValues, newName = "oldValues")
  newRecodes <- rename_column(newRecodes, oldName = newValues, newName = "newValues")
  newRecodes <- newRecodes[, c("oldValues", "newValues")]
  return(newRecodes)
}
