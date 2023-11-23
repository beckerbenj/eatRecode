test_that("input validation works", {
  dat_list <- list("a" = 1)
  dat_missing_column <- data.frame("oldValues" = c(1, 2))
  dat_no_error <- data.frame(
    "oldValues" = c("a", "b"),
    "newValues" = c("c", "d")
  )

  expect_error(checkRecode(dat_list), "'recodeList' must be a data.frame.")
  expect_error(checkRecode(dat_missing_column), "'recodeList' must contain the column 'newValues'.")
  expect_no_error(checkRecode(dat_no_error))
})


test_that("contradicting values are detected", {
  dat_no_duplicates <- data.frame(
    oldValues = c("Italia", "Wales"),
    newValues = c("Italy", "Wales")
  )

  dat_na <- data.frame(
    oldValues = c(NA, "Wales", "Italia", "Italia", "Wales", "Egyptian"),
    newValues = c("Italy", "Wales", "Italy", "Italien", "UK", "Egypt")
  )

  dat_duplicates <- data.frame(
    oldValues = c("Italia", "Wales", "Italia", "Italia", "Wales", "Egyptian"),
    newValues = c("Italy", "Wales", "Italy", "Italien", "UK", "Egypt")
  )

  expect_no_error(checkRecode(dat_no_duplicates))

  expect_error(checkRecode(dat_na), "Please check the NAs in 'oldValues'.")

  expect_error(
    checkRecode(dat_duplicates),
    "There are contradicting entrys in 'newValues': \n \n  oldValues newValues\n1    Italia     Italy\n4    Italia   Italien\n2     Wales     Wales\n5     Wales        UK"
  )
})
