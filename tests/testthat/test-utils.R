test_that("input validation works", {
  dat_list <- list("a" = "b")
  dat_missing_column <- data.frame("oldValues" = c("a", "b"))
  dat_na <- data.frame("oldValues" = c(NA, "d"), "newValues" = c("a", "b"))
  dat_no_error <- data.frame(
    "oldValues" = c("a", "b"),
    "newValues" = c("c", "d"),
    "extraValues" = c(1, 2)
  )

  expect_error(checkRecode(dat_list), "Assertion on 'recodeList' failed: Must be of type 'data.frame', not 'list'.")
  expect_error(checkRecode(dat_missing_column))
  expect_error(checkRecode(dat_na))
  expect_no_error(checkRecode(dat_no_error))
})


test_that("contradicting values are detected", {
  dat_no_duplicates <- data.frame(
    oldValues = c("Italia", "Wales"),
    newValues = c("Italy", "Wales")
  )

  dat_duplicates <- data.frame(
    oldValues = c("Italia", "Wales", "Italia", "Italia", "Wales", "Egyptian"),
    newValues = c("Italy", "Wales", "Italy", "Italien", "UK", "Egypt")
  )

  expect_no_error(checkRecode(dat_no_duplicates))

  expect_error(
    checkRecode(dat_duplicates),
    "There are contradicting entrys in 'newValues': \n \n  oldValues newValues\n1    Italia     Italy\n4    Italia   Italien\n2     Wales     Wales\n5     Wales        UK"
  )
})
