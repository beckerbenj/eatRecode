df <- data.frame(oldValues = c("ANGOLA", "Anggola", "UND TUNESISCH", "England", "ENGGLAND"),
                 newValues = c("Angola", NA, -98, "VereinigtesKoenigreich", NA))

test_that("errors", {
  expect_error(extractManualRecode(recodedList = 1),
               "'df' must be a data.frame.")
})


test_that("correct rows are selected", {

  out <- extractManualRecode(recodedList = df)

  expect_true(all(is.na(out$newValues)))

})

