df <- data.frame(
  country = c("ANGOLA", "Anggola", "UND TUNESISCH", "England", "ENGGLAND"),
  country_r = c("Angola", NA, -98, "VereinigtesKoenigreich", NA)
)

test_that("errors", {
  expect_error(
    extractManualRecode(recodedList = 1)
  )
})


test_that("correct rows are selected", {
  out <- extractManualRecode(recodedList = df, varName = "country_r")

  expect_true(all(is.na(out$newValues)))
})
