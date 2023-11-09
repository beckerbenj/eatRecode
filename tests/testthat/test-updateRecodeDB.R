

new_recodes1 <- data.frame(oldValues = c("Italia", "Wales", "Ingland", "Bavaria", "Italia"),
                          newValues = c("Italy", NA, "England", NA, "Italy"), stringsAsFactors = FALSE)
#
# updating the recode db would change the underlying db; therefore the db must be copied as a temp file
f <- tempfile()
#file.copy(from = "tests/testthat/helper_recodeDB.xlsx", to = f)
file.copy(from = "helper_recodeDB.xlsx", to = f)


test_that("updating a recode list", {
  updateRecodeDB(new_recodes1, recodeDBPath = f,
                        name = "country")

  out <- getRecodeList(f, name = "country")

  expect_equal(out$oldValues[4:5], c("Ingland", "Italia"))
  expect_equal(out$newValues[4:5], c("England", "Italy"))
})

