new_recodes1 <- data.frame(
  oldValues = c("Italia", "Wales", "Ingland", "Bavaria", "Italia"),
  newValues = c("Italy", "Wales", "England", "Austria", "Italy"), stringsAsFactors = FALSE
)

new_recodes2 <- data.frame(
  oldValues = c("Italia", "Wales", "Ingland", "Bavaria", "Italia", "Italia", "Wales"),
  newValues = c("Italy", "Wales", "England", "Austria", "Italy", "Italy", "Wales"),
  stringsAsFactors = FALSE
)


test_that("updating a recode list", {
  f <- tempfile()
  updateRecodeDB(new_recodes1,
    recodeDBPath = test_path("helper_recodeDB.xlsx"),
    newRecodeDBPath = f,
    name = "country"
  )

  out <- getRecodeList(f, name = "country")
  expect_equal(dim(out), c(6, 2))
  expect_equal(out$oldValues[c(1, 4:5)], c("Bavaria", "Ingland", "Italia"))
  expect_equal(out$newValues[c(1, 4:5)], c("Germany", "England", "Italy"))

  updateRecodeDB(new_recodes2,
    recodeDBPath = test_path("helper_recodeDB.xlsx"),
    newRecodeDBPath = f,
    name = "country"
  )

  out <- getRecodeList(f, name = "country")
  expect_equal(dim(out), c(6, 2))
  expect_equal(out$oldValues[c(1, 4:5)], c("Bavaria", "Ingland", "Italia"))
  expect_equal(out$newValues[c(1, 4:5)], c("Germany", "England", "Italy"))
})

test_that("updating a recode list while overriding existing recodes", {
  f2 <- tempfile()
  expect_message(
    updateRecodeDB(new_recodes1,
      recodeDBPath = test_path("helper_recodeDB.xlsx"), newRecodeDBPath = f2,
      name = "country",
      override = TRUE
    ),
    "The recodes for the following oldValues in the existing data base in sheet 'country' will be overwritten: Wales, Bavaria"
  )

  out <- getRecodeList(f2, name = "country")

  expect_equal(out$oldValues[c(1, 4:5)], c("Bavaria", "Ingland", "Italia"))
  expect_equal(out$newValues[c(1, 4:5)], c("Austria", "England", "Italy"))
})
