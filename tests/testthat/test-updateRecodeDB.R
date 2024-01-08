new_recodes1 <- data.frame(
  oldValues = c("Italia", "Ingland", "Italia"),
  newValues = c("Italy", "England", "Italy"), stringsAsFactors = FALSE
)

new_recodes2 <- data.frame(
  oldValues = c("Italia", "Wales", "Ingland", "Bavaria", "Italia", "Italia", "Wales"),
  newValues = c("Italy", "Wales", "England", "Austria", "Italy", "Italy", "Wales"),
  stringsAsFactors = FALSE
)

new_recodes3 <- data.frame(
  oldValues = c("Italia", "Wales", "Ingland", "Bavaria", "Italia"),
  newValues = c("Italy", "UK", "England", "Austria", "Italy"), stringsAsFactors = FALSE
)

test_that("updating a recode list", {
  f <- tempfile()
  updateRecodeDB(new_recodes1,
    oldValues = "oldValues",
    recodeDBPath = test_path("helper_recodeDB.xlsx"),
    newRecodeDBPath = f,
    name = "country"
  )

  out <- getRecodeList(f, name = "country")
  expect_equal(dim(out), c(6, 2))
  expect_equal(out$oldValues[c(1, 4:5)], c("Bavaria", "Ingland", "Italia"))
  expect_equal(out$newValues[c(1, 4:5)], c("Germany", "England", "Italy"))

  updateRecodeDB(new_recodes2,
    oldValues = "oldValues",
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
  f2a <- tempfile()
  updateRecodeDB(new_recodes1,
    oldValues = "oldValues",
    recodeDBPath = test_path("helper_recodeDB.xlsx"), newRecodeDBPath = f2a,
    name = "country",
    override = TRUE
  )

  out <- getRecodeList(f2a, name = "country")
  expect_equal(out$oldValues[c(1, 4:5, 6)], c("Bavaria", "Ingland", "Italia", "Wales"))
  expect_equal(out$newValues[c(1, 4:5, 6)], c("Germany", "England", "Italy", "UK"))

  f2 <- tempfile()
  expect_message(
    updateRecodeDB(new_recodes2,
      oldValues = "oldValues",
      recodeDBPath = test_path("helper_recodeDB.xlsx"), newRecodeDBPath = f2,
      name = "country",
      override = TRUE
    ),
    "The following recode pairs in the existing data base in sheet 'country' will be overwritten:\nWales -> UK; now: Wales\nBavaria -> Germany; now: Austria"
  )

  out2 <- getRecodeList(f2, name = "country")
  expect_equal(out2$oldValues[c(1, 4:5, 6)], c("Bavaria", "Ingland", "Italia", "Wales"))
  expect_equal(out2$newValues[c(1, 4:5, 6)], c("Austria", "England", "Italy", "Wales"))
})

test_that("updating a recode list while overriding existing recodes with redundant recodes", {
  f3 <- tempfile()
  expect_message(
    updateRecodeDB(new_recodes3,
      oldValues = "oldValues",
      recodeDBPath = test_path("helper_recodeDB.xlsx"), newRecodeDBPath = f3,
      name = "country",
      override = TRUE
    ),
    "The following recode pairs in the existing data base in sheet 'country' will be overwritten:\nBavaria -> Germany; now: Austria"
  )

  out <- getRecodeList(f3, name = "country")
  expect_equal(out$oldValues[c(1, 4:5, 6)], c("Bavaria", "Ingland", "Italia", "Wales"))
  expect_equal(out$newValues[c(1, 4:5, 6)], c("Austria", "England", "Italy", "UK"))
})
