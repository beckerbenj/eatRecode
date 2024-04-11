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
  d <- tempdir()
  updateRecodeDB(new_recodes1,
                 directory = test_path(),
                 newDirectory = d,
                 DBname = "helper_recodeDB",
                 ListName = "country",
                 fileType = "xlsx"
  )

  out <- getRecodeList(d, DBname = "helper_recodeDB", ListName = "country", fileType = "xlsx")
  expect_equal(dim(out), c(6, 2))
  expect_equal(out$oldValues[c(1, 4:5)], c("Bavaria", "Ingland", "Italia"))
  expect_equal(out$newValues[c(1, 4:5)], c("Germany", "England", "Italy"))

  updateRecodeDB(new_recodes2,
                 directory = test_path(),
                 newDirectory = d,
                 DBname = "helper_recodeDB",
                 ListName = "country",
                 fileType = "xlsx"
  )

  out <- getRecodeList(d, DBname = "helper_recodeDB", ListName = "country", fileType = "xlsx")
  expect_equal(dim(out), c(6, 2))
  expect_equal(out$oldValues[c(1, 4:5)], c("Bavaria", "Ingland", "Italia"))
  expect_equal(out$newValues[c(1, 4:5)], c("Germany", "England", "Italy"))
})

test_that("updating a recode list while overriding existing recodes", {
  d <- tempdir()
  updateRecodeDB(new_recodes2,
                 directory = test_path(),
                 newDirectory = d,
                 DBname = "helper_recodeDB",
                 ListName = "country",
                 fileType = "xlsx"
  )

  out <- getRecodeList(d, DBname = "helper_recodeDB", ListName = "country", fileType = "xlsx")
  expect_equal(out$oldValues[c(1, 4:5, 6)], c("Bavaria", "Ingland", "Italia", "Wales"))
  expect_equal(out$newValues[c(1, 4:5, 6)], c("Germany", "England", "Italy", "UK"))

  expect_message(
    updateRecodeDB(new_recodes2,
                   directory = test_path(),
                   newDirectory = d,
                   DBname = "helper_recodeDB",
                   ListName = "country",
                   fileType = "xlsx",
                   override = TRUE
    ),
    "The following recode pairs in the existing data base in sheet 'country' will be overwritten:\nWales -> UK; now: Wales\nBavaria -> Germany; now: Austria"
  )

  out <- getRecodeList(d, DBname = "helper_recodeDB", ListName = "country", fileType = "xlsx")
  expect_equal(out$oldValues[c(1, 4:5, 6)], c("Bavaria", "Ingland", "Italia", "Wales"))
  expect_equal(out$newValues[c(1, 4:5, 6)], c("Austria", "England", "Italy", "Wales"))
})

test_that("updating a recode list while overriding existing recodes with redundant recodes", {
  d <- tempdir()
  expect_message(
    updateRecodeDB(new_recodes3,
                   directory = test_path(),
                   newDirectory = d,
                   DBname = "helper_recodeDB",
                   ListName = "country",
                   fileType = "xlsx",
                   override = TRUE
    ),
    "The following recode pairs in the existing data base in sheet 'country' will be overwritten:\nBavaria -> Germany; now: Austria"
  )

  out <- getRecodeList(d, DBname = "helper_recodeDB", ListName = "country", fileType = "xlsx")
  expect_equal(out$oldValues[c(1, 4:5, 6)], c("Bavaria", "Ingland", "Italia", "Wales"))
  expect_equal(out$newValues[c(1, 4:5, 6)], c("Austria", "England", "Italy", "UK"))
})
