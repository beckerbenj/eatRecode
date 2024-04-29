rl <- readRDS(test_path("helper_recodeList.RDS"))
rl2 <- readRDS(test_path("helper_recodeList2.RDS"))


test_that("create recode db - xlsx", {
  d <- tempdir()
  input <- list(country = rl, language = rl2)
  createRecodeDB(input, directory = d, DBname = "", fileType = "xlsx", overwrite = TRUE)
  out <- getRecodeDB(d, "", "xlsx")

  expect_equal(input, out)
})


test_that("create recode db - xlsx, test overwrite FALSE", {
  d <- tempdir()
  input <- list(country = rl, language = rl2)
  createRecodeDB(input, directory = d, DBname = "", fileType = "xlsx", overwrite = TRUE)

  expect_error(
    createRecodeDB(input, directory = d, DBname = "", fileType = "xlsx"), # overwrite = FALSE is default
    "There already is a database on your path. Please rename or move.")
})


test_that("create recode db - csv2", {
  d <- tempdir()
  input <- list(country = rl, language = rl2)
  createRecodeDB(input, directory = d, DBname = "", overwrite = TRUE) # csv2 is default
  out <- getRecodeDB(d, "")

  expect_equal(input, out)
})


test_that("create recode db - csv2, test overwrite FALSE", {
  d <- tempdir()
  input <- list(country = rl, language = rl2)
  createRecodeDB(input, directory = d, DBname = "", overwrite = TRUE)

  expect_error(
    createRecodeDB(input, directory = d, DBname = ""), # overwrite = FALSE is default
    "There already is a database on your path. Please rename or move.")
})


test_that("create recode db - csv", {
  d <- tempdir()
  input <- list(country = rl, language = rl2)
  createRecodeDB(input, directory = d, DBname = "", fileType = "csv", overwrite = TRUE)
  out <- getRecodeDB(d, "", "csv")

  expect_equal(input, out)
})


test_that("create recode db - csv, test overwrite FALSE", {
  d <- tempdir()
  input <- list(country = rl, language = rl2)
  createRecodeDB(input, directory = d, DBname = "", fileType = "csv", overwrite = TRUE)

  expect_error(
    createRecodeDB(input, directory = d, DBname = "", fileType = "csv"), # overwrite = FALSE is default
    "There already is a database on your path. Please rename or move.")
})
