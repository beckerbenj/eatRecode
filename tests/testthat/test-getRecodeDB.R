rl <- readRDS(test_path("helper_recodeList.RDS"))
rl2 <- readRDS(test_path("helper_recodeList2.RDS"))



test_that("get single recode list: xlsx", {
  out <- getRecodeList(directory = test_path(),
                       DBname = "helper_recodeDB",
                       ListName = "country",
                       fileType = "xlsx")
  expect_equal(out, rl)

  out2 <- getRecodeList(directory = test_path(),
                        DBname = "helper_recodeDB",
                        ListName = "language",
                        fileType = "xlsx")
  expect_equal(out2, rl2)
})


test_that("get single recode list: csv", {
  out <- getRecodeList(directory = test_path(),
                       DBname = "helper_recodeDB_csv",
                       ListName = "country",
                       fileType = "csv")
  expect_equal(out, rl)

  out2 <- getRecodeList(directory = test_path(),
                        DBname = "helper_recodeDB_csv",
                        ListName = "language",
                        fileType = "csv")
  expect_equal(out2, rl2)
})


test_that("get single recode list: csv2", {
  out <- getRecodeList(directory = test_path(),
                       DBname = "helper_recodeDB_csv2",
                       ListName = "country") # csv2 is default for fileType
  expect_equal(out, rl)

  out2 <- getRecodeList(directory = test_path(),
                        DBname = "helper_recodeDB_csv2",
                        ListName = "language") # csv2 default for fileType
  expect_equal(out2, rl2)
})



test_that("get recode list names: xlsx", {
  out <- namesRecodeDB(directory = test_path(),
                       DBname = "helper_recodeDB",
                       fileType = "xlsx")
  expect_equal(out, c("country", "language"))
})


test_that("get recode list names: csv", {
  out <- namesRecodeDB(directory = test_path(),
                       DBname = "helper_recodeDB_csv",
                       fileType = "csv")
  expect_equal(out, c("country", "language"))
})


test_that("get recode list names: csv2", {
  out <- namesRecodeDB(directory = test_path(),
                       DBname = "helper_recodeDB_csv2")
  expect_equal(out, c("country", "language")) # csv2 default for fileType
})
