

#rl <- readRDS("tests/testthat/helper_recodeList.RDS")
#rl2 <- readRDS("tests/testthat/helper_recodeList2.RDS")
rl <- readRDS("helper_recodeList.RDS")
rl2 <- readRDS("helper_recodeList2.RDS")



test_that("get single recode list", {
  #out <- getRecodeList("tests/testthat/helper_recodeDB.xlsx", name = "country")
  out <- getRecodeList("helper_recodeDB.xlsx", name = "country")
  expect_equal(out, rl)

  out2 <- getRecodeList("helper_recodeDB.xlsx", name = "language")
  expect_equal(out2, rl2)
})


test_that("get recode list names", {
  #out <- namesRecodeDB("tests/testthat/helper_recodeDB.xlsx")
  out <- namesRecodeDB("helper_recodeDB.xlsx")
  expect_equal(out, c("country", "language"))
})
