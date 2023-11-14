rl <- readRDS(test_path("helper_recodeList.RDS"))
rl2 <- readRDS(test_path("helper_recodeList2.RDS"))



test_that("get single recode list", {
  out <- getRecodeList(test_path("helper_recodeDB.xlsx"), name = "country")
  expect_equal(out, rl)

  out2 <- getRecodeList("helper_recodeDB.xlsx", name = "language")
  expect_equal(out2, rl2)
})


test_that("get recode list names", {
  out <- namesRecodeDB(test_path("helper_recodeDB.xlsx"))
  expect_equal(out, c("country", "language"))
})
