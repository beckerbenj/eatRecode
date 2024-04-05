rl <- readRDS(test_path("helper_recodeList.RDS"))
rl2 <- readRDS(test_path("helper_recodeList2.RDS"))



test_that("get single recode list", {
  out <- getRecodeList(directory = here::here(),
                       DBname = "helper_recodeDB",
                       ListName = "country",
                       fileType = "xlsx")
  expect_equal(out, rl)

  out2 <- getRecodeList(directory = here::here(),
                        DBname = "helper_recodeDB",
                        ListName = "language",
                        fileType = "xlsx")
  expect_equal(out2, rl2)
})


test_that("get recode list names", {
  out <- namesRecodeDB(directory = here::here(),
                       DBname = "helper_recodeDB",
                       fileType = "xlsx")
  expect_equal(out, c("country", "language"))
})
