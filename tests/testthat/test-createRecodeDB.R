rl <- readRDS(test_path("helper_recodeList.RDS"))
rl2 <- readRDS(test_path("helper_recodeList2.RDS"))



test_that("create recode db", {
  d <- tempdir()
  input <- list(country = rl, language = rl2)
  createRecodeDB(input, directory = d, DBname = "input", fileType = "xlsx")

  out <- getRecodeDB(d)
  expect_equal(input, out)
})
