rl <- readRDS(test_path("helper_recodeList.RDS"))
rl2 <- readRDS(test_path("helper_recodeList2.RDS"))



test_that("create recode db", {
  f <- tempfile(fileext = ".xlsx")
  input <- list(country = rl, language = rl2)
  createRecodeDB(input, filePath = f)

  out <- getRecodeDB(f)
  expect_equal(input, out)
})
