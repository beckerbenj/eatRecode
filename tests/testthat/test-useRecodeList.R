rl <- readRDS(test_path("helper_recodeList.RDS"))
dat1 <- readRDS(test_path("helper_dat.RDS"))


test_that("errors", {
  expect_error(
    useRecodeList(df = 1, recodeList = rl),
    "'df' must be a data.frame."
  )
})

test_that("use recode list", {
  out <- useRecodeList(df = dat1, varName = "var1", new_varName = "var_new", recodeList = rl)
  expect_equal(names(out), c("var1", "var2", "var_new"))
  expect_equal(out[["var1"]], dat1[["var1"]])
  expect_equal(out[["var_new"]], c(NA, NA, "Germany", "UK", NA, NA))
})
