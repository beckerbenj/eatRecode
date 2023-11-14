test_that("errors", {
  expect_error(
    checkRecodeList(1),
    "'recodeList' must be a data.frame."
  )
})
