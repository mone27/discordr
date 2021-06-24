test_that("paste url", {
  expect_equal(paste_url("rest", "api", "path"), "rest/api/path")
})
