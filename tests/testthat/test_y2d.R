test_that("y2d converts correctly", {
  expect_true(y2d(1) == 365)
  # we should have a number of leap years in a 1000 years
  expect_true(y2d(1000) > 365 * 1000)
})
