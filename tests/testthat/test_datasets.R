test_that("datasets have the documented dimensions", {
  expect_identical(dim(lacerta), c(1268L, 3L))
  expect_identical(names(lacerta), c("ID", "latitude", "longitude"))
  expect_identical(dim(horses), c(788L, 3L))
  expect_identical(names(horses), c("latitude", "longitude", "time_bp"))
  expect_identical(dim(lacertidae_background), c(34677L, 3L))
  expect_identical(
    names(lacertidae_background),
    c("ID", "latitude", "longitude")
  )
})
