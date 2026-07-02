test_that("sample_time_uncertainty() works correctly",{
  test_data <- tibble::tibble(
    id = 1:6,
    mean_time = c(NA, NA, NA, -2000, -3000, NA),
    sd_time = c(NA, NA, NA, 100, 200, NA),
    oldest_time = c(-210, -320, -500, NA, NA, NA),
    youngest_time = c(-190, -280, -400, NA, NA, NA),
    fixed_time = c(NA, NA, NA, NA, NA, 70)
  )

  sampled_times <- sample_time_uncertainty(test_data,
                                           sd_time_units = "years",
                                           lubridate_fun = pastclim::ybp2date)

  expect_equal(length(sampled_times), nrow(test_data))
  expect_true(all(sampled_times[1:3] %in% as.Date(c("2020-01-01", "2021-01-01"))))
  expect_true(all(sampled_times[4] >= as.Date("1900-01-01") & sampled_times[4] <= as.Date("2100-12-31")))
  expect_true(all(sampled_times[5] >= as.Date("2800-01-01") & sampled_times[5] <= as.Date("3200-12-31")))
})

test_that("sample_time_uncertainty() works on complex stratigraphy",{
  test_data <- read.csv(
    file.path(test_path("testdata"), "times_complex_strat.csv"))
  
})
