test_that("out_of_range_warning catches extreme values",{
  time_steps <- seq(10,100,by=10)
  times <- c(5,20,30,50)
  # this should be fine
  expect_null(out_of_range_warning(times=times, time_steps=time_steps))
  # now get a warning
  times <- c(-5,20,30,50)
  expect_warning(out_of_range_warning(times=times, time_steps=time_steps),
                 "^Some dates are out of the range")
  times <- c(20,30,50,1000)
  expect_warning(out_of_range_warning(times=times, time_steps=time_steps),
                 "^Some dates are out of the range")
  
})
  