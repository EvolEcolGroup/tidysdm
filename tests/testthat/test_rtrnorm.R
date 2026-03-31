test_that("output length matches input length", {
  mean  <- c(0, 1, 2)
  sd    <- c(1, 1, 1)
  lower <- c(-1, 0, 1)
  upper <- c(1, 2, 3)
  
  x <- rtrnorm_naive(mean, sd, lower, upper)
  
  expect_length(x, length(mean))
})

test_that("output values are within bounds", {
  mean  <- c(0, 1, 2)
  sd    <- c(1, 1, 1)
  lower <- c(-1, 0, 1)
  upper <- c(1, 2, 3)
  
  x <- rtrnorm_naive(mean, sd, lower, upper)
  
  expect_true(all(x >= lower))
  expect_true(all(x <= upper))
})


test_that("works for very narrow intervals", {
  set.seed(1)
  
  mean  <- rep(0, 10)
  sd    <- rep(1, 10)
  lower <- rep(0.1, 10)
  upper <- rep(0.1001, 10)
  
  x <- rtrnorm_naive(mean, sd, lower, upper)
  
  expect_true(all(x >= lower & x <= upper))
})


test_that("handles heterogeneous parameter vectors", {
  set.seed(42)
  
  mean  <- c(-2, 0, 3)
  sd    <- c(0.5, 1, 2)
  lower <- c(-3, -1, 2)
  upper <- c(-1,  1, 5)
  
  x <- rtrnorm_naive(mean, sd, lower, upper)
  
  expect_equal(length(x), 3)
  expect_true(all(x >= lower & x <= upper))
})

test_that("errors on unequal vector lengths", {
  mean  <- c(0, 1)
  sd    <- c(1, 1, 1)
  lower <- c(-1, 0)
  upper <- c(1, 2)
  
  expect_error(
    rtrnorm_naive(mean, sd, lower, upper),
    "same length"
  )
})

test_that("errors when sd <= 0", {
  mean  <- c(0, 1)
  sd    <- c(1, 0)
  lower <- c(-1, 0)
  upper <- c(1, 2)
  
  expect_error(
    rtrnorm_naive(mean, sd, lower, upper),
    "strictly positive"
  )
})

test_that("errors when lower > upper", {
  mean  <- c(0, 1)
  sd    <- c(1, 1)
  lower <- c(1, 2)
  upper <- c(0, 1)
  
  expect_error(
    rtrnorm_naive(mean, sd, lower, upper),
    "<="
  )
})

test_that("sample mean is within bounds and reasonable", {
  set.seed(123)
  
  n <- 1000
  mean  <- rep(0, n)
  sd    <- rep(1, n)
  lower <- rep(-1, n)
  upper <- rep(1, n)
  
  x <- rtrnorm_naive(mean, sd, lower, upper)
  
  # Should lie within bounds
  expect_true(mean(x) > -1 && mean(x) < 1)
  
  # Rough symmetry check
  expect_true(abs(mean(x)) < 0.1)
})

test_that("fully NA rows return NA", {
  mean  <- c(0, NA, 1)
  sd    <- c(1, NA, 1)
  lower <- c(-1, NA, 0)
  upper <- c(1, NA, 2)
  
  x <- rtrnorm_naive(mean, sd, lower, upper)
  
  expect_true(is.na(x[2]))
  expect_false(is.na(x[1]))
  expect_false(is.na(x[3]))
  
  x <- rtrnorm_naive(mean, sd, lower, upper)
  # valid rows respect bounds when mixed with NA rows
  valid <- !is.na(mean)
  
  expect_true(all(x[valid] >= lower[valid]))
  expect_true(all(x[valid] <= upper[valid]))
  expect_true(is.na(x[!valid]))
})
