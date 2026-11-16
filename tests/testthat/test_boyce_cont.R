test_that("boyce_cont returns NA with too few windows", {
  # predictions with only two values that barely separate presences from
  # background (reprex from issue #111): only 2 windows are left, which used to
  # give a perfect score of 1
  two_values <- data.frame(
    class = factor(rep(c("presence", "background"), each = 100),
      levels = c("presence", "background")
    ),
    .pred_presence = c(
      rep(c(0.4, 0.6), c(49, 51)), # presences
      rep(c(0.4, 0.6), c(50, 50)) # background
    )
  )
  expect_warning(
    res <- boyce_cont(two_values, class, .pred_presence),
    "min_windows"
  )
  expect_identical(res$.estimate, NA_real_)
  # with min_windows = 2, we get the old behaviour
  expect_equal(
    boyce_cont(two_values, class, .pred_presence, min_windows = 2)$.estimate,
    1
  )
  # same for the vector version
  expect_warning(
    res_vec <- boyce_cont_vec(two_values$class, two_values$.pred_presence),
    "min_windows"
  )
  expect_identical(res_vec, NA_real_)

  # three distinct values fill 13 overlapping windows, but they count as 3
  three_values <- data.frame(
    class = factor(rep(c("presence", "background"), each = 100),
      levels = c("presence", "background")
    ),
    .pred_presence = c(
      rep(c(0.4, 0.5, 0.6), c(30, 30, 40)), # presences
      rep(c(0.4, 0.5, 0.6), c(34, 33, 33)) # background
    )
  )
  expect_no_warning(
    res <- boyce_cont(three_values, class, .pred_presence)
  )
  expect_false(is.na(res$.estimate))
  expect_warning(
    res <- boyce_cont(three_values, class, .pred_presence, min_windows = 4),
    "only 3 distinct"
  )
  expect_identical(res$.estimate, NA_real_)

  # continuous predictions are not affected
  expect_identical(
    boyce_cont(two_class_example, truth, Class1),
    boyce_cont(two_class_example, truth, Class1, min_windows = 2)
  )
})

test_that("boyce_cont min_windows can be set in a metric set", {
  two_values <- data.frame(
    class = factor(rep(c("presence", "background"), each = 100),
      levels = c("presence", "background")
    ),
    .pred_presence = c(
      rep(c(0.4, 0.6), c(49, 51)), # presences
      rep(c(0.4, 0.6), c(50, 50)) # background
    )
  )
  boyce_cont_2 <- yardstick::metric_tweak("boyce_cont", boyce_cont,
    min_windows = 2
  )
  my_metrics <- yardstick::metric_set(boyce_cont_2, roc_auc)
  res <- my_metrics(two_values, class, .pred_presence)
  # the metric keeps its name, and uses the tweaked min_windows
  expect_identical(res$.metric, c("boyce_cont", "roc_auc"))
  expect_equal(res$.estimate[1], 1)
  # the default metric set gives NA
  expect_warning(
    res <- sdm_metric_set()(two_values, class, .pred_presence),
    "min_windows"
  )
  expect_identical(res$.estimate[1], NA_real_)
})

test_that("boyce_cont checks min_windows", {
  for (bad_value in list(1, 102, 2.5, c(3, 4), "a", NA)) {
    expect_error(
      boyce_cont(two_class_example, truth, Class1, min_windows = bad_value),
      "must be a single whole number"
    )
  }
})
