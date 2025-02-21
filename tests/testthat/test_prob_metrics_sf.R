test_that("prob_metrics for sf objects", {
  # class predictions
  two_class_example_sf <- two_class_example
  two_class_example_sf$X <- sample(1:5, nrow(two_class_example_sf),
    replace = TRUE
  )
  two_class_example_sf$Y <- sample(1:5, nrow(two_class_example_sf),
    replace = TRUE
  )
  two_class_example_sf <- sf::st_as_sf(two_class_example_sf,
    coords = c("X", "Y")
  )

  expect_identical(
    average_precision(two_class_example, truth, Class1),
    average_precision(two_class_example_sf, truth, Class1)
  )
  expect_identical(
    brier_class(two_class_example, truth, Class1),
    brier_class(two_class_example_sf, truth, Class1)
  )
  expect_identical(
    classification_cost(two_class_example, truth, Class1),
    classification_cost(two_class_example_sf, truth, Class1)
  )
  expect_identical(
    gain_capture(two_class_example, truth, Class1),
    gain_capture(two_class_example_sf, truth, Class1)
  )
  expect_identical(
    mn_log_loss(two_class_example, truth, Class1),
    mn_log_loss(two_class_example_sf, truth, Class1)
  )
  expect_identical(
    pr_auc(two_class_example, truth, Class1),
    pr_auc(two_class_example_sf, truth, Class1)
  )
  expect_identical(
    roc_auc(two_class_example, truth, Class1),
    roc_auc(two_class_example_sf, truth, Class1)
  )
  ### multiclass metrics (not really relevant for SDMs)
  multi_class_example <- hpc_cv
  multi_class_example_sf <- multi_class_example
  multi_class_example_sf$X <- sample(1:5, nrow(multi_class_example_sf),
    replace = TRUE
  )
  multi_class_example_sf$Y <- sample(1:5, nrow(multi_class_example_sf),
    replace = TRUE
  )
  multi_class_example_sf <- sf::st_as_sf(multi_class_example_sf,
    coords = c("X", "Y")
  )
  expect_identical(
    roc_aunp(multi_class_example, obs, VF:L),
    roc_aunp(multi_class_example_sf, obs, VF:L)
  )
  expect_identical(
    roc_aunu(multi_class_example, obs, VF:L),
    roc_aunu(multi_class_example_sf, obs, VF:L)
  )
})
