skip_if_not_installed("earth")
data.table::setDTthreads(2)
test_that("simple_ensemble predictions", {
  ## now add some models (the first 3) using default metric
  test_ens <- simple_ensemble() %>% add_member(two_class_res[1:3, ],
    metric = "roc_auc"
  )
  # default mean prediction across the 3 models
  mean_pred <- predict(test_ens, new_data = two_class_dat)
  # now test the median
  median_pred <- predict(test_ens, new_data = two_class_dat, fun = "median")
  # mean and median should differ!
  expect_false(identical(mean_pred, median_pred))
  # but the mean requested with a string should be the same as the default
  mean_pred_string <- predict(test_ens, new_data = two_class_dat, fun = "mean")
  expect_true(identical(mean_pred, mean_pred_string))
  # now extract individual member predictions
  mean_pred_members <- predict(test_ens,
    new_data = two_class_dat,
    members = TRUE
  )
  # this should have 4 times the columns of a mean prediction (it has the mean
  # as well as the three models)
  expect_true(ncol(mean_pred_members) == (ncol(mean_pred) * 4))
  # now get predictions just for the individual members
  pred_members <- predict(test_ens, new_data = two_class_dat, fun = "none")
  # this should have 3 times the columns of a mean prediction (from the three
  # member models)
  expect_true(ncol(pred_members) == (ncol(mean_pred) * 3))
  # now create weighted predictions
  weighted_preds <- predict(test_ens,
    new_data = two_class_dat,
    fun = c("mean", "median", "weighted_mean", "weighted_median")
  )
  # check that we have 4 columns
  expect_true(ncol(weighted_preds) == (ncol(mean_pred) * 4))
  # predict with threshold (should only contain 2 models)
  pred_thresh <- predict(test_ens,
    new_data = two_class_dat,
    fun = "none", metric_thresh = c("roc_auc", 0.88)
  )
  expect_true(ncol(pred_thresh) == (ncol(mean_pred) * 2))
  pred_thresh_m <- predict(test_ens,
    new_data = two_class_dat,
    fun = "mean", metric_thresh = c("roc_auc", 0.88)
  )
  # check that the mean is indeed from the two models
  expect_true(all(apply(pred_thresh, 1, mean) == pred_thresh_m))
  # and it is not the same as the mean from all models
  expect_false(identical(mean_pred, pred_thresh))

  # now filter so that we only have one model
  pred_thresh_1 <- predict(test_ens,
    new_data = two_class_dat,
    fun = "none", metric_thresh = c("accuracy", 0.815)
  )
  expect_true(ncol(pred_thresh_1) == (ncol(mean_pred)))
  pred_thresh_1m <- predict(test_ens,
    new_data = two_class_dat,
    fun = "mean", metric_thresh = c("accuracy", 0.815)
  )
  expect_true(ncol(pred_thresh_1m) == (ncol(mean_pred)))
  # and throw and error if we end up without any model!
  expect_error(
    predict(test_ens,
      new_data = two_class_dat, fun = "mean",
      metric_thresh = c("accuracy", 0.83)
    ),
    "the current metric_threshold excludes all models"
  )

  # class predictions
  ## now add some models (the first 3) using default metric
  test_ens <- simple_ensemble() %>% add_member(two_class_res[1:3, ],
    metric = "roc_auc"
  )
  # expect error if we haven't calibrated the model
  expect_error(predict(test_ens,
    new_data = two_class_dat, type = "class", class_thresh = c("tss_max"),
    fun = c("mean", "median"),
    "this model needs to be first calibrated"
  ))
  # fix this by first calibrating the model
  test_ens <- calib_class_thresh(test_ens, class_thresh = "tss_max")
  pred_class <- predict(test_ens,
    new_data = two_class_dat, type = "class", class_thresh = c("tss_max"),
    fun = c("mean", "median")
  )
  expect_true(inherits(pred_class[, 1], "factor"))
  pred_class <- predict(test_ens,
    new_data = two_class_dat, type = "class", class_thresh = 0.4,
    fun = c("mean", "median")
  )

  # expect error if we give invalid function
  expect_error(
    predict(test_ens,
      new_data = two_class_dat, type = "class", fun = "blah",
      class_thresh = 0.4,
    ),
    "fun should be either "
  )
  # expect error if we give invalid function
  expect_error(
    predict(test_ens,
      new_data = two_class_dat, type = "class", fun = c(1, 2, 3),
      class_thresh = 0.4,
    ),
    "fun should be either "
  )
})


test_that("simple_ensemble prediction errors", {
  ## now add some models (the first 3) using default metric
  test_ens <- simple_ensemble() %>% add_member(two_class_res[1:3, ],
    metric = "roc_auc"
  )
  # default mean prediction across the 3 models
  expect_error(
    predict(test_ens, new_data = two_class_dat, type = "blah"),
    "'type' can only take"
  )
  expect_error(
    predict(test_ens, new_data = two_class_dat, type = "class", fun = "none"),
    "classes can be generated only if"
  )
  # expect no error if new_data is null
  expect_no_error(predict(test_ens, new_data = NULL))
})

test_that("weighted_median_vec computes the weighted median", {
  # equal weights give the same result as the median
  expect_identical(weighted_median_vec(c(3, 1, 2), rep(1, 3)), 2)
  expect_identical(weighted_median_vec(c(4, 1, 3, 2), rep(0.7, 4)), 2.5)
  expect_identical(weighted_median_vec(c(0.2, 0.6), c(0.8, 0.8)), 0.4)
  # a heavy weight pulls the median towards its value
  expect_identical(weighted_median_vec(c(1, 2, 3), c(1, 1, 5)), 3)
  expect_identical(weighted_median_vec(c(1, 2, 3), c(5, 1, 1)), 1)
  # ties in the cumulative weights give a single value
  expect_identical(weighted_median_vec(c(1, 2, 3), c(1, 2, 1)), 2)
  expect_length(weighted_median_vec(c(1, 2, 3, 4), c(1, 1, 1, 1)), 1)
})

test_that("predict with weighted_median returns one value per location", {
  test_ens <- simple_ensemble() %>%
    add_member(two_class_res[1:3, ], metric = "roc_auc")
  # set the metrics so that cumulative weights are tied
  test_ens$metrics <- mapply(function(x, w) {
    x$mean[x$.metric == "roc_auc"] <- w
    x
  }, test_ens$metrics, c(0.25, 0.5, 0.25), SIMPLIFY = FALSE)
  pred <- predict(test_ens, fun = c("median", "weighted_median"))
  expect_identical(names(pred), c("median", "weighted_median"))
  expect_true(all(is.finite(pred$weighted_median)))
  # with equal weights, the weighted median is the median
  test_ens$metrics <- lapply(test_ens$metrics, function(x) {
    x$mean[x$.metric == "roc_auc"] <- 0.8
    x
  })
  pred <- predict(test_ens[1:2, ], fun = c("median", "weighted_median"))
  expect_equal(pred$weighted_median, pred$median)
})
