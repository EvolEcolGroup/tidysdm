test_that("simple_ensemble predictions", {
  data.table::setDTthreads(2)
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
  # first we calibrate the model
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
