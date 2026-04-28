test_that("predict correctly a repeated ensemble", {
  # create a dataset to predict from the dataset used in the first simple
  # ensemble
  new_data_ex <- workflowsets::extract_mold(
    lacerta_rep_ens$workflow[[1]]
  )$predictors
  # now predict the individual repeats
  pred_reps <- predict(lacerta_rep_ens, new_data = new_data_ex, fun = "none")
  # this should have 6 columns (3 repeat and 2 algorithms)
  expect_true(ncol(pred_reps) == 6)
  # now output predictions by repeat for weighted mean
  pred_by_repeat_wmean <- predict(lacerta_rep_ens,
    new_data = new_data_ex,
    fun = "weighted_mean", by_repeat = TRUE
  )
  # this should have 3 columns (one for each repeat)
  expect_true(ncol(pred_by_repeat_wmean) == 3)
  # now aggregate over repeats with the weighted mean
  pred_wmean <- predict(lacerta_rep_ens,
    new_data = new_data_ex,
    fun = "weighted_mean"
  )
  # this should have 1 column (the weighted mean across
  # repeats and members)
  expect_true(ncol(pred_wmean) == 1)
  # compute the mean across the three repeats of the weighted mean by hand
  apply(pred_by_repeat_wmean, 1, mean) %>% expect_equal(pred_wmean[, 1])
  # now predict the normal mean and check that it differs from the weighted mean
  pred_mean <- predict(lacerta_rep_ens, new_data = new_data_ex, fun = "mean")
  expect_false(all(pred_mean == pred_wmean))
  # now predict for both the mean, the weighted mean, and the median
  pred_multi <- predict(lacerta_rep_ens,
    new_data = new_data_ex,
    fun = c("mean", "weighted_mean", "median")
  )
  # this should have 3 columns (one for each function)
  expect_true(ncol(pred_multi) == 3)
  # TODO check that median and weighted median differ from the mean and from
  # each other


  # TODO bring this test back when thresholding is implemented for repeated
  lacerta_rep_ens_calib <- calib_class_thresh(lacerta_rep_ens,
    class_thresh = c("tss_max")
  )

  # ensembles make prediction by class thresholding with a metric threshold
  pred_class_mean <- predict(lacerta_rep_ens_calib,
    new_data = new_data_ex,
    fun = "mean", type = "class", class_thresh = c("tss_max")
  )
  # this should have 1 column
  expect_true(ncol(pred_class_mean) == 1)
  # check with multiple fun
  pred_class_mean <- predict(lacerta_rep_ens_calib,
    new_data = new_data_ex,
    fun = c("mean", "weighted_mean", "median"),
    type = "class", class_thresh = c("tss_max")
  )


  # TODO we need to test when we give multiple fun (currently, that is not
  # implemented)


  # check for error if we use "none" with multiple functions
  expect_error(
    predict(
      lacerta_rep_ens,
      new_data = new_data_ex,
      fun = c("mean", "none")
    ), "if 'fun' has length >1, it cannot be 'none'"
  )
  
  # check that binary prediction works with metric_thresh set
  # and class_fun = "majority"
  lacerta_rep_ens_calib_thresh <- calib_class_thresh(
    lacerta_rep_ens,
    class_thresh = "tss_max",
    metric_thresh = c("boyce_cont", 0.5)
  )
  pred_class_metric_thresh <- predict(
    lacerta_rep_ens_calib_thresh,
    new_data = new_data_ex,
    fun = "median",
    type = "class",
    class_fun = "majority",
    class_thresh = "tss_max",
    metric_thresh = c("boyce_cont", 0.5)
  )
  # should have 1 column and be a factor with presence/background levels
  expect_true(ncol(pred_class_metric_thresh) == 1)
  expect_true(is.factor(pred_class_metric_thresh[[1]]))
})
