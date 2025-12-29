skip_if_not_installed("earth")
test_that("calibrate class thresholds", {
  # class predictions
  ## now add some models (the first 3) using default metric
  test_ens <- simple_ensemble() %>% add_member(two_class_res[1:3, ],
    metric = "roc_auc"
  )
  # first we calibrate the model
  test_ens <- calib_class_thresh(test_ens, class_thresh = "tss_max")
  expect_true(nrow(attr(test_ens, "class_thresholds")) == 4)
  expect_message(
    test_ens <- calib_class_thresh(test_ens, class_thresh = "tss_max"),
    "this ensemble is already calibrated"
  )
  # now add another calibration
  test_ens <- calib_class_thresh(test_ens, class_thresh = c("sens", 0.9))
  expect_true(nrow(attr(test_ens, "class_thresholds")) == 8)

  # test collection of thresholds
  class_thresh_df <- collect_class_thresh(test_ens)
  expect_true(nrow(class_thresh_df) == 8)
  # check that we get an error if we try to collect from a non-ensemble object
  expect_error(
    collect_class_thresh(two_class_res),
    "no method available for this object type:"
  )
})
