skip_if_not_installed("earth")
test_that("calibrate class thresholds for simple_ensemble", {
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

test_that("calibrate class thresholds for repeat_ensemble", {
  # check that we have no calibration info for the lacerta_rep_ens
  expect_null(attr(lacerta_rep_ens, "class_thresholds_list"))
  # extract the last simple ensemble for later
  test_ens <- get_repeat(lacerta_rep_ens, 3)
  # this should not have any class thresholds
  expect_null(attr(test_ens, "class_thresholds"))


  # first we calibrate the model
  lacerta_rep_ens <- calib_class_thresh(lacerta_rep_ens,
    class_thresh = c("sens", 0.9)
  )
  expect_true(length(attr(lacerta_rep_ens, "class_thresholds_list")) == 3)
  # the names of the three elements should be the same as the names of the three
  # repeats
  expect_true(
    all(
      names(
        attr(
          lacerta_rep_ens, "class_thresholds_list"
        )
      ) %in%
        unique(lacerta_rep_ens$rep_id)
    )
  )
  # extract the first repeat and check that its class thresholds are the same
  # as those from a simple_ensemble with the same modelslass
  extracted_ens <- get_repeat(lacerta_rep_ens, 3)
  # this should have a class thresholds attribute
  expect_true(!is.null(attr(extracted_ens, "class_thresholds", exact = TRUE)))
  # now check that the class thresholds are the same as those from a
  # simple_ensemble
  test_ens <- calib_class_thresh(test_ens, class_thresh = c("sens", 0.9))
  expect_equal(
    attr(extracted_ens, "class_thresholds", exact = TRUE),
    attr(test_ens, "class_thresholds", exact = TRUE)
  )
})
