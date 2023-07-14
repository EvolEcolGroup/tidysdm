test_that("simple_ensemble predictions", {
  # class predictions
  ## now add some models (the first 3) using default metric
  test_ens <- simple_ensemble() %>% add_member(two_class_res[1:3, ], metric="roc_auc")
  # first we calibrate the model
  test_ens<-calib_class_thresh(test_ens,class_thresh = "tss_max")
  expect_true(nrow(attr(test_ens,"class_thresholds"))==4)
  expect_message(test_ens<-calib_class_thresh(test_ens,class_thresh = "tss_max"),
                 "this ensemble is already calibrated")
  # now add another calibration
  test_ens <- calib_class_thresh(test_ens, class_thresh=c("sens",0.9))
  expect_true(nrow(attr(test_ens,"class_thresholds"))==8)
})
