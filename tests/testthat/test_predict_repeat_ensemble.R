test_that("repeat_ensemble predictions", {
  skip_if(FALSE)
  ## now add some models (the first 2) using default metric
  test_ens_2 <- simple_ensemble() %>% add_member(two_class_res[1:3, ], metric="roc_auc")
  ens_list <- list(test_ens_2, test_ens_2, test_ens_2)
  test_rep_ens <- repeat_ensemble() %>% add_repeat(ens_list)
  # default mean prediction across the 3 repeates and two models
  mean_pred<-predict(test_rep_ens, new_data = two_class_dat)
  # now test the median
  median_pred <- predict(test_rep_ens, new_data = two_class_dat, fun="median")
  # mean and median should differ!
  expect_false(identical(mean_pred, median_pred))
  # but the mean requested with a string should be the same as the default
  mean_pred_string<-predict(test_rep_ens, new_data = two_class_dat, fun="mean")
  expect_true(identical(mean_pred, mean_pred_string))

  # now extract individual member predictions
  mean_pred_members <- predict(test_rep_ens, new_data = two_class_dat, members=TRUE)
  # this should have 4 times the columns of a mean prediction (it has the mean as well as the three models)
  expect_true(ncol(mean_pred_members)==(ncol(mean_pred)*3*3+1))
  # now get predictions just for the individual members
  pred_members <- predict(test_rep_ens, new_data = two_class_dat, fun="none")
  # this should have 3 times the columns of a mean prediction (from the three member models)
  expect_true(ncol(pred_members)==(ncol(mean_pred)*3*3))

  # predict with threshold (should only contain 2 models)
  pred_thresh <- predict(test_rep_ens, new_data = two_class_dat, fun="none", metric_thresh = c("roc_auc",0.88))
  expect_true(ncol(pred_thresh)==(ncol(mean_pred)*2*3))
  pred_thresh_m <- predict(test_rep_ens, new_data = two_class_dat, fun="mean", metric_thresh = c("roc_auc",0.88))
  # check that the mean is indeed from the two models
  expect_true(all(apply(pred_thresh,1,mean)==pred_thresh_m[,1]))
  # and it is not the same as the mean from all models
  expect_false(identical(mean_pred, pred_thresh))

  # now filter so that we only have one model
  pred_thresh_1 <- predict(test_rep_ens, new_data = two_class_dat, fun="none", metric_thresh = c("accuracy",0.815))
  expect_true(ncol(pred_thresh_1)==(ncol(mean_pred)*3))
  pred_thresh_1m <- predict(test_rep_ens, new_data = two_class_dat, fun="mean", metric_thresh = c("accuracy",0.815))
  # and throw and error if we end up without any model!
  expect_error(predict(test_rep_ens, new_data = two_class_dat, fun="mean", metric_thresh = c("accuracy",0.83)),
               "the current metric_threshold excludes all models")
})
