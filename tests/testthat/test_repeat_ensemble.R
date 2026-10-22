skip_if_not_installed("earth")
test_that("repeat_ensemble constructor", {
  # an empty workflow
  test_rep_ens <- repeat_ensemble()
  # has all the slots
  expect_true(all(names(test_rep_ens) == c(
    "rep_id", "wflow_id",
    "workflow", "metrics"
  )))
  # but they are empty
  expect_true(nrow(test_rep_ens) == 0)
})

test_that("add_repeat to empty repeat ensemble", {
  # add a single workflow
  expect_warning(
    test_ens_1 <-
      simple_ensemble() %>% add_member(two_class_res[c(1:3), ])
  )
  test_rep_ens <- repeat_ensemble() %>% add_repeat(test_ens_1)
  expect_true(nrow(test_rep_ens) == 3)
  expect_true(inherits(test_rep_ens, "repeat_ensemble"))
  # now add another one (simply use another copy of the simple ens)
  test_rep_ens <- test_rep_ens %>% add_repeat(test_ens_1)
  expect_true(nrow(test_rep_ens) == 6)
  # now change the name of one of the models in the repeat being added
  test_ens_2 <- test_ens_1
  test_ens_2$wflow_id[1] <- "blah"
  expect_error(
    test_rep_ens %>% add_repeat(test_ens_2),
    "the models "
  )
  # now change the best model
  test_ens_2 <- test_ens_1
  attr(test_ens_2, "best_metric") <- "blah"
  expect_error(
    test_rep_ens %>% add_repeat(test_ens_2),
    "the best metric "
  )
  # and finally the metric
  test_ens_2 <- test_ens_1
  attr(test_ens_2, "metrics") <- "blah"
  expect_error(
    test_rep_ens %>% add_repeat(test_ens_2),
    "the metrics "
  )
  # also if only some of the metrics differ
  test_ens_2 <- test_ens_1
  attr(test_ens_2, "metrics") <- c(attr(test_ens_1, "metrics")[1], "blah")
  expect_error(
    test_rep_ens %>% add_repeat(test_ens_2),
    "the metrics "
  )
  # or if the repeat has an additional metric
  test_ens_2 <- test_ens_1
  attr(test_ens_2, "metrics") <- c(attr(test_ens_1, "metrics"), "blah")
  expect_error(
    test_rep_ens %>% add_repeat(test_ens_2),
    "the metrics "
  )
  # but the order of the metrics does not matter
  test_ens_2 <- test_ens_1
  attr(test_ens_2, "metrics") <- rev(attr(test_ens_1, "metrics"))
  expect_no_error(test_rep_ens %>% add_repeat(test_ens_2))
  # now make a list
  ens_list <- list(test_ens_1, test_ens_1, test_ens_1)
  test_rep_ens <- repeat_ensemble() %>% add_repeat(ens_list)
  expect_true(nrow(test_rep_ens) == 9)
  expect_true(setequal(unique(test_rep_ens$rep_id), c(
    "rep_01", "rep_02",
    "rep_03"
  )))
})

test_that("add_repeat drops existing class threshold calibration", {
  rep_1 <- get_repeat(lacerta_rep_ens, "rep_01")
  rep_2 <- get_repeat(lacerta_rep_ens, "rep_02")
  rep_3 <- get_repeat(lacerta_rep_ens, "rep_03")

  # adding to an uncalibrated ensemble does not warn
  expect_no_warning(
    test_rep_ens <- repeat_ensemble() %>% add_repeat(list(rep_1, rep_2))
  )
  test_rep_ens <- calib_class_thresh(test_rep_ens, class_thresh = "tss_max")
  expect_false(is.null(attr(test_rep_ens, "class_thresholds_list")))

  # adding a repeat removes the calibration
  expect_warning(
    test_rep_ens_3 <- test_rep_ens %>% add_repeat(rep_3),
    "removes the existing class threshold calibration"
  )
  expect_null(attr(test_rep_ens_3, "class_thresholds_list"))
  expect_true(setequal(
    unique(test_rep_ens_3$rep_id),
    c("rep_01", "rep_02", "rep_03")
  ))

  # the same applies if the repeat being added is itself calibrated
  rep_3_calib <- calib_class_thresh(rep_3, class_thresh = "tss_max")
  expect_warning(
    test_rep_ens_3 <- test_rep_ens %>% add_repeat(rep_3_calib),
    "removes the existing class threshold calibration"
  )
  expect_null(attr(test_rep_ens_3, "class_thresholds_list"))

  # a list of repeats only warns once
  expect_warning(
    test_rep_ens_4 <- test_rep_ens %>% add_repeat(list(rep_3, rep_3)),
    "removes the existing class threshold calibration"
  )
  expect_null(attr(test_rep_ens_4, "class_thresholds_list"))

  # calibrations of the simple ensembles being added are not carried over
  test_rep_ens_calib <- repeat_ensemble() %>%
    add_repeat(list(
      calib_class_thresh(rep_1, class_thresh = "tss_max"),
      rep_3_calib
    ))
  expect_null(attr(test_rep_ens_calib, "class_thresholds_list"))

  # a reused rep_id after filtering does not inherit a stale calibration
  calib_rep_ens <- calib_class_thresh(lacerta_rep_ens, class_thresh = "tss_max")
  calib_rep_ens <- dplyr::filter(calib_rep_ens, rep_id != "rep_03")
  expect_warning(
    calib_rep_ens <- calib_rep_ens %>% add_repeat(rep_2),
    "removes the existing class threshold calibration"
  )
  expect_true("rep_03" %in% calib_rep_ens$rep_id)
  expect_null(attr(calib_rep_ens, "class_thresholds_list"))
  expect_null(attr(get_repeat(calib_rep_ens, "rep_03"), "class_thresholds"))
})
