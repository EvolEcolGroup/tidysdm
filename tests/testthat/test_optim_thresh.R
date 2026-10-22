test_that("optim_thresh returns thresholds for valid metrics", {
  for (this_metric in list("tss_max", "kap_max", c("sens", 0.9))) {
    this_thresh <- optim_thresh(two_class_example$truth,
      two_class_example$Class1,
      metric = this_metric
    )
    expect_true(is.numeric(this_thresh))
    expect_length(this_thresh, 1)
  }
})

test_that("optim_thresh gives an error for invalid metrics", {
  expect_error(
    optim_thresh(two_class_example$truth, two_class_example$Class1,
      metric = "blah"
    ),
    "invalid metric"
  )
  expect_error(
    optim_thresh(two_class_example$truth, two_class_example$Class1,
      metric = c("accuracy", 0.8)
    ),
    "invalid metric"
  )
  expect_error(
    optim_thresh(two_class_example$truth, two_class_example$Class1,
      metric = c("tss_max", "kap_max", "sens")
    ),
    "invalid metric"
  )
  expect_error(
    optim_thresh(as.character(two_class_example$truth),
      two_class_example$Class1,
      metric = "tss_max"
    ),
    "truth should be a factor"
  )
  expect_error(
    optim_thresh(two_class_example$truth, two_class_example$Class1,
      metric = "tss_max", event_level = "blah"
    ),
    "event_level should be either"
  )
})
