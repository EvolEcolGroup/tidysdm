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

test_that("optim_thresh achieves the target sensitivity", {
  truth <- factor(c(rep("presence", 10), rep("absence", 10)),
    levels = c("presence", "absence")
  )
  estimate <- c(seq(0.55, 1, length.out = 10), seq(0, 0.45, length.out = 10))
  presences <- estimate[truth == "presence"]
  for (sens_target in c(0, 0.05, 0.1, 0.5, 0.85, 0.9, 0.95, 1)) {
    this_thresh <- optim_thresh(truth, estimate,
      metric = c("sensitivity", sens_target)
    )
    # the target sensitivity is achieved
    expect_gte(mean(presences >= this_thresh), sens_target)
    # and it would not be with any higher threshold (thresholds are rounded
    # to 8 digits)
    higher_thresh <- estimate[estimate > this_thresh + 1e-8]
    for (i_thresh in higher_thresh) {
      expect_lt(mean(presences >= i_thresh), sens_target)
    }
  }
  # with very low targets, the threshold is the highest presence
  expect_identical(
    optim_thresh(truth, estimate, metric = c("sens", 0.05)),
    max(presences)
  )
  expect_error(
    optim_thresh(truth, estimate, metric = c("sens", 1.5)),
    "should be a number between 0 and 1"
  )
})
