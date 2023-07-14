test_that("tss returns correct value", {
  # tss is really a wrapper around the j_index, so we expect them to be identical
  expect_identical(tss(two_class_example, truth, predicted)$.estimate,
  yardstick::j_index(two_class_example, truth, predicted)$.estimate)

})
