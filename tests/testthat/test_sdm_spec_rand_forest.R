test_that("sdm_spec_rand_forest", {
  # error for invalid tuning method
  expect_error(sdm_spec_rand_forest(tune = "blah")) # automatic errof from rlang
  this_spec_rf <- sdm_spec_rand_forest()
  expect_true(inherits(this_spec_rf, "model_spec"))
  expect_true(inherits(this_spec_rf, "rand_forest"))
  # for the default, which is sdm, we only tune mtry
  expect_false(rlang::quo_is_null(this_spec_rf$args$mtry))
  expect_true(rlang::quo_is_null(this_spec_rf$args$trees))
  expect_true(rlang::quo_is_null(this_spec_rf$args$min))
  # if we now set to all, they should all be formulae for set to tune
  this_spec_rf <- sdm_spec_rand_forest(tune = "all")
  expect_false(rlang::quo_is_null(this_spec_rf$args$mtry))
  expect_false(rlang::quo_is_null(this_spec_rf$args$trees))
  expect_false(rlang::quo_is_null(this_spec_rf$args$min))
  # and if we set to none, they are all null
  this_spec_rf <- sdm_spec_rand_forest(tune = "none")
  expect_true(rlang::quo_is_null(this_spec_rf$args$mtry))
  expect_true(rlang::quo_is_null(this_spec_rf$args$trees))
  expect_true(rlang::quo_is_null(this_spec_rf$args$min))
  # the same for custom
  this_spec_rf <- sdm_spec_rand_forest(tune = "custom")
  expect_true(rlang::quo_is_null(this_spec_rf$args$mtry))
  expect_true(rlang::quo_is_null(this_spec_rf$args$trees))
  expect_true(rlang::quo_is_null(this_spec_rf$args$min))
  # check the short form
  this_spec_rf <- sdm_spec_rf()
  expect_true(inherits(this_spec_rf, "model_spec"))
  expect_true(inherits(this_spec_rf, "rand_forest"))
})
