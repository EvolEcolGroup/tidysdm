skip_if_not_installed("earth")

test_that("simple_ensemble constructor", {
  # an empty workflow
  test_ens <- simple_ensemble()
  # has all the slots
  expect_true(all(names(test_ens) == c("wflow_id", "workflow", "metrics")))
  # but they are empty
  expect_true(nrow(test_ens) == 0)
})

test_that("add_member to empty simple ensemble", {
  # add a single workflow
  none_mars <- workflowsets::extract_workflow_set_result(two_class_res,
    id = "none_mars"
  )
  ## now add some models (the first 3) using default metric (which leads to a
  ## warning)
  expect_warning(test_ens <- simple_ensemble() %>% add_member(none_mars))
  expect_true(all(names(test_ens) == c("wflow_id", "workflow", "metrics")))
  expect_true(nrow(test_ens) == 1)
  # check that we fetched the name correctly from the object
  expect_true(test_ens$wflow_id == "none_mars")
  expect_true(attr(test_ens, "best_metric") == "roc_auc")
  expect_true(setequal(attr(test_ens, "metrics"), c("roc_auc", "accuracy")))
  # now add another model
  none_glm <- workflowsets::extract_workflow_set_result(two_class_res,
    id = "none_glm"
  )
  test_ens <- test_ens %>% add_member(none_glm)
  expect_true(nrow(test_ens) == 2)
  expect_true(all(test_ens$wflow_id == c("none_mars", "none_glm")))
  # fail to readd the same workflow (same name)
  expect_error(
    test_ens %>% add_member(none_glm),
    "x already has a member with the same name"
  )
  # error on using a different metric to choose the best model
  yj_trans_glm <- workflowsets::extract_workflow_set_result(two_class_res,
    id = "yj_trans_glm"
  )
  expect_error(
    test_ens %>% add_member(yj_trans_glm, metric = "accuracy"),
    "the requested metric is not the same as the one previously used in x"
  ) # nolint
  test_ens <- test_ens %>% add_member(yj_trans_glm, metric = "roc_auc")

  # check that autoplot successfully builds a plot
  p <- autoplot(test_ens)
  expect_true(inherits(p, "gg"))

  # add a workflow
  expect_warning(
    test_ens_set <-
      simple_ensemble() %>% add_member(two_class_res[c(3, 2, 5), ])
  )
  expect_true(nrow(test_ens_set) == 3)
  identical(test_ens$wflow_id, test_ens_set$wflow_id)

  # now create an ensemble not with the default metric
  test_ens <-
    simple_ensemble() %>% add_member(two_class_res[1:3, ], metric = "accuracy")
  expect_true(attr(test_ens, "best_metric") == "accuracy")
  p <- autoplot(test_ens)
  expect_true(inherits(p, "gg"))
  # give error if we request metric that does not exist
  expect_error(
    simple_ensemble() %>%
      add_member(two_class_res[1:3, ], metric = "boyce_cont")
  )
})
