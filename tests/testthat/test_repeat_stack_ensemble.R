skip_if_not_installed("earth")
test_that("repeat_stack constructor", {
  # an empty workflow
  test_rep_stack <- repeat_stack()
  # has all the slots
  expect_true(all(names(test_rep_stack) == c(
    "rep_id", ".stack"
  )))
  # but they are empty
  expect_true(nrow(test_rep_stack) == 0)
})

test_that("add_repeat to empty repeat ensemble", {
  lacerta_models <- readRDS("./testdata/lacerta_models.rds")
  # add a single stack
  expect_warning(
    test_stack_1 <- stacks::stacks() %>%
      stacks::add_candidates(lacerta_models) %>%
      stacks::blend_predictions(
        metric = sdm_metric_set()
      ) %>%
      stacks:::fit_members()
  )

  test_rep_stack <- repeat_stack() %>% add_repeat(test_stack_1)
  expect_true(nrow(test_rep_stack) == 1)
  expect_true(inherits(test_rep_stack, "repeat_stack"))
  # now add another one (simply use another copy of the simple ens)
  test_rep_stack <- test_rep_stack %>% add_repeat(test_stack_1)
  expect_true(nrow(test_rep_stack) == 2)

  # now make a list
  ens_list <- list(test_stack_1, test_stack_1, test_stack_1)
  test_rep_stack <- repeat_stack() %>% add_repeat(ens_list)
  expect_true(nrow(test_rep_stack) == 3)
  expect_true(setequal(test_rep_stack$rep_id, c(
    "rep_01", "rep_02",
    "rep_03"
  )))
  # test errors for wrong object type
  expect_error(
    repeat_stack() %>% add_repeat("blah"),
    "no method available"
  )
  expect_error(
    "blah" %>% add_repeat(test_stack_1),
    "x must be a repeat_stack object"
  )
})
