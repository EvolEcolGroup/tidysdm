test_that("repeat_ensemble constructor", {
  # an empty workflow
  test_rep_ens <- repeat_ensemble()
  # has all the slots
  expect_true (all(names(test_rep_ens) == c("rep_id","wflow_id", "workflow", "metrics")))
  # but they are empty
  expect_true (nrow(test_rep_ens)==0)
})

test_that("add_repeat to empty repeat ensemble", {
  # add a single workflow
  expect_warning(test_ens_1 <-
    simple_ensemble() %>% add_member(two_class_res[c(1:3), ]))
  test_rep_ens <- repeat_ensemble() %>% add_repeat(test_ens_1)
  expect_true(nrow(test_rep_ens)==3)
  expect_true(inherits(test_rep_ens,"repeat_ensemble"))
  #now add another one (simply use another copy of the simple ens)
  test_rep_ens <- test_rep_ens %>% add_repeat(test_ens_1)
  expect_true(nrow(test_rep_ens)==6)
  # now change the name of one of the models in the repeat being added
  test_ens_2 <- test_ens_1
  test_ens_2$wflow_id[1]<-"blah"
  expect_error(test_rep_ens %>% add_repeat(test_ens_2),
               "the models ")
  # now change the best model
  test_ens_2 <- test_ens_1
  attr(test_ens_2, "best_metric") <- "blah"
  expect_error(test_rep_ens %>% add_repeat(test_ens_2),
               "the best metric ")
  # and finally the metric
  test_ens_2 <- test_ens_1
  attr(test_ens_2, "metrics") <- "blah"
  expect_error(test_rep_ens %>% add_repeat(test_ens_2),
               "the metrics ")
  # now make a list
  ens_list <- list(test_ens_1, test_ens_1, test_ens_1)
  test_rep_ens <- repeat_ensemble() %>% add_repeat(ens_list)
  expect_true(nrow(test_rep_ens)==9)
  expect_true(setequal(unique(test_rep_ens$rep_id),c("rep_01","rep_02","rep_03")))
})
