test_that("check_split_balance correctly summarises splits", {
  lacerta_thin <- readRDS(system.file("extdata/lacerta_thin_all_vars.rds",
    package = "tidysdm"
  ))
  set.seed(1005)
  lacerta_initial <- spatial_initial_split(lacerta_thin,
    prop = 1 / 5, spatial_block_cv,
    cellsize = grid_cellsize(lacerta_thin),
    offset = grid_offset(lacerta_thin) + 0.001
  )
  initial_table <- check_splits_balance(lacerta_initial, class)
  expect_true(all(dim(initial_table) == c(1, 4)))
  expect_true(names(initial_table[1]) == "presence_test")
  # check error if we give non existing column
  expect_error(
    check_splits_balance(lacerta_initial, blah),
    ".col should be a column in the data used to generate the splits"
  )

  set.seed(1005)
  lacerta_training <- training(lacerta_initial)
  lacerta_cv <- spatial_block_cv(lacerta_training,
    v = 5,
    cellsize = grid_cellsize(lacerta_thin),
    offset = grid_offset(lacerta_thin) + 0.001
  )

  cv_table <- check_splits_balance(lacerta_cv, class)
  expect_true(all(dim(cv_table) == c(5, 4)))
  expect_true(names(cv_table[1]) == "presence_assessment")
  # check error if we give non existing column
  expect_error(
    check_splits_balance(lacerta_cv, blah),
    ".col should be a column in the data used to generate the splits"
  )
})
