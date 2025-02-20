test_that("check_sdm_presence gives correct errors", {
  expect_true(two_class_dat %>% check_sdm_presence(Class,
                                                   presence_level = "Class1"))
  expect_error(
    two_class_dat %>% check_sdm_presence(Class, presence_level = "Class2"),
    "level Class2 is not the first"
  )
  expect_error(
    two_class_dat %>% check_sdm_presence(Class, presence_level = "Class3"),
    "level Class3 is not used in Class"
  )
  expect_error(
    two_class_dat %>% check_sdm_presence(blah, presence_level = "Class1"),
    "Can't extract"
  )
  expect_error(
    two_class_dat %>% check_sdm_presence(A, presence_level = "Class1"),
    "A should be"
  )
  # test when we use the column directly
  check_sdm_presence(two_class_dat$Class, presence_level = "Class1")
})
