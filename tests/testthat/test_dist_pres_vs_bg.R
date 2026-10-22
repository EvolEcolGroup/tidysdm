skip_if_not_installed("overlapping")

test_that("NAs in dataframe", {
  # create dataframe with NAs
  test_dataset <- tibble(
    class = factor(c(
      "presence", "presence", "presence", "presence", "presence",
      "absence", "absence", "absence", "absence", "absence"
    )),
    cld6190_ann = c(76, 76, 57, 57, 57, 46, 74, 74, NA, 50),
    dtr6190_ann = c(104, 104, 114, 112, 113, 143, 93, 93, NA, 161),
    frs6190_ann = c(2, 2, 1, 3, 3, 112, 0, 0, NA, 133),
    h_dem = c(121, 121, 211, 363, 303, 1040, 134, 63, NA, 3624)
  )

  # compute differences between presence and background
  expect_error(
    test_dataset %>% dist_pres_vs_bg(class),
    "NAs in the dataframe"
  )
})

test_that("only numeric variables are used", {
  test_dataset <- tibble(
    class = factor(c(
      "presence", "presence", "presence", "presence", "presence",
      "absence", "absence", "absence", "absence", "absence"
    ), levels = c("presence", "absence")),
    cld6190_ann = c(76, 76, 57, 57, 57, 46, 74, 74, 70, 50),
    h_dem = c(121, 121, 211, 363, 303, 1040, 134, 63, 80, 3624),
    ecoreg = factor(c(1, 1, 2, 2, 2, 3, 3, 1, 2, 3)),
    id = letters[1:10]
  )
  dist_vec <- test_dataset %>% dist_pres_vs_bg(class)
  expect_true(setequal(names(dist_vec), c("cld6190_ann", "h_dem")))
  # also for plots
  p <- test_dataset %>% plot_pres_vs_bg(class)
  expect_true(inherits(p, "patchwork"))
  expect_length(p, 2)
})
