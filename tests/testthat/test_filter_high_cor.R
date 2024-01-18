test_that("filter correlated variables", {
  lacerta_thin <- readRDS(system.file("extdata/lacerta_climate_sf.RDS",
    package = "tidysdm"
  ))
  set.seed(123)
  to_keep <- filter_high_cor(lacerta_thin)
  expect_true(all(c("bio02", "bio19", "altitude", "bio15", "bio08", "bio09", "bio03")
  %in% to_keep))
  expect_true(all(is.numeric(filter_high_cor(lacerta_thin, names = FALSE))))
  verbose_test <- suppressMessages(filter_high_cor(lacerta_thin, verbose = TRUE))
  expect_error(
    filter_high_cor(lacerta_thin, to_keep = "blah"),
    "to_keep should only include numeric"
  )
  # error if forced variables are too correlated
  expect_error(
    filter_high_cor(lacerta_thin, to_keep = c("bio01", "bio06")),
    "some variables in"
  )
  # keep variables in if they are reasonable
  set.seed(123)
  vars_kept <- filter_high_cor(lacerta_thin, to_keep = c("bio01", "bio09"))
  expect_true(all(c("bio01", "bio09") %in%
    vars_kept))
  vars_kept_cor <- cor(lacerta_thin[, vars_kept] %>% sf::st_drop_geometry())
  diag(vars_kept_cor) <- NA
  expect_true(max(abs(vars_kept_cor), na.rm = TRUE) < 0.7)
  # keep variables in if they are reasonable
  expect_true(all(c("bio01") %in%
    filter_high_cor(lacerta_thin, to_keep = c("bio01"))))
  # make sure the matrix is correct
  foo <- matrix(c(1, 0.1, 0.3, 1), nrow = 2, dimnames = list(c("a", "b"), c("a", "b")))
  expect_error(filter_high_cor(foo), "correlation matrix is not symmetric")
  # one dimensional matrix
  foo <- matrix(c(1), nrow = 1, dimnames = list(c("a"), c("a")))
  expect_error(filter_high_cor(foo), "only one variable given")
  # error for defautl object
  expect_error(
    filter_high_cor("blah"),
    "no method available for this object type"
  )
  
  # test method on SpatRaster
  climate_present <- terra::rast(system.file("extdata/lacerta_climate_present_10m.nc",
                                      package = "tidysdm"
  ))
  cor_spatraster <- filter_high_cor(climate_present)
})
