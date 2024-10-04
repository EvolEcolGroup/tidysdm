test_that("filter collinear variables with cor_caret", {
  lacerta_thin <- readRDS(system.file("extdata/lacerta_climate_sf.RDS",
    package = "tidysdm"
  ))
  set.seed(123)
  vars_to_keep <- filter_collinear(lacerta_thin)
  expect_true(all(c("bio02", "bio19", "altitude", "bio15", "bio08", "bio09", "bio03")
  %in% vars_to_keep))
  expect_true(all(is.numeric(filter_collinear(lacerta_thin, names = FALSE))))
  verbose_test <- suppressMessages(filter_collinear(lacerta_thin, verbose = TRUE))
  expect_error(
    filter_collinear(lacerta_thin, to_keep = "blah"),
    "to_keep includes variables that are not present in x"
  )
  # error if forced variables are too correlated
  expect_error(
    filter_collinear(lacerta_thin, to_keep = c("bio01", "bio06")),
    "some variables in"
  )
  # keep variables in if they are reasonable
  set.seed(123)
  vars_kept <- filter_collinear(lacerta_thin, to_keep = c("bio01", "bio09"))
  expect_true(all(c("bio01", "bio09") %in%
    vars_kept))
  vars_kept_cor <- cor(lacerta_thin[, vars_kept] %>% sf::st_drop_geometry())
  diag(vars_kept_cor) <- NA
  expect_true(max(abs(vars_kept_cor), na.rm = TRUE) < 0.7)
  # keep variables in if they are reasonable
  expect_true(all(c("bio01") %in%
    filter_collinear(lacerta_thin, to_keep = c("bio01"))))
  # one dimensional dataset
  lacerta_1var <- lacerta_thin[,1]
  expect_error(filter_collinear(lacerta_1var), "at least 2 numeric variables are needed")
  # error for defautl object
  expect_error(
    filter_collinear("blah"),
    "no method available for this object type"
  )

  # sample from data.frame
  set.seed(123)
  expect_true(!identical(filter_collinear(lacerta_thin, max_cells = 100), vars_to_keep))


  # test method on SpatRaster
  climate_present <- terra::readRDS(system.file("extdata/lacerta_climate_present_10m.rds",
                                      package = "tidysdm"
  ))
  cor_spatraster_ken <- filter_collinear(climate_present, cor_type = "kendall")
  cor_spatraster_ken_sub <- filter_collinear(climate_present, max_cells = 200, cor_type = "kendall")
  expect_true(!identical(cor_spatraster_ken, cor_spatraster_ken_sub))
})


test_that("filter collinear variables with vif_step", {
  lacerta_thin <- readRDS(system.file("extdata/lacerta_climate_sf.RDS",
                                      package = "tidysdm"
  ))
  set.seed(123)
  vars_to_keep <- filter_collinear(lacerta_thin, method="vif_step")
  # we should remove two variables
  expect_true(all(!c("bio01", "bio18") %in% vars_to_keep))
  # now keep them in
  expect_true(all(c("bio01", "bio18") %in%
                    filter_collinear(lacerta_thin, method="vif_step",
                                     to_keep = c("bio01", "bio18"))))
})

test_that("filter collinear variables with vif_cor", {
  lacerta_thin <- readRDS(system.file("extdata/lacerta_climate_sf.RDS",
                                      package = "tidysdm"
  ))
  set.seed(123)
  vars_to_keep <- filter_collinear(lacerta_thin, method="vif_cor")
  # we should remove two variables
  expect_true(all(!c("bio01", "bio18") %in% vars_to_keep))
  # now keep them in
  expect_true(all(c("bio01", "bio18") %in%
                    filter_collinear(lacerta_thin, method="vif_cor",
                                     to_keep = c("bio01", "bio18"))))
})
