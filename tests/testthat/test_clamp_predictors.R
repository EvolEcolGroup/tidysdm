test_that("clamping_predictor works on SpatRasters", {
  # get current climate and subset to 3 variables
  climate_present <- terra::readRDS(system.file("extdata/lacerta_climate_present_10m.rds",
    package = "tidysdm"
  ))
  climate_present <- climate_present[[c("bio05", "bio13", "bio06", "bio15")]]
  lacerta_env <- bind_cols(terra::extract(climate_present, lacerta[, c(3, 2)], ID = FALSE))
  # now get future climate
  climate_future <- terra::readRDS(system.file("extdata/lacerta_climate_future_10m.rds",
    package = "tidysdm"
  ))
  climate_future <- climate_future[[names(climate_present)]]
  # and clamp it
  clamped_raster <- clamp_predictors(climate_future, lacerta_env)
  # check that before we had values beyond the extremes of training
  future_minmax <- terra::minmax(climate_future, compute = TRUE)
  clamped_minmax <- terra::minmax(clamped_raster, compute = TRUE)
  training_minmax <- apply(lacerta_env, 2, range, na.rm = TRUE)
  # expect that the original raster had more extreme values than the clamped version
  expect_true(all(future_minmax[1, ] <= clamped_minmax[1, ]))
  expect_true(all(future_minmax[2, ] >= clamped_minmax[2, ]))
  # and that it had some more extreme values than the training
  expect_true(any(future_minmax[1, ] <= training_minmax[1, ]))
  expect_true(any(future_minmax[2, ] >= training_minmax[2, ]))
  # but the training range is bigger or equal to the clamped raster
  expect_true(all(training_minmax[1, ] <= clamped_minmax[1, ]))
  expect_true(all(training_minmax[2, ] >= clamped_minmax[2, ]))
  # get error for missing variable
  climate_sub <- climate_present[[1:2]]
  expect_error(
    clamp_predictors(climate_sub, training = lacerta_env),
    "`x` is missing the following"
  )
  # error for missing class
  expect_error(
    clamp_predictors(lacerta_env, lacerta_env),
    "no method available for this object type"
  )
  # check that we can use the original training dataset as an sf object
  clamped_rast <- clamp_predictors(climate_future, lacerta_thin, .col = class)
  expect_true(inherits(clamped_rast, "SpatRaster"))
  # error if we fail to specify the class column
  expect_error(
    clamp_predictors(climate_future, lacerta_thin),
    "`x` is missing the following variables"
  )
})


test_that("clamping_predictor works on stars", {
  # a function to emulate terra::minmax for stars
  min_max <- function(x) {
    r <- sapply(x, range, na.rm = TRUE)
    rownames(r) <- c("min", "max")
    r
  }

  # get current climate and subset to 3 variables
  climate_present <- terra::readRDS(system.file("extdata/lacerta_climate_present_10m.rds",
    package = "tidysdm"
  ))
  climate_present <- climate_present[[c("bio05", "bio13", "bio06", "bio15")]]
  lacerta_env <- bind_cols(terra::extract(climate_present, lacerta[, c(3, 2)], ID = FALSE))
  # now get future climate
  climate_future <- terra::readRDS(system.file("extdata/lacerta_climate_future_10m.rds",
    package = "tidysdm"
  ))
  climate_future <- climate_future[[names(climate_present)]] %>%
    stars::st_as_stars(as_attributes = TRUE)
  # and clamp it
  clamped_raster <- clamp_predictors(climate_future, lacerta_env)
  # check that before we had values beyond the extremes of training
  future_minmax <- min_max(climate_future)
  clamped_minmax <- min_max(clamped_raster)
  training_minmax <- apply(lacerta_env, 2, range, na.rm = TRUE)
  # expect that the original raster had more extreme values than the clamped version
  expect_true(all(future_minmax[1, ] <= clamped_minmax[1, ]))
  expect_true(all(future_minmax[2, ] >= clamped_minmax[2, ]))
  # and that it had some more extreme values than the training
  expect_true(any(future_minmax[1, ] <= training_minmax[1, ]))
  expect_true(any(future_minmax[2, ] >= training_minmax[2, ]))
  # but the training range is bigger or equal to the clamped raster
  expect_true(all(training_minmax[1, ] <= clamped_minmax[1, ]))
  expect_true(all(training_minmax[2, ] >= clamped_minmax[2, ]))
  # get error for missing variable
  climate_sub <- climate_present[[1:2]] %>% stars::st_as_stars(as_attributes = TRUE)
  expect_error(
    clamp_predictors(climate_sub, training = lacerta_env),
    "`x` is missing the following"
  )
  # error for missing class
  expect_error(
    clamp_predictors(lacerta_env, lacerta_env),
    "no method available for this object type"
  )
  # check that we can use the original training dataset as an sf object
  clamped_rast <- clamp_predictors(climate_future, lacerta_thin, .col = class)
  expect_true(inherits(clamped_rast, "stars"))
  # error if we fail to specify the class column
  expect_error(
    clamp_predictors(climate_future, lacerta_thin),
    "`x` is missing the following variables"
  )
})

test_that("clamping_predictor works on SpatRasterDatasets", {
  suppressPackageStartupMessages(library(pastclim))
  set_data_path(on_CRAN = TRUE)
  suppressPackageStartupMessages(library(sf))
  climate_vars <- c("bio01", "bio10", "bio12")
  climate_full <- pastclim::region_series(
    bio_variables = climate_vars,
    data = "Example",
    crop = region_outline$Europe
  )
  # get climate
  horses_df <- location_slice_from_region_series(horses,
    region_series = climate_full,
    nn_interpol = FALSE
  )
  horses_env <- horses_df[, c("bio01", "bio10", "bio12")]
  clamped_sds <- clamp_predictors(climate_full, training = horses_env)
  climate_minmax <- lapply(climate_full, minmax)
  clumped_minmax <- lapply(clamped_sds, minmax)
  training_minmax <- apply(horses_env, 2, range, na.rm = TRUE)
  # expect the full climate to be broader than the clumped
  expect_true(all(climate_minmax[[1]][1, ] <= clumped_minmax[[1]][1, ]))
  expect_true(all(climate_minmax[[2]][2, ] >= clumped_minmax[[2]][2, ]))
  # expect the training set to be wider or equal
  expect_true(all(training_minmax[1, 1] <= clumped_minmax[[1]][1, ]))
  expect_true(all(training_minmax[2, 2] >= clumped_minmax[[2]][2, ]))
  # get error for missing variable
  climate_sub <- climate_full[[c("bio01", "bio10")]]
  expect_error(
    clamp_predictors(climate_sub, training = horses_env),
    "`x` is missing the following"
  )
  # now add a class column
  horses_env$class <- "presence"
  clamped_rast <- clamp_predictors(climate_full, horses_env, .col = class)
  expect_true(inherits(clamped_rast, "SpatRasterDataset"))
  # error if we fail to specify the class column
  expect_error(
    clamp_predictors(climate_full, horses_env),
    "`x` is missing the following variables"
  )
})
