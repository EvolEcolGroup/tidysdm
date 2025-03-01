test_that("mess_predictor works on SpatRasters and df", {
  # now get future climate
  climate_future <- terra::readRDS(
    system.file("extdata/lacerta_climate_future_10m.rds",
      package = "tidysdm"
    )
  )
  mess_rast <- extrapol_mess(climate_future,
    training = lacerta_thin,
    .col = class
  )
  expect_true(inherits(mess_rast, "SpatRaster"))

  # now check that we can feed in a data.frame
  climate_future_df <- climate_future %>% terra::as.data.frame()
  mess_df <- extrapol_mess(climate_future_df,
    training = lacerta_thin,
    .col = class
  )
  expect_true(inherits(mess_df, "data.frame"))
  # we should get the same result
  expect_true(all.equal(mess_df, terra::as.data.frame(mess_rast),
    check.attributes = FALSE
  ))

  # test a single layer
  climate_future_single <- climate_future[[1]]
  mess_rast_single <- extrapol_mess(climate_future,
    training = lacerta_thin,
    .col = class
  )
  expect_true(inherits(mess_rast_single, "SpatRaster"))

  # mismatch the variables
  expect_error(
    extrapol_mess(
      climate_future,
      training = lacerta_thin %>% dplyr::select(-bio15),
      .col = class
    ), "`x` and `training` should contain"
  )

  # make training too short
  expect_error(
    extrapol_mess(
      climate_future,
      training = lacerta_thin[1, ],
      .col = class
    ), "insufficient number of reference points"
  )
})

test_that("mess_predictor works on stars", {
  climate_future <- terra::readRDS(
    system.file("extdata/lacerta_climate_future_10m.rds",
      package = "tidysdm"
    )
  ) %>%
    stars::st_as_stars(as_attributes = TRUE)
  mess_rast <- extrapol_mess(climate_future,
    training = lacerta_thin, .col = class
  )
  expect_true(inherits(mess_rast, "stars"))
})

test_that("mess_predictor works on SpatRasterDatasets", {
  library(pastclim)
  set_data_path(on_CRAN = TRUE)
  library(sf)
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
  # Just use the plain data.frame of env
  horses_env <- horses_df[, c("bio01", "bio10", "bio12")]
  mess_rast <- extrapol_mess(climate_full, training = horses_env)
  expect_true(inherits(mess_rast, "SpatRaster"))
  expect_true(terra::timeInfo(mess_rast)$time)
  # We expect recent time steps to have a higher MESS than older time steps
  expect_true(
    unlist(
      global(mess_rast[[1]], mean, na.rm = TRUE)
    ) <
      unlist(global(mess_rast[[5]], mean, na.rm = TRUE))
  )
})
