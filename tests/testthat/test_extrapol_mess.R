skip_if_not_installed("pastclim")

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
  mess_rast_single <- extrapol_mess(climate_future_single,
    training = lacerta_thin %>% select(bio15, class),
    .col = class
  )
  expect_true(inherits(mess_rast_single, "SpatRaster"))
  # same for df
  climate_future_single_df <- climate_future_df %>% select(bio15)
  mess_df_single <- extrapol_mess(climate_future_single_df,
    training = lacerta_thin %>% select(bio15, class),
    .col = class
  )
  expect_true(inherits(mess_df_single, "data.frame"))

  # mismatch the variables
  expect_error(
    extrapol_mess(
      climate_future,
      training = lacerta_thin %>% dplyr::select(-bio15),
      .col = class
    ), "`x` and `training` should contain"
  )

  # make training too short for spatraster
  expect_error(
    extrapol_mess(
      climate_future,
      training = lacerta_thin[1, ],
      .col = class
    ), "insufficient number of reference points"
  )
  # same for df
  expect_error(
    extrapol_mess(
      climate_future_df,
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
  # catch lack of units for time
  terra::time(climate_full) <- terra::time(climate_full)
  expect_error(
    extrapol_mess(climate_full, training = horses_env),
    "The time step of `x` is not `years`"
  )
  # catch lack of time
  time(climate_full) <- NULL
  expect_error(
    extrapol_mess(climate_full, training = horses_env),
    "The rasters in `SpatRasterDataset` "
  )
})

test_that("extrapol_mess matches training variables to layers by name", {
  x <- terra::rast(nrows = 2, ncols = 2, nlyrs = 2, vals = c(1:4, 101:104))
  names(x) <- c("a", "b")
  training <- data.frame(a = c(2, 3), b = c(102, 103))
  expected_mess <- c(-100, 100, 0, -100)
  expect_equal(
    as.vector(terra::values(extrapol_mess(x, training))),
    expected_mess
  )
  # same result if the training columns are in a different order
  expect_equal(
    as.vector(terra::values(extrapol_mess(x, training[, c("b", "a")]))),
    expected_mess
  )
  # and for the data.frame method
  x_df <- as.data.frame(terra::values(x))
  expect_equal(extrapol_mess(x_df, training)$mess, expected_mess)
  expect_equal(
    extrapol_mess(x_df, training[, c("b", "a")])$mess,
    expected_mess
  )
  # which also works with tibbles
  expect_equal(
    extrapol_mess(
      tibble::as_tibble(x_df),
      tibble::as_tibble(training[, c("b", "a")])
    )$mess,
    expected_mess
  )
})
