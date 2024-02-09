test_that("clamping_predictor works on SpatRasters",{
  # get current climate and subset to 3 variables
  climate_present <- terra::rast(system.file("extdata/lacerta_climate_present_10m.nc",
                                package = "tidysdm"
  ))
  climate_present <- climate_present[[1:3]]
  lacerta_env <- bind_cols(terra::extract(climate_present, lacerta[,c(3,2)], ID = FALSE))
  # now get future climate
  climate_future <- terra::rast(system.file("extdata/lacerta_climate_future_10m.nc",
                                     package = "tidysdm"
  ))
  climate_future <- climate_future [[names(climate_present)]]
  # and clamp it
  clamped_raster <- clamp_predictors(climate_future, lacerta_env)
  # check that before we had values beyond the extremes of training
  future_minmax <- terra::minmax(climate_future, compute=TRUE)
  clamped_minmax <- terra::minmax(clamped_raster, compute=TRUE)
  training_minmax <- apply(lacerta_env, 2, range, na.rm=TRUE)
  # expect that the original raster had more extreme values than the clamped version
  expect_true(all(future_minmax[1,]<=clamped_minmax[1,]))
  expect_true(all(future_minmax[2,]>=clamped_minmax[2,]))
  # and that it had some more extreme values than the training
  expect_true(any(future_minmax[1,]<=training_minmax[1,]))
  expect_true(any(future_minmax[2,]>=training_minmax[2,]))
  # but the training range is bigger or equal to the clamped raster
  expect_true(all(training_minmax[1,]<=clamped_minmax[1,]))
  expect_true(all(training_minmax[2,]>=clamped_minmax[2,]))
  # get error for missing variable
  climate_sub<-climate_present[[1:2]]
  expect_error(clamp_predictors(climate_sub, training=lacerta_env),
               "`x` is missing the following")
  #error for missing class
  expect_error(clamp_predictors(lacerta_env, lacerta_env),
               "no method available for this object type")
})

test_that("clamping_predictor works on SpatRasterDatasets",{
  library(pastclim)
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
  horses_env <- horses_df[,c("bio01","bio10","bio12") ]
  clamped_sds <- clamp_predictors(climate_full, training=horses_env)
  climate_minmax<-lapply(climate_full,minmax)
  clumped_minmax <- lapply(clamped_sds,minmax)
  training_minmax <- apply(horses_env, 2, range, na.rm=TRUE)
  # expect the full climate to be broader than the clumped
  expect_true(all(climate_minmax[[1]][1,]<=clumped_minmax[[1]][1,]))
  expect_true(all(climate_minmax[[2]][2,]>=clumped_minmax[[2]][2,]))
  # expect the training set to be wider or equal
  expect_true(all(training_minmax[1,1]<=clumped_minmax[[1]][1,]))
  expect_true(all(training_minmax[2,2]>=clumped_minmax[[2]][2,]))
  # get error for missing variable
  climate_sub<-climate_full[[c("bio01","bio10")]]
  expect_error(clamp_predictors(climate_sub, training=horses_env),
               "`x` is missing the following")
}
)
