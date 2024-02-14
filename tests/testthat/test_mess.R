test_that("mess_predictor works on SpatRasters",{
  # now get future climate
  climate_future <- terra::rast(system.file("extdata/lacerta_climate_future_10m.nc",
                                     package = "tidysdm"
  ))
  mess_rast <- mess_predictors(climate_future, training = lacerta_thin, .col= class)
  expect_true(inherits(mess_rast, "SpatRaster"))
})

# test_that("clamping_predictor works on SpatRasterDatasets",{
#   library(pastclim)
#   set_data_path(on_CRAN = TRUE)
#   library(sf)
#   climate_vars <- c("bio01", "bio10", "bio12")
#   climate_full <- pastclim::region_series(
#     bio_variables = climate_vars,
#     data = "Example",
#     crop = region_outline$Europe
#   )
#   # get climate
#   horses_df <- location_slice_from_region_series(horses,
#                                                  region_series = climate_full,
#                                                  nn_interpol = FALSE
#   )
#   horses_env <- horses_df[,c("bio01","bio10","bio12") ]
#   clamped_sds <- clamp_predictors(climate_full, training=horses_env)
#   climate_minmax<-lapply(climate_full,minmax)
#   clumped_minmax <- lapply(clamped_sds,minmax)
#   training_minmax <- apply(horses_env, 2, range, na.rm=TRUE)
#   # expect the full climate to be broader than the clumped
#   expect_true(all(climate_minmax[[1]][1,]<=clumped_minmax[[1]][1,]))
#   expect_true(all(climate_minmax[[2]][2,]>=clumped_minmax[[2]][2,]))
#   # expect the training set to be wider or equal
#   expect_true(all(training_minmax[1,1]<=clumped_minmax[[1]][1,]))
#   expect_true(all(training_minmax[2,2]>=clumped_minmax[[2]][2,]))
#   # get error for missing variable
#   climate_sub<-climate_full[[c("bio01","bio10")]]
#   expect_error(clamp_predictors(climate_sub, training=horses_env),
#                "`x` is missing the following")
#   # now add a class column
#   horses_env$class <- "presence"
#   clamped_rast <- clamp_predictors(climate_full, horses_env, .col=class)
#   expect_true(inherits(clamped_rast,"SpatRasterDataset"))
#   # error if we fail to specify the class column
#   expect_error(clamp_predictors(climate_full, horses_env),
#                "`x` is missing the following variables")  
# }
# )
