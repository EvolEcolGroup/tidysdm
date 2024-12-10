test_that("thin_by_cell respects projections", {
  # get the lacerta data and set crs to latlong
  lacerta <- sf::st_as_sf(lacerta, coords = c("longitude", "latitude"))
  sf::st_crs(lacerta) <- "+proj=longlat"
  # get the raster (with crs latlong)
  land_mask <- terra::readRDS(system.file("extdata/lacerta_land_mask.rds",
                                          package = "tidysdm"
  ))
  # and npw project it
  iberia_proj4 <- "+proj=aea +lon_0=-4.0 +lat_1=36.8 +lat_2=42.6 +lat_0=39.7 +datum=WGS84 +units=m +no_defs"
  land_mask <- terra::project(land_mask, y = iberia_proj4)
  # thin the data with a mismatch in projections
  set.seed(123)
  lacerta_thin <- thin_by_cell(lacerta, land_mask)
  # now project the points
  lacerta_proj <- sf::st_transform(lacerta, iberia_proj4)
  # and thin the data with matching projections
  set.seed(123)
  lacerta_thin_proj <- thin_by_cell(lacerta_proj, land_mask)
  # check that the thinning is the same
  expect_equal(lacerta_thin, lacerta_thin_proj)
  # confirm that if we had used a data.frame with the wrong projection we would get a nonsense result
  lacerta_df <- as.data.frame(lacerta) %>% dplyr::bind_cols(sf::st_coordinates(lacerta))
  set.seed(123)
  lacerta_thin_df <- thin_by_cell(lacerta_df, land_mask)
  expect_false(nrow(lacerta_thin_df) == nrow(lacerta_thin))

})
