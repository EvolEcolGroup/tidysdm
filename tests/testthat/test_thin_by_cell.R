test_that("thin_by_cell respects projections", {
  # get the lacerta data and set crs to latlong
  lacerta <- sf::st_as_sf(lacerta, coords = c("longitude", "latitude"))
  sf::st_crs(lacerta) <- "+proj=longlat"
  # get the raster (with crs latlong)
  land_mask <- terra::readRDS(system.file("extdata/lacerta_land_mask.rds",
    package = "tidysdm"
  ))
  # and npw project it
  iberia_proj4 <- "+proj=aea +lon_0=-4.0 +lat_1=36.8 +lat_2=42.6 +lat_0=39.7 +datum=WGS84 +units=m +no_defs" # nolint
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
  # confirm that if we had used a data.frame with the wrong projection we would
  # get a nonsense result
  lacerta_df <- as.data.frame(lacerta) %>%
    dplyr::bind_cols(sf::st_coordinates(lacerta))
  set.seed(123)
  lacerta_thin_df <- thin_by_cell(lacerta_df, land_mask)
  expect_false(nrow(lacerta_thin_df) == nrow(lacerta_thin))
})


test_that("thin_by_cell works correctly with coords and sf", {
  library(terra)
  # Minimal raster grid
  r <- terra::rast(ncols = 2, nrows = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
            crs = "EPSG:4326")
  values(r) <- 1

  # sf points that ALSO keep longitude/latitude columns
  occ <- tibble::tibble(
    id = 1:3,
    longitude = c(0.25, 0.75, 1.25),
    latitude  = c(0.25, 0.75, 1.25)
  ) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # this should work
  set.seed(123)
  new_obs <- thin_by_cell(occ, r)
  # if we give coords, it should be ignored and we should get a warning
  set.seed(123)
  expect_warning(
    new_obs_coords <- thin_by_cell(occ, r, coords = c("longitude",
      "latitude"
    )),
    "The 'coords' argument is ignored when 'data' is an sf object, as")
  expect_equal(new_obs, new_obs_coords)
  # no warning with X and Y as coords
  set.seed(123)
  new_obs_XY <- thin_by_cell(occ, r, coords = c("X", "Y"))
  expect_equal(new_obs, new_obs_XY)
  names(occ)<- c("id", "X", "Y", "geometry")
  set.seed(123)
  new_obs_XY_rep <- thin_by_cell(occ, r, coords = c("X", "Y"))

})
