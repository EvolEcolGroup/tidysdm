# set up a small world
# small grid
library(terra)
grid_raster <- terra::rast(matrix(1:16, ncol = 4, byrow = TRUE),
  extent = terra::ext(c(-2, 2, -2, 2)),
  crs = "epsg:4326"
)

terra::add(grid_raster) <- grid_raster

# locations (first is off to the side, then two pairs to each other
locations <- data.frame(
  lon = c(-1.5, -0.3, -0.6, 1.9, 1.4),
  lat = c(-1.8, 0.2, 0.8, -1.8, -1.5),
  time_bp = c(0, 0, 0, -10, -10),
  id = 1:5
)

test_that("thin_by_cell_time removes the correct points", {
  # with a data.frame that does not really involve time
  expect_error(
    thin_by_cell_time(locations,
      raster = grid_raster,
      time_col = "time_bp",
      lubridate_fun = pastclim::ybp2date
    ),
    "`raster` does not have a time dimension"
  )
  pastclim::time_bp(grid_raster) <- c(0, -10)
  set.seed(123)
  thin_100k <- thin_by_cell_time(locations,
    raster = grid_raster,
    time_col = "time_bp",
    lubridate_fun = pastclim::ybp2date
  )
  expect_true(inherits(thin_100k, "data.frame"))
  expect_false(inherits(thin_100k, "sf"))
  # we lose onepoint in each pair
  expect_true(setequal(thin_100k$id, c(1, 2, 4)))

  # now set the times to prevent points 2 and 3 to overlap
  locations$time_bp <- c(0, 0, -10, -10, -10)
  set.seed(123)
  thin_100k_t <- thin_by_cell_time(locations,
    raster = grid_raster,
    time_col = "time_bp",
    lubridate_fun = pastclim::ybp2date
  )
  # we should now have the first pair, but lose one of the last two
  expect_true(setequal(thin_100k_t$id, c(1, 2, 3, 5)))

  # repeat with an sf object
  set.seed(123)
  locations_sf <- sf::st_as_sf(locations, coords = c("lon", "lat")) %>% sf::st_set_crs(4326)
  thin_100k_t_sf <- thin_by_cell_time(locations_sf,
    raster = grid_raster,
    time_col = "time_bp",
    lubridate_fun = pastclim::ybp2date
  )
  expect_true(inherits(thin_100k_t_sf, "sf"))
  expect_true(inherits(thin_100k_t_sf, "data.frame")) # it is also a df!
  expect_true(all(thin_100k_t$id == thin_100k_t_sf$id))

  # check that the function can handle a sf object with X, Y columns
  locations_xy <- locations_sf %>% dplyr::bind_cols(sf::st_coordinates(.))
  expect_no_error(thin_by_cell_time(locations_xy,
    raster = grid_raster,
    time_col = "time_bp",
    lubridate_fun = pastclim::ybp2date
  ))
  locations_xy$X <- rep(NA)
  expect_warning(
    thin_by_cell(locations_xy,
      raster = grid_raster
    ),
    "sf object contained 'X' and 'Y' coordinates that did not match the sf point geometry"
  )

  # now use a SpatRasterDataset
  raster_list <- list(bio01 = grid_raster, bio10 = grid_raster)
  grid_sds <- terra::sds(raster_list)
  set.seed(123)
  thin_100k_sd <- thin_by_cell_time(locations_sf,
    raster = grid_sds,
    time_col = "time_bp",
    lubridate_fun = pastclim::ybp2date
  )
  expect_true(inherits(thin_100k_sd, "sf"))
  expect_true(all(thin_100k_t$id == thin_100k_sd$id))
})

# sample code to plot the small world to inspect what is going on
# plot(grid_raster,colNA="darkgray")
# polys(terra::as.polygons(grid_raster))
# points(vect(locations), col="red", cex=2)
