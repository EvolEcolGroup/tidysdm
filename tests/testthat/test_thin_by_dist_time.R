# set up a small world
# small grid
# library(terra)
# grid_raster <- terra::rast(matrix(1:16, ncol=4,byrow=TRUE),
#                    extent=terra::ext(c(-2,2,-2,2)),
#                    crs="epsg:4326")

# locations (first is off to the side, then two close to each other, and
# then two points in unique but closeby cells)
locations <- data.frame(
  lon = c(-1.5, -0.3, -0.6, 1.9, 1.4),
  lat = c(-1.8, 0.2, 0.8, -1.8, -0.5),
  time_bp = c(0, 0, 0, 0, 0),
  id = 1:5
)

test_that("thin_by_dist_time removes the correct points", {
  # with a data.frame that does not really involve time
  set.seed(123)
  thin_100k <- thin_by_dist_time(locations,
    dist_min = 100000,
    interval_min = 365 * 2000,
    time_col = "time_bp",
    lubridate_fun = pastclim::ybp2date
  )
  expect_true(inherits(thin_100k, "data.frame"))
  expect_false(inherits(thin_100k, "sf"))
  # we lose one of the points close to each other
  expect_true(setequal(thin_100k$id, c(1, 3, 4, 5)))

  # now set the times to prevent points 2 and 3 to overlap
  locations$time_bp <- c(0, -10000, -15000, -10000, -5000)
  set.seed(123)
  thin_100k_t <- thin_by_dist_time(locations,
    dist_min = 100000,
    interval_min = 365 * 2000,
    time_col = "time_bp",
    lubridate_fun = pastclim::ybp2date
  )
  # we should now get everything back
  expect_true(setequal(thin_100k_t$id, c(1, 2, 3, 4, 5)))

  # repeat with an sf object
  set.seed(123)
  locations_sf <- sf::st_as_sf(locations, coords = c("lon", "lat")) %>% sf::st_set_crs(4326)
  thin_100k_t_sf <- thin_by_dist_time(locations_sf,
    dist_min = 100000,
    interval_min = 365 * 2000,
    time_col = "time_bp",
    lubridate_fun = pastclim::ybp2date
  )
  expect_true(inherits(thin_100k_t_sf, "sf"))
  expect_true(inherits(thin_100k_t_sf, "data.frame")) # it is also a df!
  expect_true(all(thin_100k_t$id == thin_100k_t_sf$id))
})

# sample code to plot the small world to inspect what is going on
# plot(grid_raster,colNA="darkgray")
# polys(terra::as.polygons(grid_raster))
# points(vect(locations), col="red", cex=2)


test_that("thin_by_dist_time respects the projection", {
  # get the lacerta data and set crs to latlong
  horses <- sf::st_as_sf(horses, coords = c("longitude", "latitude"))
  sf::st_crs(horses) <- 4326
  # and npw project it
  iberia_proj4 <- "+proj=merc +lon_0=0 +lat_ts=10 +k=0.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  horses_proj <- sf::st_transform(horses, iberia_proj4)
  # thin the data with a mismatch in projections
  set.seed(123)
  horses_thin_gc <- thin_by_dist_time(horses_proj,
    dist_min = km2m(100),
    interval_min = y2d(2000),
    time_col = "time_bp",
    lubridate_fun = pastclim::ybp2date
  ) # great circle method
  set.seed(123)
  horses_thin_eu <- thin_by_dist_time(horses_proj,
    dist_min = km2m(100),
    interval_min = y2d(2000),
    time_col = "time_bp",
    lubridate_fun = pastclim::ybp2date,
    dist_method = "euclidean"
  ) # euclidean method
  # check that the thinning is not the same
  expect_false(nrow(horses_thin_gc) == nrow(horses_thin_eu))

  # check that the great circle method did not remove the crs from the data
  expect_equal(sf::st_crs(horses_thin_gc), sf::st_crs(horses_proj))
  # now thin the original dataset (in latlong)
  set.seed(123)
  horses_thin_gc_ll <- thin_by_dist_time(horses,
    dist_min = km2m(100),
    interval_min = y2d(2000),
    time_col = "time_bp",
    lubridate_fun = pastclim::ybp2date
  )
  # expect this to be identical to the great circle method
  expect_equal(horses_thin_gc$time_bp, horses_thin_gc_ll$time_bp)
})
