# set up a small world
# small grid
library(terra)
grid_raster <- terra::rast(matrix(1:64, ncol = 8, byrow = TRUE),
  extent = terra::ext(c(-2, 2, -2, 2)),
  crs = "lonlat"
)
# crs="epsg:4326")
grid_raster[c(1:5, 15, 16, 23, 24)] <- NA

# locations (first isolated, two closer to each other)
locations <- data.frame(
  lon = c(0.8, 1.9, 0.7),
  lat = c(1.3, -1.8, -0.9),
  id = 1:3
)
#min_buffer <- terra::buffer(terra::vect(locations, crs = "lonlat"), 60000)
max_buffer <- terra::buffer(terra::vect(locations, crs = "lonlat"), 90000)


# points in poygons function
# (from https://stackoverflow.com/questions/72384038/point-in-polygon-using-terra-package-in-r)
pts_in_polys <- function(pts, polys) {
  e <- terra::extract(polys, pts)
  e[!is.na(e[, 2]), 1]
}

test_that("sample_background samples in the right places", {
  set.seed(123)
  bg_random <- sample_background(locations,
    n = 25, raster = grid_raster,
    return_pres = FALSE
  )
  # we should now check that some points are outside the buffers
  expect_true(length(pts_in_polys(terra::vect(bg_random), max_buffer)) < 25)
  # some points conside with the presences
  expect_true(any(terra::extract(grid_raster, bg_random[,1:2])$ID %in%
                    terra::extract(grid_raster, locations[,1:2])$ID))
  # there should be no presences
  expect_true(unique(bg_random$class) == "background")


  # now use a maximum buffer
  set.seed(123)
  bg_max <- sample_background(locations,
    n = 5, raster = grid_raster,
    method = c("dist_max", 90000),
    return_pres = FALSE
  )
  # all are within the max buffer
  expect_true(length(pts_in_polys(terra::vect(bg_max), max_buffer)) == nrow(bg_max))

  # and now use the values as bias
  # first make 2 rows of the raster 100 times more probably than the top
  grid_raster[c(6,8),] <- 100*grid_raster[c(6,8),]
  set.seed(123)
  bg_bias <- sample_background(locations,
    n = 25, raster = grid_raster,
    method = "bias",
    return_pres = FALSE
  )
  # all cells in those two rows should include points
  expect_true(sum(bg_bias$lat %in% c(-0.75,-1.75))==16)
  
  bg_pres <- sample_background(locations, n = 25, raster = grid_raster)
  expect_true(all(levels(bg_pres$class) == c("presence", "background")))

  # now confirm that it all works if we use an sf object
  set.seed(123)
  locations_sf <- sf::st_as_sf(locations, coords = c("lon", "lat")) %>% sf::st_set_crs(4326)
  bg_random_sf <- sample_background(locations_sf,
    n = 25, raster = grid_raster,
    return_pres = FALSE
  )
  expect_true(inherits(bg_random_sf, "sf"))
  expect_true(inherits(bg_random_sf, "data.frame")) # it is also a df!
  expect_true(all(bg_random$id == bg_random_sf$id))

  # test error messages
  expect_error(
    sample_background(locations_sf, n = 25, raster = grid_raster, method = "blah"),
    "method has to be"
  )
  expect_error(
    sample_background(locations_sf, n = 25, raster = grid_raster, method = c("blah", 25)),
    "method has to be"
  )
  expect_error(
    sample_background(locations_sf, n = 25, raster = grid_raster, method = c("dist_max",10,20)),
    "method 'dist_max' should have one threshold"
  ) 
})

test_that("handling of data frames and sf objects", {
  locations_sf <- sf::st_as_sf(locations, coords = c("lon", "lat")) %>% sf::st_set_crs(4326)
  expect_error(
    sample_background(locations_sf, coords = c("x", "y"), raster = grid_raster),
    "There are no recognised coordinate columns"
  )
  expect_warning(sample_background(locations_sf, raster = grid_raster, n = 100), "There are fewer available cells for raster 'NA' (3 presences) than the requested 100 background points. Only 55 will be returned.", fixed = TRUE)
  locations_sf <- locations_sf %>% dplyr::bind_cols(sf::st_coordinates(.))
  expect_no_error(sample_background(locations_sf, coords = c("X", "Y"), raster = grid_raster, n = 25))
  locations_sf$X <- rep(0, 3)
  locations_sf$Y <- rep(0, 3)
  expect_error(
    sample_background(locations_sf, coords = c("X", "Y"), raster = grid_raster, n = 25),
    "sf object contains X and Y coordinates that do not match the sf point geometry"
  )
  locations_sf$X <- rep(NA, 3)
  locations_sf$Y <- rep(NA, 3)
  expect_error(
    sample_background(locations_sf, coords = c("X", "Y"), raster = grid_raster, n = 25),
    "sf object contains NA values in the X and Y coordinates"
  )
})


test_that("sample_background works with stars", {
  set.seed(123)
  expect_no_error(sample_background(locations,
                                 n = 25, 
                                 raster = stars::st_as_stars(grid_raster, as_attributes = TRUE),
                                 return_pres = FALSE))
})

# sample code to plot points in and buffers
# plot(grid_raster,colNA="darkgray")
# polys(terra::as.polygons(grid_raster))
# points(vect(locations), col="red", cex=2)
# points(vect(bg_bias), col="blue", cex=2)
# polys(min_buffer)
# polys(max_buffer)
