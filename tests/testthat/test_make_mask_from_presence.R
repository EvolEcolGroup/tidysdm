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

# convert it to an sf object
locations_sf <- sf::st_as_sf(locations, coords = c("lon", "lat")) %>% sf::st_set_crs(4326)

#min_buffer <- terra::buffer(terra::vect(locations, crs = "lonlat"), 60000)
#max_buffer <- terra::buffer(terra::vect(locations, crs = "lonlat"), 90000)


# points in poygons function
# (from https://stackoverflow.com/questions/72384038/point-in-polygon-using-terra-package-in-r)
pts_in_polys <- function(pts, polys) {
  e <- terra::extract(polys, pts)
  e[!is.na(e[, 2]), 1]
}

test_that("make_mask_from_presence works correctly", {
  set.seed(123)
  # create a buffer
  mask_buffer <- make_mask_from_presence(locations_sf,method = "buffer", buffer = 60000, return_sf = TRUE)
  # expect points close to the presences to be included in the mask buffer
  buffer_locations <- data.frame(
    lon = c(0.5, 1, 2),
    lat = c(-1, 1.6, -2),
    id = 1:3
  ) %>% sf::st_as_sf(coords = c("lon", "lat")) %>% sf::st_set_crs(4326)
  expect_true(nrow(sf::st_filter(buffer_locations, mask_buffer))==3)
  # create a minimum convex polygon
  mask_ch <- make_mask_from_presence(locations_sf, method = "convex_hull", return_sf = TRUE)
  # expect points close to the presences not to be included in the mask convex hull if they are off to the sides
  expect_true(nrow(sf::st_filter(buffer_locations, mask_ch))==0)
  ch_locations <- data.frame(
    lon = c(1, 1.5, 1),
    lat = c(-0.5, -1.3, -0),
    id = 1:3
  ) %>% sf::st_as_sf(coords = c("lon", "lat")) %>% sf::st_set_crs(4326)
  expect_true(nrow(sf::st_filter(ch_locations, mask_ch))==3)
  # only one of these locations is within the buffer
  expect_true(nrow(sf::st_filter(ch_locations, mask_buffer))==1)
  # create a convex hull with a buffer
  mask_ch_buffer <- make_mask_from_presence(locations_sf, method = "convex_hull", buffer = 60000, return_sf = TRUE)
  # all locations should be within this bigger ch
  expect_true(nrow(sf::st_filter(ch_locations, mask_ch_buffer))==3)
  expect_true(nrow(sf::st_filter(buffer_locations, mask_ch_buffer))==3)

})

# sample code to plot points in and buffers
# plot(grid_raster,colNA="darkgray")
# polys(terra::as.polygons(grid_raster))
# points(vect(locations), col="red", cex=2)
# points(vect(mask_locations), col="lightblue", cex=2)
# points(vect(buffer_locations), col="green", cex=2)
# polys (vect(mask_buffer))
# polys(vect(mask_ch), col="blue", cex=2)
# polys(vect(mask_ch_buffer), col="green", cex=2)
# polys(min_buffer)
# polys(max_buffer)
