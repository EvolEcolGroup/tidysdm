# set up a small world
# small grid
#grid_raster <- terra::rast(matrix(1:16, ncol=4,byrow=TRUE),
#                    extent=terra::ext(c(-2,2,-2,2)),
#                    crs="epsg:4326")

# locations (first is off to the side, then two close to each other, and
# then two points in unique but closeby cells)
locations <- data.frame(lon=c(-1.5, -0.3, -0.6, 1.9, 1.4),
                        lat=c(-1.8, 0.2, 0.8, -1.8, -0.5),
                        id = 1:5)

test_that("thin_by_dist removes the correct points", {
  # with a data.frame
  set.seed(123)
  thin_100k <- thin_by_dist(locations, dist_min = 100000)
  expect_true(inherits(thin_100k,"data.frame"))
  expect_false(inherits(thin_100k,"sf"))
  # we lose one of the points close to each other
  expect_true(setequal(thin_100k$id,c(1,3,4,5)))
  # now keep NA
  thin_200k <- thin_by_dist(locations, dist_min = 200000)
  # we now also lose one of the points in adjacent cells
  expect_true(setequal(thin_200k$id,c(1,3,5)))

  # repeat with an sf object
  set.seed(123)
  locations_sf <-  sf::st_as_sf(locations, coords=c("lon","lat")) %>% sf::st_set_crs(4326)
  thin_100k_sf <- thin_by_dist(locations_sf, dist_min = 100000)
  expect_true(inherits(thin_100k_sf,"sf"))
  expect_true(inherits(thin_100k_sf,"data.frame")) # it is also a df!
  expect_true(all(thin_100k$id ==thin_100k_sf$id))
})

# sample code to plot the small world to inspect what is going on
# plot(grid_raster,colNA="darkgray")
# polys(terra::as.polygons(grid_raster))
# points(vect(locations), col="red", cex=2)
