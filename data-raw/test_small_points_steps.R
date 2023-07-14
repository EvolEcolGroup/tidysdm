# small grid
grid_raster <- rast(matrix(1:16, ncol=4,byrow=TRUE),
                    extent=ext(c(-2,2,-2,2)),
                    crs="epsg:4326")
grid_polys<- terra::as.polygons(grid)
grid_raster[c(1,2,15)]<-NA

# locations (first is NA, then two close to each other, adn then one close to the margin)
locations <- data.frame(lon=c(-1.5, -0.3, -0.6, 1.9, 1.4),
                        lat=c(1.8, 0.2, 0.8, -1.8, -0.5),
                        id = 1:5)

#
set.seed(123)
plot(grid_raster,colNA="darkgray")
polys(grid_polys)
points(vect(locations), col="red", cex=2)

locations_thin1<-thin_by_cell(locations, grid_raster)
# we expect 3 points
points(vect(locations_thin1), col="blue", cex=2)

plot(grid_raster,colNA="darkgray")
polys(grid_polys)
points(vect(locations), col="red", cex=2)
locations_thin1<-thin_by_cell(locations, grid_raster,agg_fact = 2)
# we expect 2 points
# now show the big grid
grid_raster_big <- aggregate(grid_raster,2)
grid_polys_big<- terra::as.polygons(grid)

points(vect(locations_thin1), col="blue", cex=2)

# plot a buffer
polys(buffer(vect(locations_thin1, crs="epsg:4326"),70000))

