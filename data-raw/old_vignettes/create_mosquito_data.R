library(tidysdm)
set.seed(123)
## Raster of climate
library(pastclim)
# SubSaharan Africa
species_vec<- terra::vect("POLYGON((-19.36 22.12,38.17 22.10,38.96 19.53,
                          40.76 16.98,43.71 12.12,52.36 13.59,54.30 7.03,
                          30.39 -34.59,15.28 -36.31,-19.18 13.59,-19.36 22.12))")
# climate variables to consider
climate_vars <- c("bio01","bio04", "bio06","bio12", "bio13","bio18", "lai")
climate_present<-pastclim::region_slice(time_bp = 0,
                                        bio_variables = climate_vars,
                                        data="Beyer2020",
                                        crop=species_vec)
terra::writeCDF(climate_present,"./inst/extdata/ssafrica.nc", overwrite=TRUE)
## Locations
library(readr)       # for importing data
aarab <- read_csv(system.file("extdata/arabiensis_wk_coordinates.csv",package="tidysdm")) %>%
  select(-ID) %>% relocate(latitude, .after=longitude)
aarab <- aarab[(cellFromXY(object = climate_present, xy = as.matrix(aarab[,1:2]))) %in%
                 cells(climate_present),]
# thin to one per cell
aarab_thin <- thin_by_cell(aarab, climate_present)

base_presences <- aarab_thin[sample(1:nrow(aarab_thin),500),] %>%
  bind_rows(aarab[sample(1:nrow(aarab),200),]) %>% mutate(class="presence")

all_locations<-sample_pseudoabs(base_presences,n=500, raster=climate_present)

plot(climate_present[[1]])
points(vect(all_locations[,1:2], geom=c("longitude","latitude")))
write.csv(all_locations,
          "./inst/extdata/arabiensis_coords.csv",row.names = FALSE)
