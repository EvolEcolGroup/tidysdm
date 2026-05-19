# packages
library(sf)
library(dplyr)
library(recipes)
library(workflowsets)
library(tidyterra)


# load the occurrences
data(lacerta)
lacerta <- st_as_sf(lacerta, coords = c("longitude", "latitude"))
st_crs(lacerta) <- "+proj=longlat"


# get the climate layers
climate_present <- terra::readRDS(
  system.file("extdata/lacerta_climate_present_10m.rds",
    package = "tidysdm"
  )
)
# project climate layers
iberia_proj4 <- paste0(
  "+proj=aea +lon_0=-4.0 +lat_1=36.8 +lat_2=42.6 ",
  "+lat_0=39.7 +datum=WGS84 +units=m +no_defs"
)
climate_present <- terra::project(climate_present, y = iberia_proj4)


# Create repeats

# empty object to store the simple ensembles that we will create
ensemble_list <- list()
set.seed(1234) # make sure you set the seed OUTSIDE the loop
for (i_repeat in 1:3) {
  # thin the data
  lacerta_thin_rep <- thin_by_cell(lacerta, raster = climate_present)
  lacerta_thin_rep <- thin_by_dist(lacerta_thin_rep, dist_min = 50000)
  # sample pseudo-absences
  lacerta_thin_rep <- sample_pseudoabs(lacerta_thin_rep,
    n = 1 * nrow(lacerta_thin_rep),
    raster = climate_present,
    method = c("dist_min", 50000)
  )
  # get climate
  lacerta_thin_rep <- lacerta_thin_rep %>%
    bind_cols(terra::extract(climate_present, lacerta_thin_rep, ID = FALSE))
  # create folds
  lacerta_thin_rep_cv <- spatial_block_cv(lacerta_thin_rep, v = 3)
  # create a recipe
  lacerta_thin_rep_rec <- recipe(lacerta_thin_rep, formula = class ~ .)
  # create a workflow_set
  lacerta_thin_rep_models <-
    # create the workflow_set
    workflow_set(
      preproc = list(default = lacerta_thin_rep_rec),
      models = list(
        # the standard glm specs
        glm = sdm_spec_glm(),
        # maxent specs with tuning
        maxent = sdm_spec_maxent()
      ),
      # make all combinations of preproc and models,
      cross = TRUE
    ) %>%
    # tweak controls to store information needed later to create the ensemble
    option_add(control = control_ensemble_grid())

  # train the model
  lacerta_thin_rep_models <-
    lacerta_thin_rep_models %>%
    workflow_map("tune_grid",
      resamples = lacerta_thin_rep_cv, grid = 3,
      metrics = sdm_metric_set(), verbose = TRUE
    )
  # make an simple ensemble and add it to the list
  ensemble_list[[i_repeat]] <- simple_ensemble() %>%
    add_member(lacerta_thin_rep_models, metric = "boyce_cont")
}

# Now we can create a `repeat_ensemble` from the list:
lacerta_rep_ens <- repeat_ensemble() %>% add_repeat(ensemble_list)


# check the predictions
#
prediction_present <- predict_raster(object = lacerta_rep_ens, raster = climate_present, fun = c ("mean", "weighted_mean"), class_fun = c("prop"))
ggplot() +
  geom_spatraster(data = prediction_present, aes(fill = mean)) +
  scale_fill_terrain_c()


# save lacerta_rep_ens as rds to the extdata folder
saveRDS(lacerta_rep_ens, file = "inst/extdata/lacerta_rep_ens.rds")


#
# # filter the models by boyce_cont > 0.5
# prediction_present_boyce <- predict_raster(lacerta_rep_ens, climate_present,
#                                            metric_thresh = c("boyce_cont", 0.7)
# )
