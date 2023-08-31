library(tidysdm)
lacerta_thin <- readRDS(system.file("extdata/lacerta_climate_sf.RDS",
                                    package="tidysdm"))
lacerta_rec <- recipe(lacerta_thin, formula=class~.) %>% step_rm(all_of( c("bio01", "bio02", "bio03", "bio04", "bio07", "bio08", "bio09", "bio10", "bio11", "bio12", "bio14", "bio16", "bio17", "bio18", "bio19", "altitude")))

lacerta_models <-
  # create the workflow_set
  workflow_set(
    preproc = list(default = lacerta_rec),
    models = list(
      # the standard glm specs
      glm = sdm_spec_glm(),
      # rf specs with tuning
      rf = sdm_spec_rf()
    ),
    # make all combinations of preproc and models,
    cross = TRUE
  ) %>%
  # tweak controls to store information needed later to create the ensemble
  option_add(control = control_ensemble_grid())
set.seed(100)
lacerta_cv <- spatial_block_cv(lacerta_thin, v = 5)
lacerta_models <- 
  lacerta_models %>% 
  workflow_map("tune_grid", resamples = lacerta_cv, grid = 3, 
               metrics = sdm_metric_set(), verbose = TRUE)
lacerta_ensemble <- simple_ensemble() %>%
  add_member(lacerta_models, metric="boyce_cont")
lacerta_ensemble



x<-lacerta_ensemble

# check the variable is present in the workflows of interest

# for each workflow, get the range of the variable of interest, and the mean of
# all other continuous predictors (and the reference of any categorical predictor)

# predict that data.table


for (i in 1:nrow(x)){
  this_predictors <- workflowsets::extract_mold(x$workflow[[i]])$predictors
}