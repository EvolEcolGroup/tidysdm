library(tidysdm)
lacerta_thin <- readRDS(system.file("extdata/lacerta_climate_sf.RDS",
                                    package="tidysdm"))
lacerta_thin <- lacerta_thin %>% select(-c("bio01", "bio02", "bio03", "bio04", "bio07", "bio08", 
                                           "bio09", "bio10", "bio11", "bio12", "bio14", "bio16", 
                                           "bio17", "bio18", "bio19", "altitude"))
lacerta_rec <- recipe(lacerta_thin, formula=class~.)

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


## simple profile with recipes
bio05_prof <- lacerta_rec %>% 
  step_profile(-bio05, profile=vars(bio05)) %>%
  prep(training = lacerta_thin)

bio05_data <- bake(bio05_prof, new_data = NULL)

bio05_data <- bio05_data %>%
  mutate(
    pred = predict(lacerta_ensemble, bio05_data)$mean
  )

ggplot(bio05_data, aes(x = bio05, y = pred)) +
  geom_point(alpha = .5, cex = 1)


########
# with DALEX
library(DALEXtra)
data_train <- extract_mold(lacerta_ensemble$workflow[[1]])$predictors
data_response <- as.numeric(extract_mold(lacerta_ensemble$workflow[[1]])$outcomes %>% pull())-1

explainer_wkflow <- 
  explain_tidymodels(
    lacerta_ensemble$workflow[[1]], 
    data= data_train,
    y=data_response,
    label = lacerta_ensemble$wflow_id[[1]])

vip_wkflow <- model_parts(explainer = explainer_wkflow)
plot(vip_wkflow)



## could create a plot function for a list of model parts with the function at
## https://www.tmwr.org/explain

custom_predict <- function(X.model, newdata) {
  predict(X.model, newdata)$mean
}

explainer_ensemble <- explain(lacerta_ensemble, 
                              data = data_train,
                              y = (data_response-1)*-1,
                              predict_function = custom_predict,
                              type = "classification")
vip_ensemble <- model_parts(explainer = explainer_ensemble)
plot(vip_ensemble)
