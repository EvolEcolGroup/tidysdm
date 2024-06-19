x <- lacerta_rec
n_per_model <- 2

recipes_esm <- function (x, n_per_model){
  var_names <- x$var_info %>% filter(role=="predictor") %>% pull(variable)
  outcome_name <- x$var_info  %>% filter(role=="outcome") %>% pull(variable)
  var_combn <- combn(var_names,n_per_model)
  subset_predictors <- function(vars,x){
    x %>% step_select(any_of(!!c(vars,outcome_name)))
  }
  recipe_list <- apply(var_combn, 2, subset_predictors, x = x)
  names(recipe_list)<-apply(var_combn,2,paste,collapse=".")
  return(recipe_list)
}

esm_rec_list <- recipes_esm(lacerta_rec, 2)


lacerta_esm <-
  # create the workflow_set
  workflow_set(
    preproc = esm_rec_list,
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
autoplot(lacerta_cv)

set.seed(1234567)
lacerta_esm <-
  lacerta_esm %>%
  workflow_map("tune_grid",
               resamples = lacerta_cv, grid = 3,
               metrics = sdm_metric_set(), verbose = TRUE
  )
## This gives error
# - Error(s) x1: object '.' not found