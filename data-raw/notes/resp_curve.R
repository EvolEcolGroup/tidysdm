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
      rf = sdm_spec_rf(),
      
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
explain_tidysdm <- function(
    model,
    data = NULL,
    y = NULL,
    predict_function = NULL,
    predict_function_target_column = NULL,
    residual_function = NULL,
    label = NULL,
    verbose = TRUE,
    precalculate = TRUE,
    colorize = !isTRUE(getOption("knitr.in.progress")),
    model_info = NULL,
    type = "classification",
    by_workflow = FALSE
) {
  if (!inherits(model,"simple_ensemble")){
    stop("this function currently only works with simple_ensembles from tidysdm")
  }
  if (is.null(data)){
    data = extract_mold(lacerta_ensemble$workflow[[1]])$predictors
  }
  if (is.null(y)){
    # note that we need presences to be 1 and absences to be zero
    y <- (as.numeric(extract_mold(lacerta_ensemble$workflow[[1]])$outcomes %>% pull())-2)*-1
  } else {
    if (!is.factor(y)){
      stop("y should be a factor with presences as reference levels")
    } else {
      y <- (as.numeric(y)-2)*-1
    }
  }
  if (type!="classification"){
    stop("type has to be classification for a tidysdm ensemble")
  }
  if (is.null(predict_function)){
    predict_function <- function(model, newdata) {
      predict(model, newdata)$mean
    }
  }
  
  model_info <- list(package = "tidysdm",
                     ver = utils::packageVersion("tidysdm"),
                     type = "classification")
  
  DALEX::explain(
    model = model,
    data = data,
    y = y,
    predict_function = predict_function,
    predict_function_target_column = predict_function_target_column,
    residual_function = residual_function,
    weights = NULL,
    label = label,
    verbose = verbose,
    precalculate = precalculate,
    colorize = colorize,
    model_info = NULL,
    type = type
  )
}

# method for model info to work on simple ensemble
#' @rdname model_info
#' @export
model_info.simple_ensemble<- function(model, is_multiclass=FALSE,...){
  if (is_multiclass){
    stop("tidysdm simple_ensembles can not be multiclass")
  }
  package <- "tidysdm"
  type <- "classification"
  ver <- try(as.character(utils::packageVersion(package)), 
             silent = TRUE)
  if (inherits(ver, "try-error")) {
    ver <- "Unknown"
  }
  model_info <- list(package = package, ver = ver, type = type)
  class(model_info) <- "model_info"
  model_info
}

# code examples
explainer_lacerta_ens <- explain_tidysdm(lacerta_ensemble)
vip_ensemble <- model_parts(explainer = explainer_lacerta_ens)
plot(vip_ensemble)

pdp_bio05 <- model_profile(explainer_lacerta_ens, N = 500, variables = "bio05")
plot(pdp_bio05)



#########
library(DALEXtra)
model <- lacerta_ensemble
explainer_list <- list()
for (i in 1:nrow(model)){
  data_train <- workflowsets::extract_mold(model$workflow[[i]])$predictors
  data_response <- as.numeric(workflowsets::extract_mold(model$workflow[[i]])$outcomes %>% dplyr::pull())-1
  explainer_list[[i]] <- 
    DALEXtra::explain_tidymodels(
      model$workflow[[i]], 
      data= data_train,
      y=data_response,
      label = model$wflow_id[[i]])
}




