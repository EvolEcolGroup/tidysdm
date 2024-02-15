x <- lacerta_rec
n_per_model <- 2

recipes_esm <- function (x, n_per_model){
  var_names <- x$var_info %>% filter(role=="predictor") %>% pull(variable)
  var_combn <- combn(var_names,n_per_model)
  subset_predictors <- function(vars,x){
    x %>% step_select(any_of(!!vars))
  }
  recipe_list <- apply(var_combn, 2, subset_predictors, x = x)
  return(recipe_list)
}

esm_rec_list <- recipes_esm(lacerta_rec, 2)
