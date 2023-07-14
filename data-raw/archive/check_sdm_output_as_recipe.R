check_sdm_outcome <- function(recipe,
                              ...,
                              role = NA,
                              trained = FALSE,
                              ref_dist = NULL,
                              options = list(presence = "presence"),
                              skip = FALSE,
                              id = rand_id("sdm_outcome")) {
  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)
  add_check(
    recipe,
    check_sdm_outcome_new(
      terms = terms,
      trained = trained,
      role = role,
      ref_dist = ref_dist,
      options = options,
      skip = skip,
      id = id
    )
  )
}
check_sdm_outcome_new <-
  function(terms,
           role,
           trained,
           ref_dist,
           options,
           skip,
           id) {
    check(
      subclass = "sdm_outcome",
      terms = terms,
      role = role,
      trained = trained,
      ref_dist = ref_dist,
      options = options,
      skip = skip,
      id = id
    )
  }
prep.check_sdm_outcome <- function(x, training, info = NULL, ...) {
  browser()
  col_names <- recipes_eval_select(x$terms, training, info)
  ## Check that the level exists
  #x$options$present %in%

  ## Use the constructor function to return the updated object.
  ## Note that `trained` is now set to TRUE
  check_sdm_outcome_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    ref_dist = ref_dist,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}
bake.check_sdm_outcome <- function(object, new_data, ...) {
  ## Check in case we do have the response variable in new_data (we shouldn't, really, or do we)
  browser()
  ## Always convert to tibbles on the way out
  tibble::as_tibble(new_data)
}
bradypus_recipe <- recipe(presence ~ ., data = bradypus_tb) %>%
  check_sdm_outcome(presence = "presence")
bradypus_recipe %>% prep(training = bradypus_tb)
