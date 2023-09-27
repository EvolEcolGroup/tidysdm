# https://www.tidymodels.org/learn/develop/recipes/

step_dummy_coords <- function(recipe, ...,
                        role = "predictor",
                        trained = FALSE,
                        inputs = NULL,
                        skip = FALSE,
                        id = rand_id("coords")) {
  inputs <- enquos(..., .named = TRUE)
  
  add_step(
    recipe,
    step_dummy_coords_new(
      terms = terms,
      trained = trained,
      role = role,
      inputs = inputs,
      skip = skip,
      id = id
    )
  )
}

step_dummy_coords_new <-
  function(terms, role, trained, inputs, skip, id) {
    step(
      subclass = "dummy_coords",
      terms = terms,
      role = role,
      trained = trained,
      inputs = inputs,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_dummy_coords <- function(x, training, info = NULL, ...) {
  step_dummy_coords_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    inputs = x$inputs,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_dummy_coords <- function(object, new_data, ...) {
  if (!all(c("X","Y") %in% names(new_data))){
    new_data <- dplyr::add_column(new_data, X = NA, Y = NA)
  } 
  browser()
  return(new_data)
  
}

#' 
#' print.step_rename <-
#'   function(x, width = max(20, options()$width - 35), ...) {
#'     title <- "Variable renaming for "
#'     trained_names <- names(x$inputs)
#'     
#'     untrained_terms <- rlang::parse_quos(
#'       trained_names %||% "",
#'       rlang::current_env()
#'     )
#'     print_step(trained_names, untrained_terms, x$trained, title, width)
#'     invisible(x)
#'   }
#' 
#' #' @rdname tidy.recipe
#' #' @export
#' tidy.step_rename <- function(x, ...) {
#'   var_expr <- map(x$inputs, quo_get_expr)
#'   var_expr <- map_chr(var_expr, quo_text, width = options()$width, nlines = 1)
#'   
#'   tibble(
#'     terms = names(x$inputs) %||% character(),
#'     value = unname(var_expr) %||% character(),
#'     id = rep(x$id, length(x$inputs))
#'   )
#' }