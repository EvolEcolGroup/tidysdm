# foo<-workflow(
#   preprocessor = lacerta_rec_uncor,
#   spec = sdm_spec_glm()
# )
# 
# foo<-lacerta_rec_uncor %>% prep(st_drop_geometry(lacerta_thin))
# foo <- prep(foo, lacerta_thin)
# bake (foo, lacerta_thin)
#' @export
prep.recipe<-function (x, training = NULL, fresh = FALSE, verbose = FALSE, 
                       retain = TRUE, log_changes = FALSE, strings_as_factors = TRUE, 
                       ...) {
  recipes:::prep.recipe(x=x,training=st_drop_geometry(training), fresh=fresh, verbose=FALSE,
                       retain=retain, log_changes=log_changes,
                       strings_as_factors =strings_as_factors, ...)
}

#' @export
bake.recipe <- function (object, new_data, ..., composition = "tibble") {
  recipes:::bake.recipe(object=object, ..., new_data = st_drop_geometry(new_data),
                        composition=composition)
}
