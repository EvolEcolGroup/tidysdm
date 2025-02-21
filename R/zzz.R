.onLoad <- function(libname, pkgname) {
  # This defines maxent in the model database
  current <- parsnip::get_model_env()
  # only set the model if it does not exist yet
  # this allows to use devtools::load_all() repeatedly
  if (!any(current$models == "maxent")) {
    make_maxent()
  }
}
