.onLoad <- function(libname, pkgname) {
  # CRAN OMP THREAD LIMIT
  # if OMP limit is unset, set it to 1
  # if the user has already set it to something else, leave it alone
  if (Sys.getenv("OMP_THREAD_LIMIT")=="") {
    Sys.setenv("OMP_THREAD_LIMIT" = 1)
  }
  # This defines maxent in the model database
  current <- parsnip::get_model_env()
  # only set the model if it does not exist yet
  # this allows to use devtools::load_all() repeatedly
  if (!any(current$models == "maxent")) {
    make_maxent()
  }
}
