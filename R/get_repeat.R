#' Extract a simple ensemble out of a repeat ensemble
#'
#' @details Note that, when using a numerical index, that index refers to the
#' order of the repeats for which at least one algorithm passed the appropriate
#' thresholds set when creating the ensemble. For example, if "rep_02" had not
#' good algorithms, the repeat ensemble would only contain labels "rep_01",
#' "rep_03", etc. So, index 2 would refer to "rep_03" in this case. If you want
#' to be sure to get a given repeat, it is safer to use the name of the repeat
#' (e.g. "rep_03") rather than the index.
#'
#' @description This function extracts a simple ensemble out of a repeat
#'   ensemble, by index or by name.
#' @param x a repeat ensemble object
#' @param i index of the simple ensemble to extract, or name of the simple
#'   ensemble to extract.
#' @return a simple ensemble object
#' @export
#' @examples
#' # extract the second simple ensemble out of the repeat ensemble
#' get_repeat(lacerta_rep_ens, i = "rep_02")
get_repeat <- function(x, i) {
  if (!inherits(x, "repeat_ensemble")) {
    stop("x must be a repeat ensemble object")
  }
  if (is.numeric(i)){
    # check that it is an integer
    if (i != as.integer(i)) {
      stop("i must be an integer")
    }
    i <- levels(as.factor(x$rep_id))[i]
  }


  if (is.character(i)) {
    if (!i %in% x$rep_id) {
      stop("i must be a valid name of a repeat in x")
    }
    simple_ens <- x %>% dplyr::filter(.data$rep_id == i)
  }
  # change the class to a repeated ensemble
  class(simple_ens)[class(simple_ens) == "repeat_ensemble"] <- "simple_ensemble"
  # TODO extract the attributes of the simple ensemble from the repeat ensemble
  # e.g. calibration info
  # if we have a class_calibration_list, get the relevant info for this repeat
   if (!is.null(attr(x, "class_thresholds_list", exact = TRUE))) {
     attr(simple_ens, "class_thresholds") <-
       attr(x, "class_thresholds_list", exact = TRUE)[[i]]
     # and remove the list
     attr(simple_ens, "class_thresholds_list") <- NULL
   }
  return(simple_ens)
}
