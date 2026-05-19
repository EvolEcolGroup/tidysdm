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
  # check that x is a repeat ensemble
  if (!inherits(x, "repeat_ensemble")) {
    stop("x must be a repeat ensemble object")
  }
  
  # only a single repeat can be extracted at a time
  if (length(i) != 1) {
    stop("i must be of length 1")
  }
  
  # store the valid repeat ids
  rep_ids <- levels(as.factor(x$rep_id))
  
  # if i is numeric, interpret it as the position of the repeat
  if (is.numeric(i)) {
    
    # reject NA, NaN and Inf values
    if (is.na(i) || !is.finite(i)) {
      stop("i must not be NA or non-finite")
    }
    
    # only integer indices are allowed
    if (i != as.integer(i)) {
      stop("i must be an integer")
    }
    
    # check that the index is within range
    if (i < 1 || i > length(rep_ids)) {
      stop("i is outside the range of repeats in x")
    }
    
    # convert the numeric index to the corresponding repeat id
    i <- rep_ids[i]
    
  } else if (is.character(i)) {
    
    # reject missing character values
    if (is.na(i)) {
      stop("i must not be NA")
    }
    
    # check that the repeat exists in the ensemble
    if (!i %in% rep_ids) {
      stop("i must be a valid name of a repeat in x")
    }
    
  } else {
    
    # only numeric or character input is supported
    stop("i must be either numeric or character")
  }
  
  # extract the requested repeat
  simple_ens <- x %>%
    dplyr::filter(.data$rep_id == i)
  
  # convert the class from repeat_ensemble to simple_ensemble
  class(simple_ens)[class(simple_ens) == "repeat_ensemble"] <-
    "simple_ensemble"
  
  # if calibration thresholds are available, extract the relevant ones
  # for this repeat and store them as standard simple ensemble attributes
  if (!is.null(attr(x, "class_thresholds_list", exact = TRUE))) {
    attr(simple_ens, "class_thresholds") <-
      attr(x, "class_thresholds_list", exact = TRUE)[[i]]
    
    # remove the repeat-level calibration list from the extracted object
    attr(simple_ens, "class_thresholds_list") <- NULL
  }
  
  return(simple_ens)
}
