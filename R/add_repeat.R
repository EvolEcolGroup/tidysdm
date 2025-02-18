#' Add repeat(s) to a repeated ensemble
#'
#' This function adds repeat(s) to a [`repeat_ensemble`] object, where each
#' repeat is a [`simple_ensemble`]. All repeats must contain the same members,
#' selected using the same metric.
#'
#' @param x a [repeat_ensemble] to which repeat(s) will be added
#' @param rep a repeat, as a single [`simple_ensemble`], or a list of
#' [`simple_ensemble`] objects
#' @param ... not used at the moment.
#' @returns a [repeat_ensemble] with additional repeat(s)
#' @export
#' @keywords ensemble

# Note that `add_repeat` is tested in `test_repeat_ensemble.R`

add_repeat <- function(x, rep, ...) {
  UseMethod("add_repeat", object = rep)
}

#' @rdname add_repeat
#' @export
add_repeat.default <- function(x, rep, ...) {
  stop("no method available for this object type")
}

#' @rdname add_repeat
#' @export
add_repeat.simple_ensemble <- function(x, rep, ...) {
  # if the repeated ensemble is empty
  if (nrow(x) == 0) {
    attr(x, "best_metric") <- attr(rep, "best_metric")
    attr(x, "metrics") <- attr(rep, "metrics")
    new_rep <- "rep_01"
  } else {
    # check that the new rep is compatible
    # check that metrics match
    if (attr(x, "best_metric") != attr(rep, "best_metric")) {
      stop("the best metric in the repeated ensemble differs ",
           "from the repeat being added")
    }
    if (all(attr(x, "metrics") != attr(rep, "metrics"))) {
      stop("the metrics in the repeated ensemble differ from ",
           "the repeat being added")
    }
    rep_number <- max(as.numeric(substr(x$rep_id, 5, nchar(x$rep_id)))) + 1
    new_rep <- paste0("rep_", sprintf("%02d", rep_number))
    if (!setequal(unique(x$wflow_id), rep$wflow_id)) {
      stop("the models in the repeated ensemble differ ",
           "from the repeat being added")
    }
  }
  rep <- rep %>% dplyr::mutate(rep_id = new_rep, .before = "wflow_id")
  x %>% dplyr::bind_rows(rep)
}

#' @rdname add_repeat
#' @export
add_repeat.list <- function(x, rep, ...) {
  for (i in seq_along(rep)) {
    x <- x %>% add_repeat(rep[[i]])
  }
  x
}
