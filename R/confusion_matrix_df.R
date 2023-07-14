#' Make a confusion matrix dataframe for multiple thresholds
#'
#' Create the confusion matrix for multiple thresholds, using to optimise tss
#'
#' @param presences Probabilities for presences
#' @param absences probabilities for absences
#' @return A data.frame of thresholds with columns *thres*, *tp*, *fp*, *fn*, and *tn*
#' @keywords internal


conf_matrix_df <- function (presences, absences) {
# Threshold breaks (nice idea from flexsdm):
if (length(presences) > 1000) {
  thresh <- as.vector(stats::quantile(presences, seq(0,1,0.001)))
} else {
  thresh <- presences
}
if (length(absences) > 1000) {
  thresh <- c(thresh, as.vector(stats::quantile(absences, seq(0,1,0.001))))
} else {
  thresh <- c(thresh, absences)
}
# think whether we want to round here
thresh <- sort(unique(round(thresh, 8)))

# confusion matrix at different thresholds, 1 flattened matrix per row
return(data.frame(
  thresh = thresh,
  tp = unlist(lapply(thresh,FUN=function(x)sum(presences >= x))),
  fp = unlist(lapply(thresh,FUN=function(x)sum(absences >= x))),
  fn = unlist(lapply(thresh,FUN=function(x)sum(presences < x))),
  tn = unlist(lapply(thresh,FUN=function(x)sum(absences < x))))
)
}
