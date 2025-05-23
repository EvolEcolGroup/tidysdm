% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optim_thresh.R
\name{optim_thresh_tss_max}
\alias{optim_thresh_tss_max}
\title{Find threshold that maximises TSS}
\usage{
optim_thresh_tss_max(presences, absences)
}
\arguments{
\item{presences}{Probabilities for presences.}

\item{absences}{Provabilities for absences}
}
\value{
the probability threshold for the event
}
\description{
This is an internal function returns the threshold to turn probabilities into
binary classes to maximise TSS
}
\keyword{internal}
