% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdm_spec_glm.R
\name{sdm_spec_glm}
\alias{sdm_spec_glm}
\title{Model specification for a GLM for SDM}
\usage{
sdm_spec_glm(..., tune = "none")
}
\arguments{
\item{...}{parameters to be passed to \code{\link[parsnip:logistic_reg]{parsnip::logistic_reg()}} to customise
the model. See the help of that function for details.}

\item{tune}{character defining the tuning strategy. As there are no
hyperparameters to tune in a \emph{glm}, the only valid option is "none". This
parameter is present for consistency with other \verb{sdm_spec_*} functions, but
it does nothing in this case.}
}
\value{
a \link[parsnip:model_spec]{parsnip::model_spec} of the model.
}
\description{
This function returns a \link[parsnip:model_spec]{parsnip::model_spec} for a Generalised Linear Model
to be used as a classifier of presences and absences in Species Distribution
Model.
}
\examples{
my_spec_glm <- sdm_spec_glm()
}
\seealso{
Other "sdm model specifications": 
\code{\link{sdm_spec_boost_tree}()},
\code{\link{sdm_spec_gam}()},
\code{\link{sdm_spec_maxent}()},
\code{\link{sdm_spec_rand_forest}()}
}
\concept{"sdm model specifications"}
