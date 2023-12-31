% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logisLateDicrease.R
\name{logisLateDicr}
\alias{logisLateDicr}
\title{Three-parameter logistic with the decrease in the late growth phase}
\usage{
logisLateDicr(
  dat,
  x,
  y,
  returnModels = FALSE,
  start = c(d0 = 50, r = 0.05, a = 1e-04),
  upper = c(d0 = 200, r = Inf, a = 1),
  lower = c(d0 = 0, r = 0, a = 0)
)
}
\arguments{
\item{dat}{data.frame including date and trait (e.g. canopy height).}

\item{x}{Column name (character) for the date after sowing or plantingl.}

\item{y}{Column name (character) for the trait.}

\item{returnModels}{Logical value whether to return the time-series model object. Default is F.}

\item{start}{Start values to estimate 'd0', 'r' and 'a'. Default is 'c(d0 = 50, r = 0.05, a = 0.0001)'.}

\item{upper}{Upper bounds to estimate 'd0', 'r' and 'a'. Default is 'c(d0 = 200, r = Inf, a = 1)'.}

\item{lower}{Lower bounds to estimate 'd0', 'r' and 'a'. Default is 'c(d0 = 0, r = 0, a = 0)'.}
}
\value{
logisLateDicr function returns the vector of estimated parameter values.
If returnModels = TRUE, this function also returns the cubic polynomial
regression object and logistic with the decrease in the
late growth phase regression object.
}
\description{
Apply a time-series model, three-parameter logistic with the decrease in the
late growth phase, to the time-series trait data.
This function was originally developed for the time-series data of rice canopy height.
Fitting the time-series model is done by the "two-step procedure".
For more information, see Taniguchi et al. (2022).
}
\examples{
library(phenolocrop)
riceCH_eg |>
   logisLateDicr("x", "height")

}
\references{
S. Taniguchi et al., (2022) Prediction of heading date, culm length, and
biomass from canopy-height-related parameters derived from time-series UAV observations of rice.
Frontiers in Plant Science, 13:998803.
}
