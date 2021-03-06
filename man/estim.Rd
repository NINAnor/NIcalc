% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estim.R
\encoding{UTF-8}
\name{estim}
\alias{estim}
\title{Fit Probability Distribution}
\usage{
estim(obsval = NULL, proba = c(0.25, 0.75), type = "continuous")
}
\arguments{
\item{obsval}{double, length = 3, observed mean and quantiles in the sequence
\code{c(lower.quantile, mean, upper.quantile)}}

\item{proba}{double, length = 2, quantiles supplied in \code{"obsval"}.
Default is the lower and upper quartiles}

\item{type}{character, length = 1, type of measurement scales. Valid types
are \code{"continuous"} and \code{"discrete"}.
When \code{type = "continuous"},
a continuous model is fitted to the
indicator observation. When \code{type = "discrete"}, a discrete model is fitted.}
}
\value{
\code{estim} returns a data.frame with \code{dim = c(1,4)},
consisting of the following vectors\cr \code{[[1]] $distrib} character,
selected family for model distribution, i.e. one of \code{c("Gamma",
"LogNormal", "TruncNormal", "Weibull", "ZIExponential", "NegBinom",
"Poisson", "ZIP")}. \cr \code{[[2]] $mu} double,
first parameter of fitted model distribution\cr \code{[[3]] $sig}
double, second parameter of fitted model distribution\cr
\code{[[4]] $crit} double, sum of squared deviations between observed
parameters and those of the fitted model distribution.
}
\description{
Function that selects, for both continuous and discrete cases and by using
the least square criterion, the distribution among a predetermined set of
model distribution families that best fits to an expected value and two
quantiles.
}
\examples{
estim(obsval = c(0.3,0.6,0.8))
estim(obsval = c(6,13,25),proba = c(0.025,0.975), type = "continuous")
estim(obsval = c(6,13,25),proba = c(0.025,0.975), type = "discrete")

}
\seealso{
\code{\link{estimlight}} for a simplified version of
\code{estim},\cr\code{\link{elicitate}} for fitting probability
distributions to multiple indicator observations and for the list of model
distributions included in the predetermined set.\cr Function
\code{\link{qdev}} calculates sum of squares between
the parameters of the indicator observation and model distributions.
}
\author{
Nigel Yoccoz and Bård Pedersen
}
