% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputeData.R
\encoding{UTF-8}
\name{imputeData}
\alias{imputeData}
\title{Multiple Imputations}
\usage{
imputeData(x = NULL, nSim = 1000, transConst = 0.01, ...)
}
\arguments{
\item{x}{list of class \code{niInput}.}

\item{nSim}{integer, number of imputations, default is 1000.}

\item{transConst}{numeric scalar, \code{0<transConst<=0.1}.
Transformation constant in log-transformation. Default is \code{0.01}.}

\item{...}{further arguments passed on to \code{mice::mice}.}
}
\value{
A list of class \code{niImputations} containing two elements:
  \cr\code{identifiers}: a data.frame with variables relating each imputed
  indicator observation to a missing observation in the data set \code{x}.
  \cr\code{imputations}: a numeric matrix where each row represents a missing
  indicator observation in the corresponding data set and contains single
  draws from each of \code{nSim} imputed distributions.
}
\description{
Multivariate imputation by chained equations. Add multiple imputations
for missing indicator observations in a Nature Index data set.
}
\details{
Two general approaches for imputing multivariate data have
emerged: joint modeling (JM) and fully conditional specification (FCS), also
known as multivariate imputation by chained equations (MICE). JM involves
specifying a multivariate distribution for the missing data, and drawing
imputations from their conditional distributions by Markov Chain Monte Carlo
(MCMC) techniques. This methodology is attractive if the multivariate
distribution is a reasonable description of the data. FCS specifies the
multivariate imputation model on a variable-by-variable basis by a set of
conditional densities, one for each incomplete variable. Starting from an
initial imputation, FCS draws imputations by iterating over the conditional
densities.

A JM approach using the R-package \code{Amelia} has been tested for missing
indicator observations in Nature Index data sets. This approach was, however,
not robust when implemented as a general method for all indicators. The
routine often crashed when the joint distribution model (multivariate normal)
was not suitable and sometimes led to fatal errors in the CPU.
\code{imputeData} therefore adopt the FCS approach using the routine
\code{\link[mice:mice]{mice::mice}}.

\code{imputeData} performs multiple imputations for all missing indicator
observations. Each imputation consists of an expected value, a lower quartile
and the interquartile distance (ID). The upper quartile of the imputed
indicator observation is calculated from the lower quartile and the
interquartile distance.

Indicator observations are normalized against their corresponding reference
value and thereafter log-transformed before imputation modeling.
The imputation model includes five variables in the order \code{logmean},
\code{loglower}, \code{logID}, \code{year}, \code{indicator}. A common
pattern of missing values in the data is that all data for some indicators
are missing for some years. This leads to empty cell problems if the
imputation model includes interactions between year and indicator.
The imputation model therefore does not contain interaction terms.

As default, \code{imputeData} uses predictive mean matching as imputation
method and calls the function \code{mice} in package \code{mice} with arguments
\code{m = nSim} and \code{method = c("pmm", "pmm", "pmm", "", "")}.

The argument \code{nSim} determines the number of imputations.
A continuous probability distribution is fitted to each imputation by
\code{\link{elicitate}}. \code{imputeData} draws and returns one observation
from each distribution.
}
\examples{
\dontrun{
imputedValues <- imputeData(x = themeData,
                                nSim = 1000,
                                transConst = 0.01,
                                maxit = 20,
                                printFlag = TRUE)
}

}
\seealso{
\code{\link{imputeDiagnostics}}, \code{\link{impStand}},
\code{\link[mice:mice]{mice::mice}}, \code{\link{elicitate}}, and
\code{\link{calculateIndex}}. \cr The vignette \code{objectsInNIcalc} gives a
more detailed description of \code{\link{niImputations}} and
\code{\link{niInput}} lists.
}
\author{
Bård Pedersen
}
