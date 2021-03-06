% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/niDataImport.R
\encoding{UTF-8}
\name{niDataImport}
\alias{niDataImport}
\alias{is.niDataImport}
\title{Create or test for Objects of Class niDataImport}
\usage{
niDataImport(
  indicators = NULL,
  referenceValues = NULL,
  indicatorObservations = NULL,
  ICunits = NULL,
  BSunits = NULL,
  ecosystems = NULL
)

is.niDataImport(x)
}
\arguments{
\item{indicators}{tibble of indicator data.}

\item{referenceValues}{list of reference value data.}

\item{indicatorObservations}{list of observation data.}

\item{ICunits}{tibble of ICunit data.}

\item{BSunits}{tibble of BSunit data.}

\item{ecosystems}{tibble of ecosystem data.}

\item{x}{object to be tested.}
}
\value{
\code{niDataImport} returns a list of class \code{niDataImport} with elements:
   \tabular{ll}{\code{[[1]]} \tab \code{indicators}\cr
   \code{[[2]]} \tab \code{referenceValues}\cr
   \code{[[3]]} \tab \code{indicatorObservations}\cr
   \code{[[4]]} \tab \code{ICunits}\cr
   \code{[[5]]} \tab \code{BSunits}\cr
   \code{[[6]]} \tab \code{ecosystems}}
   \code{is.niDataImport} returns \code{TRUE} if the argument is of class
   \code{niDataImport}.
}
\description{
\code{niDataImport} assembles arguments and returns an \code{S3} object of class
\code{niDataImport}. \cr\code{is.niDataImport} tests if a candidate object is of
class \code{niDataImport}.
}
\details{
Lists of class \code{niDataImport} contain a complete data set for calculating
the Nature Index or a thematic index. The function \code{\link{importDatasetApi}}
returns objects of class \code{niDataImport} after reading the data from the
NI database via the
[The Nature Index Application Programming Interface](https://www8.nina.no/NaturindeksNiCalc/index.html).
}
\seealso{
Function \code{\link{importDatasetApi}}. The vignette \code{objectsInNIcalc}
gives a more detailed description of \code{niDataImport} lists.
}
\author{
Bård Pedersen
}
