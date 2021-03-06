% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeDistribution.R
\encoding{UTF-8}
\name{makeDistribution}
\alias{makeDistribution}
\title{Create a distribution function to describe the uncertainty for an indicator}
\usage{
makeDistribution(input = NULL, distParams = NULL)
}
\arguments{
\item{input}{Either "logNormal", "Poisson", a vector of values, or a data frame of possible values and value probabilities. See examples.}

\item{distParams}{(optional) Parameters for the distribution function, if such is provided in `input`. See examples.}
}
\value{
an object of class `NIdistribution`
}
\description{
This functions formats various representations of indicator uncertainty into a common structure for further processing
}
\examples{
myDist <- makeDistribution(input = "logNormal", distParams = list("mean" = 1, "sd" = 0.2))
sampleDistribution(myDist, 10)

myDist <- makeDistribution(input = "Poisson", distParams = list("lambda" = 3))
sampleDistribution(myDist, 10)

myProbs <- data.frame("est" = c(0.2, 0.23, 0.34, 0.4), "probs" = c(0.1, 0.4, 0.4, 0.1 ))
myDist <- makeDistribution(myProbs)
sampleDistribution(myDist, 10)

codaSamples <- rnorm(1000, mean = 0.87, sd = 0.3)
myDist <- makeDistribution(codaSamples)
sampleDistribution(myDist, 10)


}
\seealso{
\code{\link{sampleDistribution}}
The vignette \code{Distributions} gives detailed descriptions of how to use
\code{makeDistribution} to generate distribution objects when revising and
updating the data set for an indicator.
}
\author{
Jens Åström
}
