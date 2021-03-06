#' Create a distribution function to describe the uncertainty for an indicator
#'
#' This functions formats various representations of indicator uncertainty into a common structure for further processing
#'
#' @encoding UTF-8
#' @author Jens Åström
#' @name makeDistribution
#'
#' @importFrom  distr r
#' @importFrom distr meanlog
#' @importFrom distr lambda
#'
#' @param input Either "logNormal", "Poisson", a vector of values, or a data frame of possible values and value probabilities. See examples.
#' @param distParams (optional) Parameters for the distribution function, if such is provided in `input`. See examples.
#' @return an object of class `NIdistribution`
#'
#' @export
#' @examples
#' myDist <- makeDistribution(input = "logNormal", distParams = list("mean" = 1, "sd" = 0.2))
#' sampleDistribution(myDist, 10)
#'
#' myDist <- makeDistribution(input = "Poisson", distParams = list("lambda" = 3))
#' sampleDistribution(myDist, 10)
#'
#' myProbs <- data.frame("est" = c(0.2, 0.23, 0.34, 0.4), "probs" = c(0.1, 0.4, 0.4, 0.1 ))
#' myDist <- makeDistribution(myProbs)
#' sampleDistribution(myDist, 10)
#'
#' codaSamples <- rnorm(1000, mean = 0.87, sd = 0.3)
#' myDist <- makeDistribution(codaSamples)
#' sampleDistribution(myDist, 10)
#'
#'
#' @seealso \code{\link{sampleDistribution}}
#' The vignette \code{Distributions} gives detailed descriptions of how to use
#' \code{makeDistribution} to generate distribution objects when revising and
#' updating the data set for an indicator.


makeDistribution <- function(input = NULL, distParams = NULL){

  allowedDistributions <- c("logNormal", "Poisson")

  errorMSG <- paste("Input needs to be either",
                    allowedDistributions[1], "or",
                    allowedDistributions[2], "with appropriate parameters in \"distParams\" or",
                    "a data frame or matrix of discrete allowed values with probabilities",
                    "or a vector of samples (for example CODA samples)")

  if (!requireNamespace("distr", quietly = TRUE)) {
    stop("Pkg 'distr' needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if ("character" %in% class(input)){
    namedDist = match.arg(input, allowedDistributions)

    dist <- switch(namedDist,
                   logNormal = distr::Lnorm(distParams$mean, distParams$sd),
                   Poisson = distr::Pois(distParams$lambda)
    )
  } else

    if ("data.frame" %in% class(input) | "matrix" %in% class(input)){
      dist <- distr::DiscreteDistribution(input[,1], input[,2])
    } else

      if (is.numeric(input)){
        dist <- distr::EmpiricalDistribution(input)
      } else stop(errorMSG)


  return(dist)

}


