#' Create a distribution function to describe the uncertainty for an indicator
#'
#' This functions formats various representations of indicator uncertainty into a common structure for further processing
#'
#'
#' @name makeDistribution
#' @author Jens Åström
#' @param input Either a name of a known distribution, a vector of values, or a data frame of possibleValues and valueProbabilities
#' @param distParams (optional) Parameters for the distribution function, if such is provided in `input`
#' @return an object of class `NIdistribution`
#' @export
#' @examples
#' myDist <- makeCustomUncertainty(input = "logNormal", distParams = list("mean" = 1, "sd" = 0.2))
#' sampleNIDistribution(myDist, 10)
#'
#' myDist <- makeCustomUncertainty(input = "Poisson", distParams = list("lambda" = 3))
#' sampleNIDistribution(myDist, 10)
#'
#' myProbs <- data.frame("est" = c(0.2, 0.23, 0.34, 0.4), "probs" = c(0.1, 0.4, 0.4, 0.1 ))
#' myDist <- makeCustomUncertainty(myProbs)
#' sampleNIDistribution(myDist, 10)
#'
#' codaSamples <- rnorm(1000, mean = 0.87, sd = 0.3)
#' myDist <- makeCustomUncertainty(codaSamples)
#' sampleNIDistribution(myDist, 10)
#'
#'
#' @seealso \code{\link{sample.NIdistribution}}


makeDistribution <- function(input = NULL, distParams = NULL){

allowedDistributions <- c("logNormal", "Poisson")

errorMSG <- paste("Input needs to be either",
      allowedDistributions[1], "or",
      allowedDistributions[2], "with appropriate parameters in \"functionParams\" or",
      "a data frame or matrix of discrete allowed values with probabilities",
      "or a vector of samples (for example CODA samples)")

if (!requireNamespace("distr", quietly = TRUE)) {
  stop("Pkg 'distr' needed for this function to work. Please install it.",
       call. = FALSE)
}

if (class(input) == "character"){
  namedDist = match.arg(input, allowedDistributions)

  dist <- switch(namedDist,
                 logNormal = distr::Lnorm(distParams$mean, distParams$sd),
                 Poisson = distr::Pois(distParams$lambda)
    )
} else

if (class(input) == "data.frame" | class(input) == "matrix"){
  dist <- distr::DiscreteDistribution(input[,1], input[,2])
} else

if (is.numeric(input)){
  dist <- distr::EmpiricalDistribution(input)
} else stop(errorMSG)


return(dist)

}


