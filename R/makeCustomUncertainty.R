#' Create a distribution function to describe the uncertainty for an indicator
#'
#' This functions formats various representations of indicator uncertainty into a common structure for further processing
#'
#'
#' @name makeCustomUncertainty
#' @author Jens Åström
#' @param input Either a name of a known distribution, a vector of values, or a data frame of possibleValues and valueProbabilities
#' @param functionParams (optional) Parameters for the distribution function, if such is provided in `input`
#' @return an object of class `NIdistribution`
#' @export
#' @examples
#'
#'  makeCustomUncertainty(dnorm)
#'
#' @seealso \code{\link{sample.NIdistribution}}


makeCustomUncertainty <- function(input = NULL, functionParams = NULL){



if (!requireNamespace("distr", quietly = TRUE)) {
  stop("Pkg 'distr' needed for this function to work. Please install it.",
       call. = FALSE)
}



}
