#' Print function for class indicatorData objects
#'
#' Simple print function that displays also the custom distributions.
#'
#' @name print.indicatorData
#' @author  Jens Åström
#' @param input object of class `indicatorData`
#' @return Vector of samples
#' @export
#' @examples
#' myDist <- makeCustomUncertainty(input = "Normal", distParams = list("mean" = 1, "sd" = 0.2))
#' sampleNIDistribution(myDist, 10)
#'
#'
#'
#' @seealso \code{\link{makeCustomDistribution}}


print.indicatorData <- function(input){
  table <- input[["IndicatorValues"]]
  table$DistributionType <- input[["DistributionType"]]

  print(table)
}
