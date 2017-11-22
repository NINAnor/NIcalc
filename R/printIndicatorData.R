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


print.niIndicatorData <- function(input){
  table = input
  # table <- input[["IndicatorValues"]]
  #
  # getDistType <- function(x){
  #   out <- paste(capture.output(print(x)), collapse = "")
  #   return(out)
  # }
  #
  # table$DistributionType <- lapply(input$customDistributions, function(x) getDistType(x))
  # table$DistributionType <- input[["DistributionType"]]

  print(table)
}
