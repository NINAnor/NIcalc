#' Set indicator values in IndicatorData object
#'
#' This function fills info into an indicatorData object which can later be uploaded to the database.
#' Usually, this object is first retreived from the database through the function \code{getIndicatorData}.
#' This object will list all indicators that a user is privileged to alter. Data for a particular indicator
#' is accepted either in the form of an estimate together with an lower and upper quartile of uncertainty,
#' or as an input to the function \code{makeCustomUncertainty}.
#'
#' In this case, data can be supplied either in the form of a vector of possible values (e.g. posterior samples),
#' a matrix or dataframe of possible values together with probabilities for each value, or as a named
#' known distribution. In case of named known distributions, parameter for these distributions is supplied
#' in distParams as a named list. See \link{makeCustomUncertainty} for further details.
#'
#' @name setIndicator
#' @author  Jens Åström
#' @param indicatorData object of class `indicatorData` created by \code{getIndicatorData}
#' @param indicatorID Identifier of the indicator to be altered
#' @param est Point estimate for the indicator. Optional.
#' @param lower lower quartile of estimate. Required when supplying point estimate.
#' @param upper upper quartile of estimate. Required when supplying point estimate.
#' @param distribution distribution type or empirical values to be passed to \code{makeCustomUncertainty}.
#' @param distrParams distribution parameters to be passed to \code{makeCustomUncertainty}. Required then
#' using a named distribution
#' @return Vector of samples
#' @export
#' @examples
#' indicatorData <- getIndicators()
#' setIndicator(indicatorID = 1,
#' est = 0.5,
#' lower = 0.2
#' upper = 0.7
#' )
#'
#' setIndicators(indicatorID = 2
#' distribution = "Normal",
#' distParams = list("mean" = 0.5,
#'                   "sd" = 0.1)
#')
#'
#'
#' @seealso \code{\link{makeCustomDistribution}}


setIndicator <- function(indicatorData = indicatorData,
                         indicatorID,
                         est = NULL,
                         lower = NULL,
                         upper = NULL,
                         distribution = NULL,
                         distParams = NULL){

  if(class(indicatorData) != "indicatorData") stop("indicatorData needs to be of class \"indicatorData\". Use function \"getIndicatorData\" to retreive or create such an object")

  if(!is.null(distribution)){
    distID <- UUIDgenerate()
    dist <- makeCustomUncertainty(input = distribution, distParams = distParams)
    est <- mean(dist)

    indicatorData$IndicatorValues[indicatorID, "est"] <- est
    indicatorData$IndicatorValues[indicatorID, "have_custom_distribution"] <- T
    indicatorData$IndicatorValues[indicatorID, "custom_distribution"] <- distID

    indicatorData$Distributions[distID] <- serialize(dist, NULL)

    indicatorData$DistributionType[distID] <- paste(capture.output(print(dist)), collapse = "")

    return(indicatorData)
  } else {

    indicatorData[indicatorData$indicatorID == indicatorID, "est"] <- est
    indicatorData[indicatorData$indicatorID == indicatorID, "lower"] <- lower
    indicatorData[indicatorData$indicatorID == indicatorID, "upper"] <- upper
    indicatorData[indicatorData$indicatorID == indicatorID, "have_custom_distribution"] <- F
    return(indicatorData)
  }

}
