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
#' @name setIndicatorValues
#' @author  Jens Åström
#' @param indicatorData object of class `indicatorData` created by \code{getIndicatorData}
#' @param indicatorID Identifier of the indicator to be altered
#' @param est Point estimate for the indicator. Optional.
#' @param lower lower quartile of estimate. Required when supplying point estimate.
#' @param upper upper quartile of estimate. Required when supplying point estimate.
#' @param distribution distribution type or empirical values to be passed to \code{makeCustomUncertainty}.
#' @param distrParams distribution parameters to be passed to \code{makeDistribution}. Required when
#' using a named distribution
#' @return Vector of samples
#' @export
#' @examples
#' getToken("testUser", "testPassword")
#' indicatorData <- getIndicators()
#' setIndicatorValues(indicatorID = 1,
#' est = 0.5,
#' lower = 0.2
#' upper = 0.7
#' )
#'
#' setIndicatorValues(indicatorID = 2
#' distribution = "Normal",
#' distParams = list("mean" = 0.5,
#'                   "sd" = 0.1)
#')
#'
#'
#' @seealso \code{\link{makeDistribution}}


setIndicatorValues <- function(indicatorData = NULL,
                               areaId = NULL,
                               years = NULL,
                               est = NULL,
                               lower = NULL,
                               upper = NULL,
                               distribution = NULL,
                               distParams = NULL){

  if(!("indicatorData" %in% class(indicatorData))) stop("indicatorData needs to be of class \"indicatorData\". Use function \"getIndicatorData\" to retreive or create such an object")

  rows <- 1:nrow(indicatorData$indicatorValues)
  if(!is.null(areaId)){
    rows <- rows[indicatorData$indicatorValues$areaId[rows] %in% areaId]
  }

  if(!is.null(years)){
    rows <- rows[indicatorData$indicatorValues$yearName[rows] %in% years]
  }


  if(!is.null(distribution)){
    distID <- uuid::UUIDgenerate()
    dist <- makeDistribution(input = distribution, distParams = distParams)

    if(class(dist) == "logNormal"){
      est <- distr::meanlog(dist)
    } else  est <- dist@q(0.5)


    indicatorData$indicatorValues[rows, "verdi"] <- est

    indicatorData$indicatorValues[rows, "customDistributionUUID"] <- distID

    indicatorData$customDistributions[[distID]] <- dist

    return(indicatorData)
  } else {

    indicatorData$indicatorValues[rows, "verdi"] <- est
    indicatorData$indicatorValues[rows, "nedre_Kvartil"] <- lower
    indicatorData$indicatorValues[rows, "ovre_Kvartil"] <- upper

    return(indicatorData)
  }

}
