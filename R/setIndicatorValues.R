#' Set indicator values in IndicatorData object
#'
#' This function fills info into an indicatorData object which can later be uploaded to the database.
#' Usually, this object is first retreived from the database through the function \code{getIndicatorData}.
#' This object will list all indicators that a user is privileged to alter. Data for a particular indicator
#' is accepted either in the form of an estimate together with an lower and upper quartile of uncertainty,
#' or as an input to the function \code{makeDistribution}.
#'
#' In this case, data can be supplied either in the form of a vector of possible values (e.g. posterior samples),
#' a matrix or dataframe of possible values together with probabilities for each value, or as a named
#' known distribution. In case of named known distributions, parameter for these distributions is supplied
#' in distParams as a named list. See \link{makeDistribution} for further details.
#'
#' @name setIndicatorValues
#' @author  Jens Åström
#' @param indicatorData Object of class `indicatorData` created by \code{getIndicatorData}.
#' @param areaID Numeric. Identifier of the indicator to be altered.
#' @param years Numeric. Which year to set values for.
#' @param est Numeric. Point estimate for the indicator. Optional.
#' @param lower Numeric. Lower quartile of estimate. Required when supplying point estimate.
#' @param upper Numeric. Upper quartile of estimate. Required when supplying point estimate.
#' @param distribution Either "logNormal", "Poisson", a vector of values, or a data frame of possible values and value probabilities, to be passed to \code{makeDistribution}. See examples therein.
#' @param distrParams Distribution parameters to be passed to \code{makeDistribution}. Required when
#' using a named distribution. See examples in \code{makeDistribution}
#' @param datatype Type of indicator. Remember to update this when the type changes. Allowed values: 1 = Ekspertvurdering, 2 = Overvåkingsdata, 3 = Beregnet fra modeller. Defaults to 1.
#' @param unitOfMeasurement Text of maximum length 100. Defaults to "Enhetsløs".
#' @return Object of class `indicatorData`.
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
                               lower = NA,
                               upper = NA,
                               distribution = NULL,
                               distParams = NULL,
                               datatype = 1,
                               unitOfMeasurement = "Enhetsløs"){

  if(!("indicatorData" %in% class(indicatorData))) stop("indicatorData needs to be of class \"indicatorData\". Use function \"getIndicatorData\" to retreive or create such an object")

  if(nchar(unitOfMeasurement) > 100) stop("unitOfMeasurement can only be 100 characters long.")

  if(!(datatype %in% 1:3)) stop("Datatype needs to be 1, 2, or 3.")


  datatypeName <- c("Ekspervurdering", "Overvåkingsdata", "Beregnet fra modeller")[datatype]

  rows <- 1:nrow(indicatorData$indicatorValues)
  if(!is.null(areaId)){
    rows <- rows[indicatorData$indicatorValues$areaId[rows] %in% areaId]
  }

  if(!is.null(years)){
    rows <- rows[indicatorData$indicatorValues$yearName[rows] %in% years]
  }


  if(!is.null(distribution)){
    if(attr(class(distribution), "package") != "distr") stop("Distribution needs to be a distribution object made from the 'makeDistribution' function")
    distID <- uuid::UUIDgenerate()
    dist <- distribution

    if(class(dist) == "Lnorm"){
      est <- logNormal2normal(distr::meanlog(dist), distr::sdlog(dist))
    }

    if(class(dist) == "Pois"){
      est <- distr::lambda(dist)
    }

    else  est <- mean(sampleDistribution(dist, 1e5))


    indicatorData$indicatorValues[rows, "verdi"] <- est
    indicatorData$indicatorValues[rows, "customDistributionUUID"] <- distID
    indicatorData$customDistributions[[distID]] <- dist

    indicatorData$indicatorValues[rows, "nedre_Kvartil"] <- NA
    indicatorData$indicatorValues[rows, "ovre_Kvartil"] <- NA
    indicatorData$indicatorValues[rows, "distributionName"] <- NA
    indicatorData$indicatorValues[rows, "distributionID"] <- NA

    indicatorData$indicatorValues[rows, "datatypeId"] <- datatype
    indicatorData$indicatorValues[rows, "datatypeName"] <- datatypeName

    ##Remove custom distributions not referenced in table
    presentIDs <- indicatorData$indicatorValues[, "customDistributionUUID"]
    indicatorData$customDistributions <- indicatorData$customDistributions[names(indicatorData$customDistributions) %in% presentIDs]

    } else {

    indicatorData$indicatorValues[rows, "verdi"] <- est
    indicatorData$indicatorValues[rows, "nedre_Kvartil"] <- lower
    indicatorData$indicatorValues[rows, "ovre_Kvartil"] <- upper

    indicatorData$indicatorValues[rows, "datatypeId"] <- datatype
    indicatorData$indicatorValues[rows, "datatypeName"] <- datatypeName
    indicatorData$indicatorValues[rows, "distributionName"] <- NA
    indicatorData$indicatorValues[rows, "distributionID"] <- NA


    }

  if(nrow(indicatorData$indicatorValues[indicatorData$indicatorValues$yearName == "Referanseverdi", c("nedre_Kvartil", "ovre_Kvartil")]) >0){
    if(any(!is.na(indicatorData$indicatorValues[indicatorData$indicatorValues$yearName == "Referanseverdi", c("nedre_Kvartil", "ovre_Kvartil")]))) {
   indicatorData$indicatorValues[indicatorData$indicatorValues$yearName == "Referanseverdi", c("nedre_Kvartil", "ovre_Kvartil")] <- NA
   message("Reference value upper and lower quartiles changed to NAs")
    }
  }
  return(indicatorData)

}


