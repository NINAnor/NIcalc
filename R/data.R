#' Example of an indicatorData set.
#'
#' A dataset containing observations of the `Dagsommerfugler` indicator
#' as an example of an object of class \code{indicatorData}.
#'
#' @format An object of class \code{indicatorData}  with two elements, a list containing
#' 1) a data frame \code{indicatorData$indicatorValues} with 30 rows and the following
#' variables
#' \describe{
#'   \item{indicatorId}{integer}
#'   \item{indicatorName}{character}
#'   \item{areaId}{integer}
#'   \item{areaName}{character}
#'   \item{yearId}{integer}
#'   \item{yearName}{character}
#'   \item{verdi}{numeric, the expected value of the observation}
#'   \item{nedre_Kvartil}{numeric, the lower quartile of the observation}
#'   \item{ovre_Kvartil}{numeric, the upper quartile of the observation}
#'   \item{datatypeId}{integer}
#'   \item{datatypeName}{character, type of observation}
#'   \item{unitOfMeasurement}{character}
#'   \item{customDistributionUUID}{character, id of corresponding distribution object}
#'   \item{distributionName}{character, name of fitted distribution}
#'   \item{distributionId}{integer}
#'   \item{distParam1}{numeric, first parameter of fitted distribution}
#'   \item{distParam2}{numeric, second parameter of fitted distribution}
#' }
#' 2) a list of distribution objects identifying the uncertainties of the estimates (optional)
#'
#' @seealso \code{\link{setIndicatorValues}}, and \code{\link{getIndicatorValues}}
"indicatorData"
#' Example of a thematic index data set.
#'
#' A constructed data set covering four "carnivore" indicators
#' as an example of data sets of class \code{niInput}.
#'
#' @format An object of class \code{niInput}  with the following six elements
#' \describe{
#'   \item{indicators}{data.frame with indicator data}
#'   \item{ICunits}{integer BSunit x indicator matrix of ICunits}
#'   \item{BSunits}{data.frame of BSunit data}
#'   \item{referenceValues}{data.frame of reference values}
#'   \item{indicatorValues}{list of data.frames of indicator observations}
#'   \item{NIunits}{0/1 BSunit x NIunit matrix of NIunits}
#' }
#' @seealso \code{\link{niInput}}. \cr The vignette \code{objectsInNIcalc} gives a
#' detailed description of \code{\link{niInput}} lists.
"themeData"
