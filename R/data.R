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

#' @title BSunits
#' @description DATASET_DESCRIPTION
#' @format A data frame with 4 rows and 5 variables:
#' \describe{
#'   \item{\code{BasicunitID}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Basicunit}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Area}}{double COLUMN_DESCRIPTION}
#'   \item{\code{NIarea1}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{NIarea2}}{integer COLUMN_DESCRIPTION}
#'}
#' @source \url{http://somewhere.important.com/}
"BSunits"

#' @title Indic
#' @description DATASET_DESCRIPTION
#' @format A data frame with 33 rows and 6 variables:
#' \describe{
#'   \item{\code{IndicatorID}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Indicator_name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{TrophicgroupID}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Scalingmodel}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Key.indicators}}{logical COLUMN_DESCRIPTION}
#'   \item{\code{Fidelity}}{double COLUMN_DESCRIPTION}
#'}
#' @source \url{http://somewhere.important.com/}
"Indic"
#' @title Indicator.area.ind
#' @description DATASET_DESCRIPTION
#' @format A data frame with 59 rows and 3 variables:
#' \describe{
#'   \item{\code{IndicatorareaID}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{IndicatorID}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Indicator_name}}{integer COLUMN_DESCRIPTION}
#'}
#' @source \url{http://somewhere.important.com/}
"Indicator.area.ind"

#' @title Indicator.area
#' @description DATASET_DESCRIPTION
#' @format A data frame with 67 rows and 3 variables:
#' \describe{
#'   \item{\code{IndicatorareaID}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{BasicsunitID}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Basicunit}}{integer COLUMN_DESCRIPTION}
#'}
#' @source \url{http://somewhere.important.com/}
"Indicator.area"

#' @title Observations
#' @description DATASET_DESCRIPTION
#' @format A data frame with 295 rows and 7 variables:
#' \describe{
#'   \item{\code{ValueID}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{IndicatorID}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Expected.value}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Lower}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Upper}}{double COLUMN_DESCRIPTION}
#'   \item{\code{ReferenceYearID}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{IndicatorareaID}}{integer COLUMN_DESCRIPTION}
#'}
#' @source \url{http://somewhere.important.com/}
"Observations"
