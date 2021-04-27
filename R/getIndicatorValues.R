#' Get indicator values from the Nature Index database
#'
#' This function retrieves the current values for a given indicator. It is used both to check the current
#' information, and as a starting point for updating the data. In case of updates, individual rows are altered
#' by the function \code{setIndicatorValue}.
#'
#' @encoding UTF-8
#' @author Jens Åström
#' @name getIndicatorValues
#'
#' @import httr
#'
#' @param indicatorID	integer	identifying the indicator.
#' Usually this is first retrieved by the \code{\link{getIndicators}} function
#' @param years integer,	the year of the values to retrieve. Default = NULL, which means you get all values.
#'
#' @return Object of class `indicatorData`, A list containing
#' 1) a data frame of the values of the indicator and
#' 2) a list of distribution objects identifying the uncertainties of the estimates (optional)
#'
#' @seealso \code{\link{setIndicatorValues}} for updating objects of class `indicatorData`,
#' and \code{\link{getIndicators}} for list of accessible indicators.
#' The vignette \code{Distributions} describes the use of these and other functions for
#' updating data sets.
#'
#' @examples
#' \dontrun{
#' myValues <- getIndicatorValues(IndicatorID = 1)
#' }
#'
#' @export
#'


getIndicatorValues <- function (indicatorID = NULL, years = NULL) {
  url = .url
  token = .niToken

  if (!exists("token"))
    stop("No connection. Connect to database using 'getToken()' first.")

  value_path <- paste0(url, "/indicators/", indicatorID,
                       "/values")
  auth_string <- paste("bearer", token, sep = " ")

  httr::set_config(httr::config(ssl_verifypeer = 0L))

  rawResult <- httr::GET(url = value_path, encode = "json",
                         httr::add_headers(Authorization = auth_string))

  rawContent <- httr::content(rawResult, as = "text")

  rawResult <- jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE)

  out <- list(indicatorValues = rawResult[!(names(rawResult) %in%
                                              c("customDistributionObject"))])

  customDistributions <- as.list(rawResult$customDistributionObject[!is.na(rawResult$customDistributionObject)])
  names(customDistributions) <- rawResult$customDistributionUUID[!is.na(rawResult$customDistributionUUID)]

  backToDist <- function(x) {
    unserialize(charToRaw(x[[1]]))
  }

  out$customDistributions <- lapply(customDistributions, backToDist)
  class(out) <- c("indicatorData", "list")

  if (!is.null(years)) {
    out$indicatorValues <- out$indicatorValues[out$indicatorValues$yearName %in%
                                                 years, ]
    presentIDs <- out$indicatorValues[, "customDistributionUUID"]
    out$customDistributions <- out$customDistributions[names(out$customDistributions) %in%
                                                         presentIDs]
  }
  return(out)
}
