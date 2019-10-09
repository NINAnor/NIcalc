#' Get the present indicator values from the Nature Index database
#'
#' This function retreives the current values for a given indicator. It is used both to check the current
#' information, and as a starting point for updating the data. In case of updates, individual rows are altered
#' by the function setIndicatorValue.
#'
#' @param indicatorData	Data frame of indicator values. Usually format retrieved from \link{getIndicatorValues} and updated through \link{setIndicatorValues}.
#' @param token A connection token aquired by \link{getToken}. Simply leave as is if you use `getToken`
#' @return A message from the database API of the turn out of the action.
#'
#' @author Jens Åström
#'
#' @section Notes:
#'
#'
#' @examples
#' \dontrun{
#' getToken()
#' myIndicators <- getIndicators()
#' myValues <- getIndicatorValues(IndicatorID = 1)
#' updatedValues <- setIndicatorValues(myValues, year = 2018, est = 0.3)
#' writeIndicatorValues(updatedValues)
#' }
#'
#' @export
#'


writeIndicatorValues <- function(indicatorData = NULL){

  url = NIcalc:::.getUrl()
  token = NIcalc:::.getToken()

  auth_string <- paste("bearer", token, sep = " ")
  api_path <- "/indicators/values"
  combinedUrl <- paste0(url, api_path)

  httr::set_config(httr::config(ssl_verifypeer = 0L)) #Fix "Peer certificate error"

  distToRaw <- function(x){
    rawToChar(serialize(x, NULL, ascii = T))
  }

  indicatorData$indicatorValues$customDistributionObject <- NA
  rawDist <- lapply(indicatorData$customDistributions, distToRaw)
  indicatorData$indicatorValues$customDistributionObject[indicatorData$indicatorValues$customDistributionUUID %in% names(rawDist)] <- unlist(rawDist[names(rawDist) %in% indicatorData$indicatorValues$customDistributionUUID])


  body <- indicatorData$indicatorValues

  postdata <- httr::POST(url = combinedUrl,
                   body = body,
                   encode = "json",
                   httr::add_headers(Authorization = auth_string))


  cat(httr::content(postdata)[[1]])
}
