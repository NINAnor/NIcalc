#' Write indicator values
#'
#' This function writes updated values for a given indicator to the Nature Index
#' database via the Nature Index API.
#'
#' Functions \code{\link{getIndicatorValues}}, \code{\link{setIndicatorValues}},
#' and \code{writeIndicatorValues} are used in series to update the set of
#' values for an indicator. After retrieving the current set of values from the
#' database using \code{getIndicatorValues}, and updating the set with
#' \code{setIndicatorValues}, \code{writeIndicatorValues} may be used to post the
#' updated set to the database.
#'
#' @seealso \code{\link{getIndicatorValues}}, \code{\link{setIndicatorValues}}
#'
#' @name writeIndicatorValues
#' @author Jens Åström
#'
#' @param indicatorData Data frame of indicator values. Usually format retrieved
#' from \code{getIndicatorValues} and updated through \code{setIndicatorValues}.
#'
#' @return If posting the data fails, an error message is returned.
#'
#' @examples
#' \dontrun{
#' getToken()
#' myIndicators <- getIndicators()
#' myValues <- getIndicatorValues(indicatorID = 359)
#' updatedValues <- setIndicatorValues(myValues, year = 2019, est = 0.3)
#' writeIndicatorValues(updatedValues)
#' }

writeIndicatorValues <- function (indicatorData = NULL){
  url = NIcalc:::.getUrl()
  token = NIcalc:::.getToken()
  auth_string <- paste("bearer", token, sep = " ")
  api_path <- "/indicators/values"
  combinedUrl <- paste0(url, api_path)

  httr::set_config(httr::config(ssl_verifypeer = 0L))

  distToRaw <- function(x) {
    rawToChar(serialize(x, NULL, ascii = T))
  } #writes dist object as text

  # function to adapt the %in% to include order
  '%ino%' <- function(x, table) {
    xSeq <- seq(along = x)
    names(xSeq) <- x
    Out <- xSeq[as.character(table)]
    Out[!is.na(Out)]
  }

  indicatorData$indicatorValues$customDistributionObject <- NA
  rawDist <- lapply(indicatorData$customDistributions, distToRaw) # doesn't secure order of distribution objects is correct
  ####problem is here
  indicatorData$indicatorValues$customDistributionObject[indicatorData$indicatorValues$customDistributionUUID %in%
                                                           names(rawDist)] <-
    unlist(rawDist[names(rawDist) %ino% indicatorData$indicatorValues$customDistributionUUID]) # in order of customdist list and not the order in the dataframe
  body <- indicatorData$indicatorValues

  postdata <- httr::POST(url = combinedUrl, body = body, encode = "json",
                         httr::add_headers(Authorization = auth_string))
  message(httr::content(postdata))

  if (length(httr::content(postdata)) == 0) {
    message("Values NOT stored!")}
}
