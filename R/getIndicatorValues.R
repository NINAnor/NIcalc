#' Get the present indicator values from the Nature Index database
#'
#' This function retreives the current values for a given indicator. It is used both to check the current
#' information, and as a starting point for updating the data. In case of updates, individual rows are altered
#' by the function setIndicatorValue.
#'
#' @param indicatorID	numerical	a numerical value identifying the indicator. Usually this is first retreived by the \link{getIndicators} function
#' @return A list containing 1) a data frame of the values of the indicator and 2) a list of distribution objects identifying the uncertainties of the estimates (optional)
#'
#' @author Jens Åström
#'
#' @section Notes:
#'
#'
#' @examples
#' \dontrun{
#' myValues <- getIndicatorValues(IndicatorID = 1)
#' }
#'
#' @export
#'



getIndicatorValues <- function(indicatorID = NULL, token = niToken){

  url <- "http://ninweb17.nina.no"
  value_path <- paste0("NaturindeksAPI/api/indicator/", indicatorID, "/values")

  auth_string <- paste("bearer", token, sep = " ")

  rawResult <- httr::GET(url = url,
                   path = value_path,
                   encode = "json",
                   httr::add_headers(Authorization = auth_string))

  rawContent <- httr::content(rawResult, as = "text")

  rawResult <- jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE)

  out <- list("indicatorValues" = rawResult[!(names(rawResult) %in% c("customDistributionObject"))])

  customDistributions <- as.list(rawResult$customDistributionObject[!is.na(rawResult$customDistributionObject)])
  names(customDistributions) <- rawResult$customDistributionUUID[!is.na(rawResult$customDistributionUUID)]

  backToDist <- function(x){
    unserialize(charToRaw(x[[1]]))
  }

  out$customDistributions <- lapply(customDistributions, backToDist)

  class(out) <- c("indicatorData", "list")

  return(out)

}
