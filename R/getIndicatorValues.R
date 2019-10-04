#' Get the present indicator values from the Nature Index database
#'
#' This function retreives the current values for a given indicator. It is used both to check the current
#' information, and as a starting point for updating the data. In case of updates, individual rows are altered
#' by the function setIndicatorValue.
#'
#' @param indicatorID	numerical	a numerical value identifying the indicator. Usually this is first retreived by the \link{getIndicators} function
#' @param year numerica the year of the values to retrieve. Default = NULL, which means you get all values.
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



getIndicatorValues <- function(indicatorID = NULL, years = NULL){

  url = NIcalc:::.getUrl()
  token = NIcalc:::.getToken()

  if(!exists("token")) stop("No connection. Connect to database using 'getToken()' first.")

  value_path <- paste0("NaturindeksAPI/indicators/", indicatorID, "/values")

  auth_string <- paste("bearer", token, sep = " ")

  httr::set_config(httr::config(ssl_verifypeer = 0L)) #Fix "Peer certificate error"


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


  if(!is.null(years)){
   out$indicatorValues <- out$indicatorValues[out$indicatorValues$yearName %in% years,]
   ##Remove custom distributions not referenced in table
   presentIDs <- out$indicatorValues[, "customDistributionUUID"]
   out$customDistributions <- out$customDistributions[names(out$customDistributions) %in% presentIDs]
   }

  return(out)

}
