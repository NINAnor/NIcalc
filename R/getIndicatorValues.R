getIndicatorValues <- function(IndicatorID = NULL, token = niToken){

  url <- "http://ninweb17.nina.no"
  value_path <- paste0("NaturindeksAPI/api/indicator/", IndicatorID, "/values")

  auth_string <- paste("bearer", token, sep = " ")

  rawResult <- GET(url = url,
                   path = value_path,
                   encode = "json",
                   add_headers(Authorization = auth_string))

  rawContent <- httr::content(rawResult, as = "text")

  rawResult <- jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE)

  indicatorData <- list("indicatorValues" = rawResult[!(names(rawResult) %in% c("customDistributionObject"))])


  customDistributions <- as.list(rawResult$customDistributionObject[!is.na(rawResult$customDistributionObject)])

  names(customDistributions) <- rawResult$customDistributionUUID[!is.na(rawResult$customDistributionUUID)]


  backToDist <- function(x){
    unserialize(charToRaw(x[[1]]))
  }

  indicatorData$customDistributions <- lapply(customDistributions, backToDist)

  return(indicatorData)

}
