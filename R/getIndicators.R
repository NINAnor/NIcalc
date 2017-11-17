getIndicators <- function(token = niToken){
  url <- "http://ninweb17.nina.no"
  indicator_path <- "NaturindeksAPI/api/indicators"

  auth_string <- paste("bearer", token, sep = " ")

  myIndicators <- GET(url = url,
                      path = indicator_path,
                      encode = "json",
                      add_headers(Authorization = auth_string))

  rawContent <- httr::content(myIndicators)

  indicators <- plyr::ldply(rawContent, data.frame)

  #indicators <- do.call(rbind, lapply(rawContent, data.frame, stringsAsFactors=FALSE))

  return(indicators)
}
