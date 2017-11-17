getToken <- function(username = NULL, password = NULL){
  url <- "http://ninweb17.nina.no"
  token_path <- paste0("NaturindeksAPI/token?username=", username,"&password=", password)
  api_path <- "NaturindeksAPI/api/indicator"

  gettoken <- POST(url = url, path = token_path, encode = "json")
  token <- content(gettoken)
  return(token)
  #in package, do something like assign(token, niToken, envir = NIcalc)
}
