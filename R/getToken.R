#' Get a Connection Token from the NI Database
#'
#' This function tries to connect to the NI database API and retrieves a token that is later used to
#' communicate with the database.
#'
#' @encoding UTF-8
#' @author Jens Åström
#'
#' @import httr
#'
#' @param username	character		Your username
#' @param password	character		Your password
#' @param url	      character		API address.
#' @return The function sets the variables niToken and url, which both are used in other functions that
#' communicate with the database through the API.
#'
#' @examples
#' \dontrun{
#' getToken("test.user", "test.password")
#' }
#'
#' @export
#'

getToken <- function (username = NULL, password = NULL, url = "https://www8.nina.no/NaturindeksNiCalc")
{
  token_path <- paste0("/token?username=", username,
                       "&password=", password)
  combinedUrl <- paste0(url, token_path)
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  gettoken <- httr::POST(url = combinedUrl, encode = "json")
  token <- httr::content(gettoken, encoding = "UTF-8")
  if (exists("token")) {
    if (any(class(token) != "character"))
      stop(paste0("Token NOT retrieved from ", url,
                  " !"))
    if (nchar(token) < 30) {
      stop(paste0("Token NOT retrieved from ", url,
                  " !"))
    }
    else message(paste0("Token retrieved from ", url,
                        " !"))
  }
  else stop(paste0("Token NOT retrieved from ", url,
                   " !"))

  pos <- 1
  envir = as.environment(pos)

  assign(".niToken", token, envir = envir) #envir = .GlobalEnv) #, envir = passEnv)
  assign(".url", url, envir = envir) #envir = .GlobalEnv) #, envir = passEnv)
}
