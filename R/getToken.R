#' Get a connection Token from the NI database
#'
#' This function tries to connect to the NI database API and retreives a token that is later used to
#' communicate with the database.
#'
#' @param username	character		Your username
#' @param password	character		Your password
#'
#' @return The function sets the variable niToken, which is used in other database functions
#'
#' @author Jens Åström
#'
#' @section Notes:
#'
#'
#' @examples
#' \dontrun{
#' getToken("test.user", "test.password")
#' }
#'
#' @export
#'


getToken <- function(username = NULL,
                     password = NULL,
                     url = "https://www8.nina.no/NaturindeksNiCalc"){


  token_path <- paste0("/token?username=", username,"&password=", password)
  combinedUrl <- paste0(url, token_path)

  httr::set_config(httr::config(ssl_verifypeer = 0L)) #Fix "Peer certificate error"


  gettoken <- httr::POST(url = combinedUrl, encode = "json")
  token <- httr::content(gettoken, encoding = "UTF-8")



  if(exists("token")){
    if(any(class(token) != "character")) stop(paste0("Token NOT retrieved from ", url, " !"))
      if(nchar(token) < 30){
        stop(paste0("Token NOT retrieved from ", url, " !"))
    } else message(paste0("Token retrieved from ", url, " !"))
  } else stop(paste0("Token NOT retrieved from ", url, " !"))

  #return(token)
  #in package, do something like assign(token, niToken, envir = NIcalc)
  assign(".niToken", token, envir = passEnv)
  assign(".url", url, envir = passEnv)


  }
