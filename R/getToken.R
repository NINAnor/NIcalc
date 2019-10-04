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
                     url = "https://ninweb17.nina.no"){


  token_path <- paste0("/NaturindeksAPI/token?username=", username,"&password=", password)

  httr::set_config(httr::config(ssl_verifypeer = 0L)) #Fix "Peer certificate error"


  gettoken <- httr::POST(url = url, path = token_path, encode = "json")
  token <- httr::content(gettoken)



  #return(token)
  #in package, do something like assign(token, niToken, envir = NIcalc)
  assign(".niToken", token, envir = passEnv)
  assign(".url", url, envir = passEnv)

  message(paste0("Token successfully retrieved from ", url))


  }
