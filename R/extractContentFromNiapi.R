#' Extract Data from the Nature Index API
#'
#' \code{extractContentFromNiapi} is a general purpose utility function called by other functions that
#' retrieves data from specified tables in the Nature Index database over the
#' [The Nature Index Application Programming Interface](https://www8.nina.no/NaturindeksNiCalc/index.html)
#' and returns the raw content as a list.
#'
#' @encoding UTF-8
#' @author Bård Pedersen
#' @name extractContentFromNiapi
#'
#' @import httr
#'
#' @param APIUrl	character, API address - including the server address
#' @param methodPath	character, method address within the API
#' @param token	character, token retrieved from the API using \code{\link{getToken}}
#' @param ... further parameters to \code{\link[httr]{content}}
#'
#' @return A list containing the raw content extracted from the API.
#'
#' @export

extractContentFromNiapi <- function(APIUrl = "https://www8.nina.no/NaturindeksNiCalc",
                                    methodPath = NULL,
                                    token = NULL, ...) {

  # Following statement might be necessary if peer certificate errors start to reoccur

  httr::set_config(httr::config(ssl_verifypeer = 0L)) #Fix "Peer certificate error"

  # Authorization

  auth_string <- paste("bearer", token, sep = " ")

  # extractContentFromNiapi does not set an user agent identifier with httr::user_agent().
  # Setting identifiers might be necessary in the future if the API is set up to log requests based on user agents.
  # Currently the API does not have a system for logging requests. The plan is to base such a system on
  # tokens and authorization strings instead of user agent identifiers. Tokens can be used to recognize the
  # clients as they are linked to their usernames in the NI database.

  # API request

  apiResponce <- httr::GET(url = paste(APIUrl,methodPath,sep=""),
                           encode = "json",
                           httr::add_headers(Authorization = auth_string))

  # Check for errors. Stop execution if errors occur

  if (httr::http_error(apiResponce)) {
    httpStatusCode <- httr::status_code(apiResponce)
    apiMessage <-"See https://tools.ietf.org/html/rfc7231"
    apiShortMessage <- ""
    if (httpStatusCode == 401) {
      apiShortMessage <- " Unauthorized."
      apiMessage <- apiResponce$headers$`www-authenticate`
    }
    if (httpStatusCode == 403) {
      apiShortMessage <- " Forbidden."
    }
    if (httpStatusCode == 404) {
      apiMessage <- paste("Resource not found: ",apiResponce$url,sep="")
    }
    if (httpStatusCode >= 500) {
      apiShortMessage <- paste(" Server error.\nResource ",apiResponce$url,sep="")
    }
    stop(
      paste(
        "API request failed.\n",
        "HTTP response status code: ",httpStatusCode,apiShortMessage,"\n",
        apiMessage, sep=""),
      call. = FALSE)
  }

  if (httr::http_type(apiResponce) != "application/json") {
    stop(paste("API response format not recognised.\n",
               APIUrl,
               methodPath,
               " did not return json.",sep=""),
         call. = FALSE)
  }

  # Extract content from API response

  rawContent <- httr::content(apiResponce, ...)

  rm(apiResponce)

  return(rawContent)
}
