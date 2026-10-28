#' Get a Connection Token from the NI Database
#'
#' This function tries to connect to the NI database API and retrieves a token that is later used to
#' communicate with the database.
#'
#' @encoding UTF-8
#' @author Chloé R. Nater
#'
#' @import httr
#'
#' @param username	character		Your username. If none provided, defaults to public read-only access.
#' @param password	character		Your password. If none provided, defaults to public read-only access.
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

getToken <- function(
    username,
    password,
    url = "https://naturindekscalcapi.nina.no"
) {

  response <- httr::POST(
    url = paste0(sub("/+$", "", url), "/token"),
    query = list(
      username = username,
      password = password
    ),
    httr::add_headers(
      Accept = "text/plain",
      `User-Agent` = "NI-R-client"
    )
  )

  # Check HTTP response before processing token
  if (httr::http_error(response)) {
    stop(
      "Token NOT retrieved. HTTP status: ",
      httr::status_code(response),
      call. = FALSE
    )
  }

  # Extract token as plain text
  token <- httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )

  # Validate response
  if (
    length(token) != 1L ||
    is.na(token) ||
    nchar(token) < 30L
  ) {
    stop(
      "Token NOT retrieved: unexpected response.",
      call. = FALSE
    )
  }

  # Preserve existing variables for other NI functions
  assign(".niToken", token, envir = .GlobalEnv)
  assign(".url", url, envir = .GlobalEnv)

  assign(".niToken", token, envir = passEnv)
  assign(".url", url, envir = passEnv)

  message("Token retrieved successfully!")

  invisible(token)
}
