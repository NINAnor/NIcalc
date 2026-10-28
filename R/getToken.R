#' Get a Connection Token from the NI Database
#'
#' This function tries to connect to the NI database API and retrieves a token that is later used to
#' communicate with the database. If no personal username and password are provided, defaults to
#' the public read-only access. If you are providing your own login credentials, you can either
#' supply them to the function directly, or specify them as environmental variables NICALC_USER and NICALC_PASSWORD.
#'
#' @encoding UTF-8
#' @author Chloé R. Nater
#'
#' @import httr
#'
#' @param username character. Your username. Defaults to open read-only credentials unless provided.
#' @param password character. Your password. Defaults to open read-only credentials unless provided.
#' @param url	 character.	API address.
#' @return Invisibly returns the authentication token and stores it in .niToken and .url for subsequent API requests.
#'
#' @examples
#' \dontrun{
#' getToken("test.user", "test.password")
#' }
#'
#' @export
#'

getToken <- function(
    username = NULL,
    password = NULL,
    url = "https://naturindekscalcapi.nina.no"
) {

  # Resolve default credentials when not explicitly provided
  if (is.null(username)) {
    username <- Sys.getenv(
      "NICALC_USER",
      unset = "lesebruker@nina.no"
    )
  }

  if (is.null(password)) {
    password <- Sys.getenv(
      "NICALC_PASSWORD",
      unset = "9Lz^O893uBy*"
    )
  }

  # Validate credentials
  if (!is.character(username) || length(username) != 1L ||
      is.na(username) || !nzchar(username)) {
    stop("A valid API username is required.", call. = FALSE)
  }

  if (!is.character(password) || length(password) != 1L ||
      is.na(password) || !nzchar(password)) {
    stop("A valid API password is required.", call. = FALSE)
  }

  # Request authentication token
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
