#' Internal functions
#'
#' These functions set up a passive environment to store tokens
#' @return Builds a passive/hidden environment
#' @author Jens Åström
#' @keywords internal
#' @export

passEnv <- new.env()

.getUrl <- function(){

  if(!exists(".url", envir = passEnv)) stop("No connection. Connect to database using 'getToken()' first.", call. = F)
  url = get(".url", envir = passEnv)

  return(url)
}

.getToken <- function(){

  if(!exists(".niToken", envir = passEnv)) stop("No connection. Connect to database using 'getToken()' first.", call. = F)
  url = get(".niToken", envir = passEnv)

  return(url)
}
