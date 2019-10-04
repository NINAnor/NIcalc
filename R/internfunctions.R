passEnv <- new.env()

.getUrl <- function(){

  url = get(".url", envir = passEnv)

  return(url)
}

.getToken <- function(){

  url = get(".niToken", envir = passEnv)

  return(url)
}
