#' Create an custom object
#'
#' Test function to create new object of a custom class, which will have its own method functions.
#'
#' This simple example will generate random numbers from a normal distribution, and produce a list with those numbers, and the bins
#' (the result of the table function). It will assign this list a particular class, to which we can develop specific methods. This will
#' probably be used a lot in the package.
#'
#' @param N Number of random numbers to generate. Integer value.
#' @param mean Mean of random numbers.
#' @param sd Standard deviation of the draws.
#' @return List containing
#' \itemize{
#'  \item{"RandomNumbers": The random numbers}
#'  \item{"TableOfNumbers": A table of the counts of individual numbers}
#' }
#'
#' @examples
#' add(1, 1)
#' add(13, 1)
#' @export
createObject <- function(N = 100, mean = 1, sd = 1) {
  rnd.numbers <- floor(rnorm(n = N, mean = mean, sd = sd))
  rnd.table <- table(rnd.numbers)
  out <- list(RandomNumbers = rnd.numbers, TableOfNumbers = rnd.table)

  class(out) <- c("testObject", "list")
  return(out)
}
