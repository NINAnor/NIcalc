#' Plot method for class testObject
#'
#' Example method to see how it works
#'
#' Some details
#' @method plot testObject
#' @export

plot.testObject <- function(x) {
  old.par <- par()
  par(mfrow=c(2,1))

  plot(x[["RandomNumbers"]], ylab = "Random numbers", xlab = "Sequence of random string", main = "Random number sequence")
  plot(x[["TableOfNumbers"]], ylab = "Counts of numbers", xlab = "Random numbers", main = "Histogram of random numbers")
}

