#' Methods for class testObject
#'
#' I think this is the wrong way to go about it.
#'
#' Should probably document different methods separately
#'

mean.testObject <- function(x) mean(x[["RandomNumbers"]])


plot.testObject <- function(x) {
  old.par <- par()
  par(mfrow=c(2,1))

  plot(x[["RandomNumbers"]], ylab = "Random numbers", xlab = "Sequence of random string", main = "Random number sequence")
  plot(x[["TableOfNumbers"]], ylab = "Counts of numbers", xlab = "Random numbers", main = "Histogram of random numbers")
}

