#' Transform normal distribution to log-normal, and vice versa
#'
#' These are two convenience functions that takes the mean and the standard deviation
#' of a normal distribution and returns the corresponding values for the lognormal
#' distibution of the values, and vice versa.
#'
#' @param mean Numerical. Mean (or meanlog) parameter.
#' @param sd Numerical. Standard deviation (or scale) parameter.
#'
#' @return \code{normal2Lognormal}: A list of "mean" and "sd", giving the mean and scale parameter of the
#' lognormal distribution, suited to be used later in e.g. \code{gamlss.dist}, \cr
#' \code{logNormal2normal}: A list of mean and standard deviation of a the corresponding normal distribution.
#'
#' @encoding UTF-8
#' @author Jens Åström, Bård Pedersen
#' @name normal2Lognormal
#'
#' @examples
#' logNormPar <- normal2Lognormal(mean = 100, sd = 10)
#' logNormal2normal(logNormPar[[1]], logNormPar[[2]])
#'
NULL
#' @rdname normal2Lognormal
#' @export
normal2Lognormal <- function(mean = NULL,
                             sd = NULL){

  mean <- as.vector(mean)
  sd <- as.vector(sd)
  out_mean <- log(mean / sqrt((1 + sd^2 / mean^2 )))
  out_sd <- sqrt(log(1 + (sd^2 / mean^2)))


  out <-list("mean" = out_mean,
             "sd" = out_sd)

  return(out)

}

#' @rdname normal2Lognormal
#' @export
logNormal2normal <- function(mean = NULL, sd = NULL){

  mean <- as.vector(mean)
  sd <- as.vector(sd)

  out_mean <- exp(mean + (sd^2 / 2))
  out_sd <- sqrt((exp(sd^2) - 1) * exp(2 * mean + sd^2))

  out <-list("mean" = out_mean,
             "sd" = out_sd)
  return(out)

}
