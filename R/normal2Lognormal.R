#' Transform normal distribution to log-normal, and vice versa
#'
#' This is convenience functions that takes the mean and the standard deviation of a normal distribution and returns the corresponding values for the lognormal distibution of the values, and vice versa.
#'
#'
#'
#' @param mu Numerical. Mean (or meanlog) parameter.
#' @param sd Numerical. Standard deviation (or scale) parameter.
#'
#' @return A list of "mean" and "sd", giving the mean and scale parameter of the lognormal distribution, suited to be used later in e.g. \code{gamlss.dist}, or the mean and the standard deviation of a the corresponding normal distribution.
#'
#' @author Jens Åström, Bård Pedersen
#'
#' @examples
#' \dontrun{
#'
#'
#' logNormPar <- normal2Lognormal(mean = 100, sd = 10)
#' logNormal2normal(logNormPar[[1]], logNormPar[[2]])
#'
#'  }
#'
#' @export
#'



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


#' @export
#'
#' @rdname normal2Lognormal
logNormal2normal <- function(mean = NULL, sd = NULL){

  mean <- as.vector(mean)
  sd <- as.vector(sd)

  out_mean <- exp(mean + (sd^2 / 2))
  out_sd <- sqrt((exp(sd^2) - 1) * exp(2 * mean + sd^2))

  out <-list("mean" = out_mean,
             "sd" = out_sd)
  return(out)

}
