#' Transform normal distribution to log-normal
#'
#' This is a convenience function that takes the mean and the standard deviation of a normal distribution and returns the corresponding values for the lognormal distibution of the values.
#'
#'
#' @param mu	numerical	a numerical value identifying the indicator. Usually this is first retreived by the \link{getIndicators} function
#' @param sd numerica the year of the values to retrieve. Default = NULL, which means you get all values.
#' @param type Return transformed mu or standard deviation? Defaults to mu
#'
#' @return A list of "mean" and "sd", giving the mean and sd of the lognormal distribution, suited to be used later in e.g. \code{gamlss.dist}
#'
#' @author Jens Åström, Bård Pedersen
#'
#' @examples
#' \dontrun{
#'
#'
#' lognormalPars <- normal2Lognormal(mean = 100, sd = 10)
#'
#'
#'  }
#'
#' @export
#'



normal2Lognormal <- function(mean = NULL,
                 sd = NULL){

  mean <- as.vector(mean)
  sd <- as.vector(sd)
  out_mean <- log(mean / sqrt((sd*2 / mean^2 + 1)))
  out_sd <- sqrt(log(1 + (sd^2 / mean^2)))

  out <-list("mean" = out_mean,
           "sd" = out_sd)

  return(out)

}
