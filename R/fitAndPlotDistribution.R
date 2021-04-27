#' Fit and plot distribution functions
#'
#' Calls elicitation functions with inputs and plots the resulting distribution.
#'
#' \code{fitAndPlotDistribution} calls \code{estim} which fits a two-parameter
#' non-negative continuous model distribution to an expected value and lower
#' and upper quartiles provided as input. It uses the least square criterion to
#' find the best fit among a predetermined set of model distribution families:
#' \code{c("Gamma","LogNormal", "TruncNormal", "Weibull", "ZIExponential")}.
#' Thereafter the function calls \code{\link{plotDistribution}} which plot the
#' distribution together with a reference value.
#'
#' @name fitAndPlotDistribution
#' @encoding UTF-8
#' @author Jens Åström
#'
#' @param obsval_mu input expected value
#' @param obsval_lower input lower quartile
#' @param obsval_upper input upper quartile
#' @param refValue the reference value to be drawn in the  plot
#' @param probQuant_lower fixed parameter set to 0.25
#' @param probQuant_upper fixed parameter set to 0.75
#' @param type fixed parameter set to "continuous"
#' @param ... further arguments passed on to \code{plotDistribution}
#'
#' @seealso \code{\link{estim}} and \code{\link{plotDistribution}}
#'
#' @note In it's present form \code{fitAndPlotDistribution} only accepts expected
#' value and lower and upper quartiles as input to \code{obsval}. It may
#' be expanded to cover other observed parameters and more distributions.
#'
#' @export

fitAndPlotDistribution <- function(obsval_mu = NULL,
                                   obsval_lower = NULL,
                                   obsval_upper = NULL,
                                   refValue = NULL,
                                   probQuant_lower = 0.25,
                                   probQuant_upper = 0.75,
                                   type = "continuous",
                                   ...){

  obsval1 <- c("lower" = obsval_lower,
               "mu" = obsval_mu,
               "upper" = obsval_upper)

  # NB! Functions estim, estimlight, plotDistribution assume the order:
  # lower quartile, expected value, upper quartile, while the order in the
  # input web-page is expected value, lower quartile, upper quartile.

  probQuant_lower <- 0.25
  probQuant_upper <- 0.75
  type <- "continuous"

  a <- try(estim(obsval = obsval1, proba = c(probQuant_lower, probQuant_upper), type = "continuous"), silent = T)
  if (length(a) == 1){a <- estimlight(obsval = obsval1, proba = c(probQuant_lower, probQuant_upper), type = "continuous")}

  plotDistribution(distrib = as.character(a$distrib[1]),
                   mu = a$mu[1],
                   sig = a$sig[1],
                   refValue = refValue,
                   obsval = obsval1,
                   type = type,
                   ...)


}
