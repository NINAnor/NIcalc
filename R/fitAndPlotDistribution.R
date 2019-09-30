#' fitAndPlotDistribution
#'
#' Calls estimate (elicitation) functions with inputs and plots the resulting distribution
#'
#'
#' @param obsval named vector containing mu and lower and upper quantiles to fit the distribution to.
#' @param refValue the reference value to be drawn in the  plot
#' @param probQuant named vector containing the lower and upper quantiles to draw in the plot
#'
#'
#'
#'
#'
#'
#'
#'
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
  #  obsval1 <- c(obsval_lower, obsval_mu, obsval_upper) # NB! Functions assume the order: lower quartile, expected value, upper quartile,
  # while the order in the input web-page is expected value, lower quartile, upper quartile.

  a <- try(estim.fct(obsval = obsval1, proba = c(probQuant_lower, probQuant_upper), ...), silent = T)
  if (length(a) == 1){a <- estimlight.fct(obsval = obsval1, proba = c(probQuant_lower, probQuant_upper), ...)}

  plotDistribution(distrib = as.character(a$distrib[1]),
                   mu = a$mu[1],
                   sig = a$sig[1],
                   refvalue = refValue,
                   obsval = obsval1,
                   ...)


}



