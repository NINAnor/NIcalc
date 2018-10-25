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


fitAndPlotDistibution <- function(obsval = c("mu" = NULL,
                                             "lower" = NULL,
                                             "upper" = NULL),
                                  refValue = 1,
                                  probQuant = c("lower" = 0.25,
                                            "upper" = 0.75),
                                  ...) {


  obsval1 <- obsval[c("lower", "mu", "upper")] # NB! Functions assume the order: lower quartile, expected value, upper quartile,
  # while the order in the input web-page is expected value, lower quartile, upper quartile.

  a <- try(estim.fct(obsval = obsval1, proba = probQuant[c("lower", "upper")], ...), silent = T)
  if (length(a) == 1){a <- estimlight.fct(obsval = obsval1, proba = probQuant[c("lower", "upper")], ...)}

  plotDistribution(distrib = as.character(a$distrib[1]),
                   mu = a$mu[1],
                   sig = a$sig[1],
                   refValue = refValue,
                   obsval = obsval1,
                   ...)


}



