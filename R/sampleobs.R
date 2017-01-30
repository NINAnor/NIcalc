#' Random Draws
#'
#' \code{sampleobs} returns \code{nsim} random draws from a two-parameter
#' probability distribution specified by the arguments.
#'
#' \code{distrib} and the parameters mu and sig
#'
#' from a predetermined set of distribution families. The predetermined set includes
#' normal-, truncated normal-, lognormal-, weibull-, gumbell-, gamma-, poisson-, negative binomial-, and zero-inflated poisson distribution.
#' The lower bound in the truncated normal distribution is always zero, while the upper bound is infinity.
#' With these restrictions all theoretical distributions have two parameters, except the poisson with only one parameter.
#' Alternatively, if distrib == "NoBoot", the function returns nsim copies of obs.

#' @name sampleobs
#' @author Nigel Yoccoz and Bård Pedersen
#'
#' @param nsim	double	length = 1, number of draws (simulations)
#' @param distrib 	character	length = 1, distribution family ("Normal","TruncNormal","LogNormal","Gumbel","Weibull","ZIExponential","Gamma","Poisson","NegBinom","ZIP") or copy obs (when distrib == "NoBoot")
#' @param mu		double	length = 1, parameter 1 of model distribution
#' @param sig		double	length = 1, parameter 2 of model distribution
#' @param obs		double	length = 1, value (observed mean) to be copied if distrib == "NoBoot".
#
#' @return double vector length = \code{nsim}, \code{nsim} random draws from
#' specified (by distrib, mu and sig) model or \code{nsim} copies of \code{obs}.
#
# Functions called:
# rNO, rtnorm, rLOGNO, rWEI, rGU, qGA, rPO, rNBII, rZIP
# random generation functions in the gamlss package
#


sampleobs <- function(nsim = 99, distrib, mu, sig, obs) {

  if (distrib == "Gamma"){
    vec <- rGA(nsim, mu = mu, sigma = sig)
    while (NaN %in% vec) {
      vec[is.na(vec)] <- rGA(length(vec[is.na(vec)]), mu = mu,
                                                sigma = sig)
      }
  }

  if (distrib == "LogNormal")    {vec <- rLOGNO(nsim, mu = mu, sigma = sig)}
  if (distrib == "TruncNormal")  {vec <- rtnorm(nsim, mean = mu, sd = sig,
                                                lower = 0)}
  if (distrib == "Weibull")      {vec <- rWEI(nsim, mu = mu, sigma = sig)}
  if (distrib == "ZIExponential") {
    vec <- rep(0, nsim)
    zero.val <- runif(nsim, 0, 1) < mu
    vec[!zero.val] <- rexp(nsim - sum(zero.val), rate = sig)
  }
  if (distrib == "Poisson")      {vec <- rPO(nsim, mu = mu)}
  if (distrib == "NegBinom")     {vec <- rNBII(nsim, mu = mu, sigma = sig)}
  if (distrib == "ZIP")          {vec <- rZIP(nsim, mu = mu, sigma = sig)}
  if (distrib == "NoBoot")       {vec <- rep(obs, nsim)}

  return(vec)

}
