#' Random Draws
#'
#' \code{sampleobs} returns \code{nsim} random draws from a two-parameter
#' probability distribution specified by the function's arguments.
#'
#' \code{sampleobs} samples from a single distribution among a predetermined set
#' of distribution families. The predetermined set consists of the gamma-,
#' lognormal-, truncated normal-, Weibull-, zero-inflated exponential-,
#' negative binomial-, Poisson-, and zero-inflated Poisson distribution families.
#' The lower bound in the truncated normal distribution is always zero, while the
#' upper bound is infinity. With these restrictions all distributions in the set
#' have two parameters, except the Poisson with only one parameter.
#'
#' \code{sampleobs} is a utility function adapted to the mathematical framework
#' for calculating the nature index.
#'
#' \code{sampleobs} calls random generation functions rLOGNO, rWEI, qGA, rPO,
#' rNBII, rZIP in the gamlss.dist package, rtnorm in the msm package, runif and
#' rexp in the stats package.
#'
#' @seealso \code{\link{sampleObsMat}}
#'
#' @name sampleobs
#' @encoding UTF-8
#' @author Nigel Yoccoz and Bård Pedersen
#'
#' @import gamlss.dist
#' @importFrom msm rtnorm
#' @importFrom stats runif
#' @importFrom stats rexp
#'
#' @param nsim	double,	length = 1, number of draws (simulations)
#' @param distrib character, length = 1, distribution family, i.e. one of
#' \code{c("Gamma", "LogNormal", "TruncNormal", "Weibull", "ZIExponential",
#' "NegBinom", "Poisson", "ZIP")} or copy obs (when distrib == "NoBoot")
#' @param mu double, length = 1, parameter 1 of model distribution
#' @param sig double, length = 1, parameter 2 of model distribution
#'
#' @return double vector, length = \code{nsim}, \code{nsim} random draws from
#' model specified by \code{distrib}, \code{mu}, and \code{sig}.
#'
#' @examples
#' nn <- 100000
#' hist(sampleobs(nsim = nn, distrib="Gamma", mu = 1, sig = 0.5), breaks=50)
#' hist(sampleobs(nsim = nn, distrib="LogNormal", mu = -2, sig = 0.5), breaks=50)
#' hist(sampleobs(nsim = nn, distrib="TruncNormal", mu = -2, sig = 0.5), breaks=50)
#' hist(sampleobs(nsim = nn, distrib="Weibull", mu = 2, sig = 7), breaks=50)
#' hist(sampleobs(nsim = nn, distrib="ZIExponential", mu = 0.2, sig = 0.5),
#'      breaks=50)
#' hist(sampleobs(nsim = nn, distrib="NegBinom", mu = 4, sig = 1))
#' hist(sampleobs(nsim = nn, distrib="Poisson", mu = 4, sig = NA))
#' hist(sampleobs(nsim = nn, distrib="ZIP", mu = 10, sig = 0.25))
#'
#'@export

sampleobs <- function(nsim = 99, distrib, mu, sig) {

  if (distrib == "Gamma"){
    vec <- gamlss.dist::rGA(nsim, mu = mu, sigma = sig)
    while (NaN %in% vec) {
      vec[is.na(vec)] <- gamlss.dist::rGA(length(vec[is.na(vec)]), mu = mu,
                                          sigma = sig)
    }
  }

  if (distrib == "LogNormal")    {vec <- gamlss.dist::rLOGNO(nsim, mu = mu, sigma = sig)}
  if (distrib == "TruncNormal")  {vec <- msm::rtnorm(nsim, mean = mu, sd = sig,
                                                     lower = 0)}
  if (distrib == "Weibull")      {vec <- gamlss.dist::rWEI(nsim, mu = mu, sigma = sig)}
  if (distrib == "ZIExponential") {
    vec <- rep(0, nsim)
    zero.val <- stats::runif(nsim, 0, 1) < mu
    vec[!zero.val] <- stats::rexp(nsim - sum(zero.val), rate = sig)
  }
  if (distrib == "Poisson")      {vec <- gamlss.dist::rPO(nsim, mu = mu)}
  if (distrib == "NegBinom")     {vec <- gamlss.dist::rNBII(nsim, mu = mu, sigma = sig)}
  if (distrib == "ZIP")          {vec <- gamlss.dist::rZIP(nsim, mu = mu, sigma = sig)}
  if (distrib == "NoBoot")       {vec <- mu}

  return(vec)

}
