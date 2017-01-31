#' Fit Probability Distribution to one Indicator Observation, reduced Version
#'
#' Function that, in the continuous case, fits a lognormal distribution to an
#' expected value and two quantiles, or fits a Poisson- or negative binomial
#' distribution in the discrete case, using the least square criterion.
#' \code{estimlight.fct} is a simplified alternative to \code{\link{estim.fct}}.
#'
#' @seealso \code{\link{estim.fct}},\cr \code{\link{elicitate}} for fitting
#' probability distributions to multiple indicator observations and for list of
#' model distributions included,\cr \code{\link{qdev}} for calculating sum of
#' squares between \code{obsval} and model.
#'
#' @name estimlight.fct
#' @author Nigel Yoccoz and Bård Pedersen
#' @importFrom stats nlminb
#' @inheritParams estim.fct
#' @return \code{estim.fct} returns a data.frame with \code{dim = c(1,4)},
#' consisting of the following vectors\cr \code{[[1]] $distrib} character,
#' selected family for model distribution, i.e. one of \code{c("LogNormal",
#' "NegBinom", "Poisson")}. \cr \code{[[2]] $mu} double,
#' first parameter of fitted model distribution\cr \code{[[3]] $sig}
#' double, second parameter of fitted model distribution\cr
#' \code{[[4]] $crit} double, sum of squared deviations between observed
#' parameters and those of the fitted model distribution.
#'
#' @examples
#' estimlight.fct(obsval = c(6,13,25))
#' estimlight.fct(obsval = c(6,13,25),proba = c(0.025,0.975), type = "continuous")
#' estimlight.fct(obsval = c(6,13,25),proba = c(0.025,0.975), type = "discrete")
#' @export

estimlight.fct <- function(obsval = NULL, proba = c(0.25, 0.75),
                           type = "continuous") {

  if(obsval[2] == 0) {obsval[2] <- 0.0001}

  if (type == "continuous") {
    #
    # For each family of predetermined continuous model distributions, find
    # parameter values that gives the best fit to obsval using the least squares
    # criterion.
    #

    mini2 = nlminb(start = c(0.5, 1), objective = qdev.LOGNO,
                   lower = c(-1e6, 0.001), prob = proba, obs = obsval)

    #
    # Select and store model with the best fit to obsval
    #
    critlist<-c(mini2$objective)
    distrib<-c("LogNormal")
    mu<-c(mini2$par[1])
    sig<-c(mini2$par[2])
    sel<-c(T)
    res<-data.frame(distrib=distrib[sel],mu=mu[sel],sig=sig[sel],crit=critlist[sel])
  }

  if (type == "discrete") {
    #
    # For each family of predetermined continuous model distributions, find
    # parameter values that gives the best fit to obsval using the least squares
    # criterion.
    #
    mini1 = nlminb(start = c(0.5), objective = qdev.PO, lower = c(0.001, 0.001),
                   prob = proba, obs = obsval)
    mini2 = nlminb(start = c(0.5, 1), objective = qdev.NBII,
                   lower = c(0.001, 0.001), prob = proba, obs = obsval)

    #
    # Select and store model with the best fit to obsval
    #

    critlist <- c(mini1$objective, mini2$objective)
    distrib <- c("Poisson", "NegBinom")
    mu <- c(mini1$par[1], mini2$par[1])
    sig <- c(NA, mini2$par[2])
    sel <- critlist==min(critlist)
    res<-data.frame(distrib = distrib[sel], mu = mu[sel], sig = sig[sel],
                    crit = critlist[sel])
  }

  if(nrow(res) > 1) {res <- res[1,]}

  return(res)
}
