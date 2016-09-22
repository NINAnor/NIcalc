#' Fit Probability Distribution to one Indicator Observation
#'
#' Function that selects, for both continuous and discrete cases and by using
#' the least square criterion, the distribution among a predetermined set of
#' model distribution families that best fits to an expected value and two
#' quantiles provided as arguments.
#'
#' @seealso  \code{\link{estimlight.fct}} for a simplified version of
#' \code{estim.fct},\cr \code{\link{elicitate}} for fitting probability
#' distributions to multiple indicator observations and for the list of model
#' distributions included in the predetermined set,\cr \code{\link{qdev}} for
#' calculating sum of squares between \code{obsval} and model.
#'
#' @name estim.fct
#' @author Nigel Yoccoz and Bård Pedersen
#'
#' @importFrom stats nlminb
#'
#' @param obsval double, length = 3, observed mean and quantiles in the sequence
#' \code{c(lower.quantile, mean, upper.quantile)}
#' @param proba double, length = 2, quantiles supplied in \code{"obsval"}.
#' Default is the lower and upper quartiles
#' @param type character, length = 1, type of measurement scales. Valid types
#' are \code{"continuous"} and \code{"discrete"}.
#' When \code{type = "continuous"}, a continuous model is fitted to the indicator
#' observation. When \code{type = "discrete"}, a discrete model is fitted.
#'
#' @return \code{estim.fct} returns a data.frame with \code{dim = c(1,4)},
#' consisting of the following vectors\cr \code{[[1]] $distrib} character,
#' selected family for model distribution, i.e. one of \code{c("LogNormal",
#' "TruncNormal", "ZIExponential", "Weibull", "Gamma","Poisson",
#' "NegBinom", "ZIP")}. \cr \code{[[2]] $mu} double,
#' first parameter of fitted model distribution\cr \code{[[3]] $sig}
#' double, second parameter of fitted model distribution\cr
#' \code{[[4]] $ssq} double, sum of squared deviations between observed
#' parameters and those of the fitted model distribution.
#'
#' @examples
#' estim.fct(obsval = c(0.3,0.6,0.8),proba = c(0.25,0.75), type = "continuous")
#' estim.fct(obsval = c(6,13,25),proba = c(0.025,0.975), type = "continuous")
#' estim.fct(obsval = c(6,13,25),proba = c(0.025,0.975), type = "discrete")
#' @export

estim.fct <- function(obsval = c(0.3,0.6,0.8),proba = c(0.25,0.75),
                      type = "continuous") {

  if (type == "continuous"){
    #
    # For each family of predetermined continuous model distributions, find
    # parameter values that gives the best fit to obsval using the least
    # squares criterion.
    #
    mini1=nlminb(start = c(0.5, 1), objective = qdev.LOGNO,
                 lower = c(-1e6, 0.001), prob = proba, obs = obsval)

    a <- try(nlminb(start = c(0.5, 1), objective = qdev.TNO,
                    lower = c(-1e6, 0.001), prob = proba, obs = obsval),
             silent = T)
    if (length(a) == 1){
      mini2 <- mini1
      mini2$objective <- 10e10
    } else {
      mini2 <- a
    }

    mini3 = nlminb(start = c(0.1, 1), objective = qdev.ZEXP,
                   lower=c(0.0, 0.001), upper = c(1, Inf),
                   prob = proba, obs = obsval)

    mini4 = nlminb(start = c(0.5, 1), objective = qdev.WEI,
                   lower = c(0.001, 0.001), prob = proba, obs = obsval)

    mini5 = nlminb(start = c(0.5, 1), objective = qdev.GA,
                   lower = c(0.001, 0.001), prob = proba, obs = obsval)

    mini <- list(mini1, mini2, mini3, mini4, mini5)

    #
    #	Select and store model with the best fit to obsval
    #

    critlist <- c(mini1$objective, mini2$objective, mini3$objective,
                  mini4$objective, mini5$objective)
    distrib <- c("LogNormal", "TruncNormal", "ZIExponential", "Weibull",
                 "Gamma")
    mu <- c(mini1$par[1], mini2$par[1], mini3$par[1], mini4$par[1],
            mini5$par[1])
    sig <- c(mini1$par[2], mini2$par[2], mini3$par[2], mini4$par[2],
             mini5$par[2])
    sel <- critlist == min(critlist)
    res <- data.frame(distrib = distrib[sel], mu = mu[sel], sig = sig[sel],
                      crit = critlist[sel])
  }

  if (type == "discrete"){
    #
    # For each family of predetermined descrete model distributions, find
    # parameter values that gives the best fit to obsval using the least
    # squares criterion.
    #
    mini1 = nlminb(start = c(0.5), objective = qdev.PO,
                   lower = c(0.001, 0.001), prob = proba, obs = obsval)
    mini2 = nlminb(start = c(0.5, 1), objective = qdev.NBII,
                   lower = c(0.001, 0.001), prob = proba, obs = obsval)
    mini3 = nlminb(start = c(0.5, 1), objective = qdev.ZIP,
                   lower = c(0.001, 0.001), upper = c(10000, 0.999),
                   prob = proba, obs = obsval)
    mini <- list(mini1, mini2, mini3)
    #
    #	Select and store model with the best fit to obsval
    #
    critlist <- c(mini1$objective, mini2$objective, mini3$objective)
    distrib <- c("Poisson", "NegBinom", "ZIP")
    mu <- c(mini1$par[1], mini2$par[1], mini3$par[1])
    sig <- c(NA, mini2$par[2], mini3$par[2])	#only one parameter in Poisson
    sel <- critlist == min(critlist)
    res <- data.frame(distrib = distrib[sel],mu = mu[sel],sig = sig[sel],
                      crit = critlist[sel])
  }

  if (nrow(res) > 1) {res <- res[1, ]}	# in case minimum is two or more equal
                                        # least squares

  return(res)
}
