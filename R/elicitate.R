#' Fit Probability Distributions to Indicator Observations
#'
#' \code{elicitate} fits probability distributions to
#' a set of non-negative indicator observations by selecting for
#' each observation the distribution among a predetermined set of
#' model distribution families that best fits the observation
#' according to the least square criterion.
#'
#' The function presumes that indicator observations are each given as
#' three parameters of either a continuous or discrete probability distribution:
#' the expected value together with two quantiles. The two quantiles default to
#' the lower- and upper quartiles respectively, but may be set to other
#' quantiles using the argument \code{prob.quant}.
#'
#' The predetermined set of continuous model distributions consists of
#' the truncated normal-, lognormal-, Weibull-, gamma-, and zero-inflated
#' exponential distribution. The predetermined set of discrete models is the
#' Poisson-, negative binomial-, and zero-inflated Poisson distribution
#' families.
#'
#' \code{Expected.value}, \code{Lower} and \code{Upper} should be of equal
#' length.\cr For indicator observations and reference values with no
#' uncertainty the corresponding elements of \code{Expected.value},
#' \code{Lower}, and \code{Upper} should be equal.\cr Negative values in any of
#' \code{Expected.value}, \code{Lower}, and \code{Upper} are interpreted as
#' missing values and all corresponding output elements are set to \code{NA}.
#' If one of \code{Expected.value}, \code{Lower} and \code{Upper} is \code{NA},
#' all corresponding output elements are set to \code{NA}.
#'
#' \code{elicitation} first try to fit a model distribution with a call to
#' function \code{\link{estim.fct}}. If this results in an error, it calls
#' \code{\link{estimlight.fct}} instead.
#'
#' \code{elicitation} is a utility function adapted to the mathematical framework
#' for calculating the nature index.
#'
#' @seealso \code{\link{estim.fct}}, \code{\link{estimlight.fct}},\cr
#' \code{\link{qdev}} for the calculation of sum of squares between
#' the parameters of the indicator observation and model distributions.
#'
#' @name elicitate
#' @author Bård Pedersen
#'
#' @param expected.value	double, length =	n.obs, vector of the indicator
#' observations' expected values
#' @param lower	double, length = n.obs, vector of the indicator observations'
#' lower quantiles (0.25 quartiles)
#' @param upper	double, length = n.obs, vector of the indicator observations'
#' upper quantiles (0.75 quartiles)
#' @param type.t character, length = 1 or n.obs, vector of types of measurement
#' scales. Valid types are \code{"continuous"} and \code{"discrete"}. If
#' \code{"continuous"} a continuous model is fitted to the corresponding
#' indicator observation. If \code{"discrete"} a discrete model is fitted to the
#' observation. If \code{length(type.t)==1} all observations are interpreted to
#' be measured on eihter a \code{"continuous"}, which is the default, or
#' \code{"discrete"} scale.
#' @param prob.quant double, length = 2, quantiles supplied in \code{"lower"}
#' and \code{"upper"}. Default is the lower and upper quartiles.
#'
#' @return \code{elicitate} returns a data.frame with \code{dim = c(n.obs,4)},
#' consisting of the following vectors\cr \code{[[1]] $FK_DistID} character,
#' vector of names of fitted model distributions, i.e. one of
#' \code{c("LogNormal", "TruncNormal", "ZIExponential", "Weibull", "Gamma",
#' "Poisson", "NegBinom", "ZIP")} for each indicator observation.\cr
#' \code{[[2]] $mu} double, vector of first parameter of fitted model
#' distributions\cr \code{[[3]] $sig} double, vector of second parameter of
#' fitted model distributions\cr \code{[[4]] $ssq} double, vector of sum of
#' squared deviations between observed parameters and those of the fitted model
#' distributions.
#'
#' @export

 elicitate <- function(expected.value=NULL, lower=NULL, upper = NULL,
                         type.t = c("continuous"), prob.quant = c(0.25,0.75)) {

  N.values <- length(expected.value)

  if (length(type.t) == 1) type.t <- rep(type.t, N.values)

  elicitation.results <- data.frame(rep(NA, N.values),rep(NA, N.values),
                                    rep(NA, N.values),rep(NA, N.values))
  dimnames(elicitation.results) [[2]] <- c("FK_DistID", "mu", "sig", "ssq")

  expected.value[(expected.value < 0 | lower < 0 | upper < 0 |
                  is.na(expected.value) | is.na(lower) | is.na(upper))] <- NA

  for (i in which(!is.na(expected.value))) {
    obsval <- c(lower[i], expected.value[i], upper[i])
    type.tt <- type.t[i]
    #
    #	First, try to fit distribution model to obsval using estim.fct, if it
    # fails - use estimlight.fct
    #
    a <- try(estim.fct(obsval = obsval, proba = prob.quant, type = type.tt),
             silent=T)
    if (length(a)==1) {
      a <- estimlight.fct(obsval = obsval, proba = prob.quant, type = type.tt)
      }
    elicitation.results$FK_DistID[i] <- as.character(a$distrib)
    elicitation.results$mu[i]<-a$mu
    elicitation.results$sig[i]<-a$sig
    elicitation.results$ssq[i]<-a$crit
  }

  return(elicitation.results)
}
