#' Draw Bootstrap Samples
#'
#' \code{sampleObsMat} returns \code{nsim} bootstrap samples of a set of
#' indicator observations from the same year or reference values.
#'
#' Bootstrap samples consist of either \code{nsim} random draws from a set of
#' probability distributions, or \code{nsim} copies of the elements of
#' \code{value}, depending on the parameter \code{obsMethod}.
#'
#' Probability distributions are either two-parameter, non-negative probability
#' distributions given by vectors \code{distrib}, \code{mu} and \code{sig}, or
#' distribution objects in the list \code{customDistribution},
#' or a distribution of \code{nsim} sampled, imputed values in \code{imputes}.
#'
#' \code{sampleObsMat} presumes that the set of probability distributions only
#' includes the truncated normal-, lognormal-, weibull-, gamma-, zero-inflated
#' exponential, poisson-, negative binomial-, and zero-inflated poisson
#' distributions. The lower bound in the truncated normal distribution is always
#' zero, while the upper bound is infinity.
#'
#' \code{sampleObsMat} is a utility function adapted to the mathematical
#' framework for calculating the nature index.
#'
#' \code{sampleObsMat} calls functions \code{\link{sampleobs}} and
#' \code{\link{sampleDistribution}} which do the random draws.
#'
#' @seealso \code{\link{sampleobs}}, \code{\link{sampleDistribution}}
#'
#' @name sampleObsMat
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param ICunitId integer vector
#' @param value numeric vector with expected values
#' @param distrib character vector with names of model distributions
#' @param mu numeric vector with parameter 1 of model distribution
#' @param sig numeric vector with parameter 2 of model distribution
#' @param customDistribution list of distribution objects and \code{NA}s
#' @param imputations optional list of class \code{niImputations}
#' @param obsMethod string, either \code{"sample"} or \code{"constant"}
#' @param obsType character vector with observation types. Valid elements are:
#'   \cr\code{"tradObs"}: corresponding observation is a two-parameter,
#'   non-negative, probability distribution given by corresponding elements in
#'   \code{distrib}, \code{mu} and \code{sig}
#'   \cr\code{"customObs"}: corresponding observation is a custom distribution
#'   object in \code{customDistribution}
#'   \cr\code{"imputations"}: corresponding observation is stored in the object
#'   \code{imputations} as \code{nsim} draws, one from each of \code{nsim}
#'   imputed distributions.
#' @param impYear character, year in question. The parameter is only relevant
#'   when the data set includes imputed values. Used to identify the correct
#'   elements in \code{imputations} which might include observations from
#'   several years. The parameter is not relevant for sets of reference values.
#' @param nsim integer - number of bootstrap simulations, default is 1000.
#'
#' @return numeric matrix, \code{dim = c(length(values), nsim)}, \code{nsim}
#' bootstrap samples of the set of indicator observations or reference values.
#'
#' @examples
#' x <- themeData$indicatorValues$`2019`[11:20,]
#' sampleObsMat(ICunitId = x$ICunitId,
#'              value = x$expectedValue,
#'              distrib = x$distributionFamilyName,
#'              mu = x$distParameter1,
#'              sig = x$distParameter2,
#'              customDistribution = x$customDistribution,
#'              obsMethod = "sample",
#'              obsType = "tradObs",
#'              nsim = 5)
#'
#'@export


sampleObsMat <- function(ICunitId = NULL,
                         value = NULL,
                         distrib = NULL,
                         mu = NULL,
                         sig = NULL,
                         customDistribution = NULL,
                         imputations = NULL,
                         obsMethod = "sample",
                         obsType = NULL,
                         impYear = NULL,
                         nsim = 1000) {

  if (length(ICunitId) == 0) {
    stop("Required argument 'ICunitId' missing with no default", call. = TRUE)
  }
  if (length(value) == 0) {
    stop("Required argument 'value' missing with no default", call. = TRUE)
  }
  if (length(distrib) == 0) {
    stop("Required argument 'distrib' missing with no default", call. = TRUE)
  }
  if (length(mu) == 0) {
    stop("Required argument 'mu' missing with no default", call. = TRUE)
  }
  if (length(sig) == 0) {
    stop("Required argument 'sig' missing with no default", call. = TRUE)
  }
  if (length(customDistribution) == 0) {
    stop("Required argument 'customDistribution' missing with no default", call. = TRUE)
  }
  if (length(obsType) == 0) {
    stop("Required argument 'obsType' missing with no default", call. = TRUE)
  }

  bootmat <- matrix(0,nrow=length(ICunitId),ncol=nsim)
  if (length(ICunitId) == 1) {
    dimnames(bootmat)[[1]] <- list(ICunitId)
  } else {
    dimnames(bootmat)[[1]] <- ICunitId
  }


  if (obsMethod == "sample") {
    for (j in ICunitId[obsType == "tradObs"]) {
      k <- which(ICunitId == j)
      bootmat[as.character(j),] <- sampleobs(nsim = nsim,
                                             distrib = distrib[k],
                                             mu = mu[k],
                                             sig = sig[k])
    }
    for (j in ICunitId[obsType == "customObs"]) {
      k <- which(ICunitId == j)
      bootmat[as.character(j),] <- sampleDistribution(dist = customDistribution[[k]],
                                                      nSamples = nsim)
    }
    for (j in ICunitId[obsType == "imputations"]) {
      bootmat[as.character(j),] <- imputations$imputations[imputations$identifiers$ICunitId == j &
                                                             imputations$identifiers$year == impYear,][1:nsim]
    }
  }

  if (obsMethod == "constant") {
    for (j in ICunitId[obsType %in% c("tradObs","customObs")]) {
      k <- which(ICunitId == j)
      bootmat[as.character(j),] <- rep(value[k],nsim)
    }

    for (j in ICunitId[obsType == "imputations"]) {
      ddd <- mean(imputations$imputations[imputations$identifiers$ICunitId == j &
                                            imputations$identifiers$year == impYear,][1:nsim])
      bootmat[as.character(j),] <- rep(ddd,nsim)
    }
  }

  return(bootmat)
}
