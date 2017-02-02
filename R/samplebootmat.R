#' Draw Bootstrap Samples
#'
#' \code{samplebootmat} returns \code{nsim} bootstrap samples of a set of
#' indicator observations and reference values.
#'
#' Bootstrap samples consist of either \code{nsim} random draws from a set of
#' probability distributions given by \code{DistID} and the parameter vectors
#' \code{mu} and \code{sig}, or \code{nsim} copies of the elements of \code{Value},
#' or a combination of draws and copies, depending on the parameter
#' \code{Type.of.uncertainty}.
#'
#' \code{samplebootmat} presumes that the set of probability distributions only
#' includes the truncated normal-, lognormal-, weibull-, gamma-, zero-inflated
#' exponential, poisson-, negative binomial-, and zero-inflated poisson
#' distributions. The lower bound in the truncated normal distribution is always
#' zero, while the upper bound is infinity. Alternatively, if distrib == “NoBoot”,
#' the function returns
#'
#' The function does not handle missing values (NAs).
#'
#' \code{samplebootmat} is a utility function adapted to the mathematical
#' framework for calculating the nature index.
#'
#' \code{samplebootmat} calls \code{\link{sampleobs}} which does the random draws.
#'
#' @seealso \code{\link{sampleobs}},
#'
#' @name samplebootmat
#' @author Bård Pedersen

#' @param ValueID	integer, \code{length = nvalues}, Indicator observation IDs.
#' @param Value	double, \code{length = nvalues}, Expected values.
#' @param RefobsID	integer, \code{length = nvalues}, Identifies elements in
#' \code{ValueID, Value, DistID, mu, sig} representing reference values.
#' @param DistID character, \code{length = nvalues}, distribution family, i.e.
#' \code{"Gamma", "LogNormal", "TruncNormal", "Weibull", "ZIExponential",
#' "NegBinom", "Poisson", "ZIP"} or \code{"NoBoot"} (copy obs).
#' @param mu double, \code{length = nvalues}, parameter 1 of model distribution
#' @param sig	double, \code{length = nvalues}, parameter 2 of model distribution
#' @param nsim double, \code{length = 1}, number of bootstrap samples (simulations)
#' @param ref.value.code integer, \code{length = 1}, Code used in RefYearID to
#' identify reference values.
#' @param Type.of.uncertainty	character, \code{length = 1}, Type of bootstrap sample.
#' \code{"All"} (all elements in the sample are draws), \code{"None"} (all
#' elements are copies), \code{"Observations"} (elements representing indicator
#' observations are draws, while elements representing reference values are copies),
#' \code{"Referencevalues"} (indicator observations are copies, reference values
#' are draws).
#'
#' @return double matrix, \code{dim = c(nvalues, nsim)}, \code{nsim} bootstrap sample
#' as specified by \code{RefobsID}, \code{Value}, \code{distrib}, \code{mu},
#' and \code{sig}.
#'
#' @examples
#' # Example data
#' aa <- 1:2 # ValueID
#' bb <- rep(1,2) # Value
#' cc <- c(0,1) # RefobsID
#' dd <- rep(c("LogNormal"), 2) # DistID
#' mu <- rep(0,2) # mu
#' sig <- rep(1,2)  # sig
#' samplebootmat(aa,bb,cc,dd,mu,sig)
#' #' samplebootmat(aa,bb,cc,dd,mu,sig,Type.of.uncertainty = "All")
#' samplebootmat(aa,bb,RefobsID = c(0,0),dd,mu,sig)
#' samplebootmat(aa,bb,cc,dd,mu,sig,ref.value.code = 1)
#' samplebootmat(aa,bb,cc,dd,mu,sig,ref.value.code = 2)
#'
#' @export

samplebootmat <- function(ValueID = NULL,
                          Value = NULL,
                          RefobsID = NULL,
                          DistID = NULL,
                          mu = NULL,
                          sig = NULL,
                          ref.value.code = 0,
                          Type.of.uncertainty = c("Observations"),
                          nsim = 100) {

  n.values <- length(ValueID)
  bootmat <- matrix(0.0, nrow = n.values, ncol = nsim)
  dimnames(bootmat)[[1]] <- ValueID

  if (Type.of.uncertainty == "Observations") {
    Notboot.values <- RefobsID == ref.value.code
  }
  if (Type.of.uncertainty == "Referencevalues") {
    Notboot.values <- RefobsID != ref.value.code
  }
  if (Type.of.uncertainty == "None") {
    Notboot.values <- rep(T, n.values)
  }
  if (Type.of.uncertainty == "All") {
    Notboot.values <- rep(F, n.values)
  }

  for (i in which(Notboot.values)) {
    bootmat[i, ] <- rep(Value[i],nsim)
  }
  for (i in which(!Notboot.values)) {
    bootmat[i, ] <- sampleobs(nsim = nsim,
                              distrib = DistID[i], mu = mu[i], sig = sig[i], obs = Value[i])
  }

  return(bootmat)
}

