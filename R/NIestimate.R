#' Point Estimates and Confidence Intervals of NI
#'
#' Function for calculating point estimates of the Nature Index together with confidence intervals for a set of
#' N.reg basic spatial units or aggregated NI-areas from a N.reg x nsim matrix of nsim simulations of NI for each area.
#'
#' @name NIestimate
#' @author Bård Pedersen
#' @param NI double matrix,	dim = (N.kom or N.reg) x \code{nsim} matrix of NI draws calculated by \code{NIcalculate}
#' @param kvantiler double, length = 3, Quantiles used as basis for point estimate and limits of confidence interval. Default is c(0.025,0.5,0.975)
#' @return \code{NIest}, double matrix	dim=(N.kom or N.reg) x 3 	matrix of NI estimates
#' @keywords
#' @export
#' @examples
#'
#' @section Notes: As default, \code{NIestimate} returns the median simulated value as the point estimate and the 0.025- and 0.975-
#' quantiles as limits for the confidence interval.
#'
#' @seealso \code{NIcalculate}


NIestimation.fct <- function(NI=matrix(1,nrow=1,ncol=100),kvantiler=c(0.025,0.5,0.975)) {

  if (dim(NI)[1] == 1) {
    NIest <- t(as.matrix(NI[,1:3]*0))
    dimnames(NIest)[[1]] <- list(dimnames(NI)[[1]])
  } else {
    NIest <- NI[,1:3]*0
  }
  dimnames(NIest)[[2]] <- as.character(kvantiler)
  for (j in 1:dim(NI)[1]) {
    NIest[j,] <- quantile(NI[j,],kvantiler)
  }
  return(NIest)
}


