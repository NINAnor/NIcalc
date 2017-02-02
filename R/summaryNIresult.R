#' Summary function for NIresult class
#'
#' Summary function that returns point estimates of the Nature Index together with confidence intervals for a set of
#' N.reg basic spatial units or aggregated NI-areas from a N.reg x nsim matrix of nsim simulations of NI for each area.
#'
#' @name summary.NIresult
#' @author Bård Pedersen, Jens Åström
#' @param NI object of class `NIresult`, double matrix,	dim = (N.kom or N.reg) x \code{nsim} matrix of NI draws calculated by \code{NIcalculate}
#' @param quantiles double, length = 3, Quantiles used as basis for point estimate and limits of confidence interval. Default is c(0.025,0.5,0.975)
#' @return \code{NIest}, double matrix	dim=(N.kom or N.reg) x 3 	matrix of NI estimates
#' @method summary NIresult
#' @export
#' @examples
#' NIoutput <- NIcalculate(Areaind.Name = Indicator.area.ind$IndicatorareaID,
#' Areaind.Indicator = Indicator.area.ind$Indicator_name, Area.Name = Indicator.area$IndicatorareaID,
#' Area.Municipality = Indicator.area$Basicunit, FK_OmraadeID = Observations$IndicatorareaID[Observations$ReferenceYearID ==i],
#' nsim = 1000, scaled.bootmat = scaled.bootmatyeari, Weights.trof = munweightsforall)
#' NIestimate(NI=matrix(1,nrow=1,ncol=100),kvantiler=c(0.025,0.5,0.975))
#'
#' @section Notes: As default, \code{NIestimate} returns the median simulated value as the point estimate and the 0.025- and 0.975-
#' quantiles as limits for the confidence interval.
#'
#' @seealso \code{\link{NIcalculate}}


summary.NIresult <- function(NI,quantiles=c(0.025,0.5,0.975)) {

  if (dim(NI)[1] == 1) {
    NIest <- t(as.matrix(NI[,1:3]*0))
    dimnames(NIest)[[1]] <- list(dimnames(NI)[[1]])
  } else {
    NIest <- NI[,1:3]*0
  }
  dimnames(NIest)[[2]] <- as.character(quantiles)
  for (j in 1:dim(NI)[1]) {
    NIest[j,] <- quantile(NI[j,],quantiles)
  }

  class(NIest) <- c("NIsummary", "matrix")
  return(NIest)
}


