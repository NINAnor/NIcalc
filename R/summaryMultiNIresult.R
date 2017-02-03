#' Summary function for multiNIresult class
#'
#' Summary function that returns point estimates of the Nature Index together with confidence intervals for a set of
#' N.reg basic spatial units or aggregated NI-areas from a N.reg x nsim matrix of nsim simulations of NI for each area.
#'
#' @name summary.multiNIresult
#' @author Bård Pedersen, Jens Åström
#' @param NI object of class `NIresult`, double matrix,	dim = (N.kom or N.reg) x \code{nsim} matrix of NI draws calculated by \code{NIcalculate}
#' @param quantiles double, length = 3, Quantiles used as basis for point estimate and limits of confidence interval. Default is c(0.025,0.5,0.975)
#' @return \code{NIest}, double matrix	dim=(N.kom or N.reg) x 3 	matrix of NI estimates
#' @method summary multiNIresult
#' @export
#' @examples
#'
#' @section Notes: As default, \code{NIestimate} returns the median simulated value as the point estimate and the 0.025- and 0.975-
#' quantiles as limits for the confidence interval.
#'
#' @seealso \code{\link{NIcalculate}}


summary.multiNIresult <- function(multiNI,quantiles=c(0.025,0.5,0.975)) {


  out <- NULL

  for(i in 1:length(multiNI)){

  NIi<-summary(multiNI[[i]])

        year <- as.numeric(gsub("(year: )([0-9]*)", "\\2", names(multiNI)[i]))

        iresult <- cbind(year,NIi)
        out <- rbind(out,iresult)
  }


  class(out) <- c("NIsummary")


  return(out)
}


