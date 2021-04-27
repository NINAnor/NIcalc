#' Summarize a Time Series of Index Value Estimates.
#'
#' Summary method for objects of class \code{\link{niSeries}}.
#'
#' @seealso \code{\link{calculateIndex}}, \code{\link{niSeries}}, and
#' \code{\link{niOutput}} for cumputation and generation of \code{niSeries} and
#' \code{niOutput} objects.
#'
#' @importFrom stats median
#' @importFrom stats quantile
#'
#' @name summary.niSeries
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @method summary niSeries
#'
#' @param object \code{niSeries} object.
#' @param ... arguments controlling the summary:
#'   \cr\code{limits}: quantiles to be used as limits in intervall
#'   estimates ("confidence interval", defaults to \code{c(0.025,0.975)}),
#'   \cr\code{locationMean}: which location
#'   parameter to use as point estimate mean (\code{T}) or median (\code{F}, default).
#'
#' @return a numeric matrix with interval and point estimates for each year,
#' together with an estimate of the displacement in the location parameter due to
#' nonlinear scaling of uncertain indicator observations etc.,
#' [cf. Pedersen and Skarpaas (2012)](https://www.nina.no/archive/nina/PppBasePdf/rapport/2012/797.pdf).
#'
#' @examples
#' \dontrun{
#' # Summary of the results from the calculation of a thematic index:
#'
#' summary(calculateIndex(x = themeData,
#'                        nsim = 1000,
#'                        fids = FALSE,
#'                        tgroups = FALSE,
#'                        keys = "ignore",
#'                        w = 0,
#'                        awbs = TRUE,
#'                        awBSunit = "Skog")$E)
#' }
#'
#' @export
#'
summary.niSeries <- function(object = NULL, ...) {

  z <- match.call()
  x <- object

  if (!is.niSeries(x)) {
    stop(paste("'",z[2],"' is not of class 'niSeries'.", sep=""), call. = TRUE)
  }

  setSummaryParameters <- function(limits = c(0.025,0.975), locationMean = F) {

    return(list(limits = limits,
                locationMean = locationMean))
  }

  param <- setSummaryParameters(...)

  limits <- param$limits
  locationMean <- param$locationMean

  yearNames <- names(x)
  nYears <- length(yearNames)
  years <- as.integer(yearNames)
  y <- y.low <- y.high <- locDisp <- NULL
  for (j in 1:nYears) {
    if (locationMean) {
      y[j] <- mean(x[[j]]$index)
      locDisp[j] <- y[j] - x[[j]]$bbb
    } else {
      y[j] <- stats::median(x[[j]]$index)
      locDisp[j] <- y[j] - x[[j]]$bbb
    }
    y.low[j] <- stats::quantile(x[[j]]$index,limits[1])
    y.high[j] <- stats::quantile(x[[j]]$index,limits[2])
  }
  summaryTable <- cbind(y.low,y,y.high,locDisp)
  dimnames(summaryTable) <- list(paste(x[[j]]$indexArea,yearNames),
                                 c(paste(limits[1]*100,"%",sep=""),
                                   ifelse(locationMean,"mean","median"),
                                   paste(limits[2]*100,"%",sep=""),
                                   "displacement"))
  return(summaryTable)
}
