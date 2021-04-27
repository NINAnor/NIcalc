#' Summarize Index Value Estimates for a Set of NIunits.
#'
#' Summary method for objects of class \code{\link{niOutput}}.
#'
#' @seealso \code{\link{calculateIndex}} and \code{\link{niOutput}} for cumputation
#' and generation of \code{niOutput} objects.
#'
#' @name summary.niOutput
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @method summary niOutput
#'
#' @param object \code{niOutput} object.
#' @param ... arguments passed on to \code{\link{summary.niSeries}} controlling
#'   the summary: \cr\code{limits}: quantiles to be used as limits in interval
#'   estimates ("confidence interval", defaults to \code{c(0.025,0.975)}),
#'   \cr\code{locationMean}: which location parameter to use as point estimate
#'   mean (\code{T}) or median (\code{F}, default).
#'
#' @return a list of numeric matrices, one matrix for each NIunit, and each
#' matrix containing interval and point estimates for each year, together with
#' an estimate of the displacement parameter (see \code{\link{summary.niSeries}}).
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
#'                        awBSunit = "Skog"))
#' }
#'
#' @export

summary.niOutput <- function(object = NULL, ...) {

  z <- match.call()
  x <- object

  if (!is.niOutput(x)) {
    stop(paste("'",z[2],"' is not of class 'niOutput'.", sep=""), call. = TRUE)
  }

  setSummaryParameters <- function(limits = c(0.025,0.975), locationMean = F) {

    return(list(limits = limits,
                locationMean = locationMean))
  }

  param <- setSummaryParameters(...)

  limits <- param$limits
  locationMean <- param$locationMean

  NIunitNames <- names(x)
  nNIunits <- length(NIunitNames)
  y <- NULL
  for (j in 1:nNIunits) {
    y[[j]] <- summary(x[[j]],limits = limits, locationMean = locationMean)
  }
  names(y) <- NIunitNames
  return(y)
}
