#' Scaling of Indicator Observations
#'
#' \code{scaleObsMat} scales a set of indicator observations according to chosen
#' scaling model (\code{"Low"} or \code{"Max"}) and associated reference values.
#'
#' Each indicator observation and reference value is assumed to be entered as
#' \code{nsim} draws from their respective distributions. The sets of
#' indicator observations and reference values are both entered as
#' \code{nObs x nsim} matrices, with identical row names.
#'
#' Scaling models are specific for each observation. They should be entered as a
#' named vector of scaling model codes \code{"Low"} or \code{"Max"}, with
#' \code{length = nObs} and names identical to the row names of
#' indicator observation and reference values matrices.
#'
#' @seealso The vignette \code{NatureIndexCalculation} for a description of the
#' framework for calculating the Nature index
#'
#' @name scaleObsMat
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param	bootmat	double matrix	\code{dim = nObs x nsim}	of indicator observations.
#' @param	refmat	double matrix	\code{dim = nObs x nsim}	of reference values.
#' @param	scalingModel character \code{length = nObs}	named
#'   vector of scaling model names \code{"Low"} or \code{"Max"}.
#' @param	truncAtRef logical flag: whether to truncate scaled indicator
#'   observations at the reference value \code{truncAtRef = TRUE} (default),
#'   or not\code{truncAtRef = FALSE}).
#'
#' @return double matrix \code{dim=nObs x nsim}	of scaled indicator observations.
#'
#' @examples
#' \dontrun{
#' # Example based on the themeData set for 2019
#' bootmat <- sampleObsMat(ICunitId = themeData$indicatorValues[["2019"]]$ICunitId,
#'                         value = themeData$indicatorValues[["2019"]]$expectedValue,
#'                         distrib = themeData$indicatorValues[["2019"]]$distributionFamilyName,
#'                         mu = themeData$indicatorValues[["2019"]]$distParameter1,
#'                         sig = themeData$indicatorValues[["2019"]]$distParameter2,
#'                         customDistribution = themeData$indicatorValues[["2019"]]$customDistribution,
#'                         imputations = NULL,
#'                         obsMethod = "sample",
#'                         obsType = "tradObs",
#'                         nsim = 10)
#' refmat <- sampleObsMat( ICunitId = themeData$referenceValues$ICunitId,
#'                         value = themeData$referenceValues$expectedValue,
#'                         distrib = themeData$referenceValues$distributionFamilyName,
#'                         mu = themeData$referenceValues$distParameter1,
#'                         sig = themeData$referenceValues$distParameter2,
#'                         customDistribution = themeData$referenceValues$customDistribution,
#'                         imputations = NULL,
#'                         obsMethod = "constant",
#'                         obsType = "tradObs",
#'                         nsim = 10)
#' scaleVec <- themeData$indicatorValues[["2019"]]$scalingModel
#' names(scaleVec) <- themeData$indicatorValues[["2019"]]$ICunitId
#' scaleVec <- scaleVec[dimnames(bootmat)[[1]]]
#' scaledBootmat <- scaleObsMat(bootmat = bootmat,
#'                         refmat = refmat,
#'                         scalingModel = scaleVec)
#' }
#'
#' @export

scaleObsMat <- function(bootmat = NULL,
                        refmat = NULL,
                        scalingModel = NULL,
                        truncAtRef = TRUE) {

  if (length(bootmat) == 0) {
    stop("Required argument 'bootmat' missing with no default", call. = TRUE)
  }
  if (length(refmat) == 0) {
    stop("Required argument 'refmat' missing with no default", call. = TRUE)
  }
  if (length(scalingModel) == 0) {
    stop("Required argument 'scalingModel' missing with no default", call. = TRUE)
  }

  scaledBootmat <- bootmat/refmat

  if (!(is.matrix(scaledBootmat))) {
    scaledBootmat <- matrix(scaledBootmat)
    dimnames(scaledBootmat)[[1]] <- names(scalingModel)
  }

  modelMat <- scaledBootmat*0
  modelMat[modelMat==0] <- "Max"

  for (i in dimnames(scaledBootmat)[[1]]) {
    modelMat[i,] <- scalingModel[i]
  }

  scaledBootmat[modelMat == "Max"] <- (-1)*scaledBootmat[modelMat == "Max"] + 2
  scaledBootmat[scaledBootmat < 0] <- 0.0
  if (truncAtRef) {
    scaledBootmat[scaledBootmat > 1] <- 1.0
  }

  if (!(is.matrix(scaledBootmat))) {
    scaledBootmat <- t(matrix(scaledBootmat))
    dimnames(scaledBootmat)[[1]] <- names(scalingModel)
  }

  return(scaledBootmat)
}
