#' Weighted Average per BSunit of Scaled Indicator Observations
#'
#' Calculates, for each unit in a set of BSunits, a sample of \code{nsim} draws
#' from the distribution of the Nature index or a thematic index.
#'
#' \code{indexCalculationPerBSunit} draws a sample of size \code{nsim} from the distribution
#' of the Nature index or a thematic index for each unit in a set of BSunits.
#' Each draw is a weighted average of a bootstrap sample of corresponding draws
#' from the distributions of a set of indicator observations.
#'
#' Weights are entered per BSunit x indicator combination. For
#' each indicator observation the delineation of the corresponding ICunit must
#' be provided in the input.
#'
#' @seealso Index calculations and associated terminology are explained in the
#' vignette \code{NatureIndexCalculation}. \cr Functions
#' \code{\link{calculateIndex}}, \code{\link{calculateBSunitWeights}},
#' and \code{\link{scaleObsMat}}.
#'
#' @name indexCalculationPerBSunit
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param ICunits numeric BSunit x indicator matrix describing the
#'   occurrence of indicators in BSunits. \code{NA}s in a column should represent
#'   BSunits where the indicator is not observed.
#' @param BSWeights A numeric BSunit x indicator matrix \code{x}, where
#'   \code{rowSums(x,na.rm = TRUE) = rep(1,nrow)}, with weights for all indicators
#'   observed in each BSunit and \code{NA}s elsewhere.
#' @param scaledBootmat numeric \code{nObs x nsim} matrix with \code{nsim}
#'   bootstrap samples from each of \code{nObs} scaled indicator observations.
#' @param nsim integer - number of bootstrap simulations.
#'
#' @note Indices are calculated for a set of BSunits, while scaled indicator
#' observations refer to indicator specific ICunits that
#' may vary in geographical extent among indicators, and will in general not
#' correspond to single BSunits. Thus, \code{indexCalculationPerBSunit} needs
#' input about the spatial extent of ICunits in terms of which BSunits they
#' include.
#'
#' @return A BSunit x \code{nsim} numeric matrix, where each row
#'   contains \code{nsim} draws from the BSunit's index distribution.
#'
#' @examples
#' \dontrun{
#' # Calculate a thematic index for each BSunit based on the themeData set for 2019
#'
#' # Preparing bootstrap sample as input to indexCalculationPerBSunit:
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
#'
#' # BSunit weights as input to indexCalculationPerBSunit:
#' BSWeights <- calculateBSunitWeights(ICunits = themeData$ICunits,
#'                                     indicators = themeData$indicators$name,
#'                                     fids = FALSE,
#'                                     tgroups = FALSE)
#' # Calculate index for each BSunit
#' indices <- indexCalculationPerBSunit(ICunits = themeData$ICunits,
#'                                      BSWeights = BSWeights,
#'                                      scaledBootmat = scaledBootmat,
#'                                      nsim = 10)
#' }
#'
#' @export

indexCalculationPerBSunit <- function(ICunits = NULL,
                                      BSWeights = NULL,
                                      scaledBootmat = NULL,
                                      nsim = NULL) {

  if (length(ICunits) == 0) {
    stop("Required argument 'ICunits' missing with no default", call. = TRUE)
  }
  if (length(BSWeights) == 0) {
    stop("Required argument 'BSWeights' missing with no default", call. = TRUE)
  }
  if (length(scaledBootmat) == 0) {
    stop("Required argument 'scaledBootmat' missing with no default", call. = TRUE)
  }
  if (length(nsim) == 0) {
    stop("Required argument 'nsim' missing with no default", call. = TRUE)
  }

  scaledObsPerBsunit <- array(0,dim = c(dim(ICunits),nsim))
  if (length(dimnames(ICunits) [[1]]) == 1) {
    dimnames(scaledObsPerBsunit) [[1]] <- list(dimnames(ICunits) [[1]])
  } else {
    dimnames(scaledObsPerBsunit) [[1]] <- dimnames(ICunits) [[1]]
  }
  if (length(dimnames(ICunits) [[2]]) == 1) {
    dimnames(scaledObsPerBsunit) [[2]] <- list(dimnames(ICunits) [[2]])
  } else {
    dimnames(scaledObsPerBsunit) [[2]] <- dimnames(ICunits) [[2]]
  }
  dimnames(scaledObsPerBsunit) [[3]] <- 1:nsim

  for (j in dimnames(scaledBootmat) [[1]]) {
    if (dim(ICunits)[2] == 1) {
      k1 <- which(ICunits[,] == j,arr.ind = TRUE)
      k2 <- rep(1,length(k1))
      k <- as.matrix(cbind(k1,k2))
    } else {
      k <- which(ICunits[,] == j,arr.ind = TRUE)
    }
    for (kk in 1:dim(k)[1]) {
      scaledObsPerBsunit[k[kk,1],k[kk,2],] <- scaledBootmat[as.character(j),]
    }
  }

  indices <- matrix(0,nrow= dim(BSWeights)[1], ncol=dim(scaledBootmat)[2])
  if (length(dimnames(BSWeights) [[1]]) == 1) {
    dimnames(indices) [[1]] <- list(dimnames(BSWeights) [[1]])
  } else {
    dimnames(indices) [[1]] <- dimnames(BSWeights) [[1]]
  }

  for (k in 1:dim(scaledBootmat)[2]) {
    indices[,k] <- rowSums(BSWeights * scaledObsPerBsunit[,,k],na.rm = TRUE)
  }

  return(indices)
}
