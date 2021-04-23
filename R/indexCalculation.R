#' Weighted Average per NIunit of Scaled Indicator Observations
#'
#' Calculates, for each unit in a set of NIunits, a sample of \code{nsim} draws
#' from the distribution of the Nature index or a thematic index.
#'
#' \code{indexCalculation} draws a sample of size \code{nsim} from the distribution
#' of the Nature index or a thematic index for each unit in a set of NIunits.
#' Each draw is a weighted average of a bootstrap sample of corresponding draws
#' from the distributions of a set of indicator observations.
#'
#' For each NIunit, weights are entered per BSunit x indicator combination. For
#' each indicator observation the delineation of the corresponding ICunit must
#' be provided in the input.
#'
#' @seealso The vignette \code{NatureIndexCalculation} for a description of the
#'   framework for calculating the Nature index, \cr and functions
#'   \code{\link{calculateIndex}}, \code{\link{calculateWeights}},
#'   and \code{\link{scaleObsMat}}.
#'
#' @name indexCalculation
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param ICunits numeric BSunit x indicator matrix describing the
#'   occurrence of indicators in BSunits. \code{NA}s in a column should represent
#'   BSunits where the indicator is not observed.
#' @param NIWeights A named list of numeric BSunit x indicator matrices \code{zi},
#'   one matrix for each NIunit. Each matrix \code{zi} containing weights for each
#'   combination of BSunit and indicator, and satisfying
#'   \code{sum(zi,na.rm = TRUE) = 1}.
#' @param scaledBootmat numeric \code{nObs x nsim} matrix with \code{nsim}
#'   bootstrap samples from each of \code{nObs} scaled indicator observations.
#' @param nsim integer - number of bootstrap simulations.
#'
#' @note Indices are calculated for a set of NIunits, while scaled indicator
#' observations, on the other hand, refer to indicator specific ICunits that may
#' vary in geographical extent among indicators, and will in general not
#' correspond to the NIunits. Thus, \code{indexCalculation} needs input about
#' the spatial extent of each ICunit and NIunit. The spatial extent of NIunits
#' are indirectly provided by the weights as Bsunits not included in the NIunit
#' are given zero weight.
#'
#' @return A named list of numeric vectors, one vector for each NIunit, where each
#'   vector contains \code{nsim} draws from the NIunit's index distribution.
#'
#' @examples
#' \dontrun{
#' # Calculate thematic indices based on the themeData set for 2019 for the NIunits
#' # defined in themeData$NIunits
#'
#' # Preparing bootstrap sample as input to indexCalculation:
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
#' # NIunit weights as input to indexCalculation:
#' xxx <- calculateBSunitWeights(ICunits = themeData$ICunits,
#'                         indicators = themeData$indicators$name,
#'                         fids = FALSE,
#'                         tgroups = FALSE)
#' yyy <- calculateNIunitWeights(BSunits = themeData$BSunits$name,
#'                         NIunits = themeData$NIunits,
#'                         awBSunit = themeData$BSunits$Skog)
#' NIWeights <- calculateWeights(BSunitWeights = xxx,NIunitWeights = yyy)
#'
#' # Calculate thematic index for each NIunit
#' indices <- indexCalculation(ICunits = themeData$ICunits,
#'                                      NIWeights = NIWeights,
#'                                      scaledBootmat = scaledBootmat,
#'                                      nsim = 10)
#' }
#'
#' @export

indexCalculation <- function(ICunits = NULL,
                             NIWeights = NULL,
                             scaledBootmat = NULL,
                             nsim = NULL) {

  if (length(ICunits) == 0) {
    stop("Required argument 'ICunits' missing with no default", call. = TRUE)
  }
  if (length(NIWeights) == 0) {
    stop("Required argument 'NIWeights' missing with no default", call. = TRUE)
  }
  if (length(scaledBootmat) == 0) {
    stop("Required argument 'scaledBootmat' missing with no default", call. = TRUE)
  }
  if (length(nsim) == 0) {
    stop("Required argument 'nsim' missing with no default", call. = TRUE)
  }

  scaledObsPerBsunit <- array(0,dim = c(dim(ICunits),nsim))
  dimnames(scaledObsPerBsunit) [[1]] <- dimnames(ICunits) [[1]]
  dimnames(scaledObsPerBsunit) [[2]] <- dimnames(ICunits) [[2]]
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

  indices <- NULL
  for (j in names(NIWeights)) {
    index <- rep(0,dim(scaledBootmat)[2])
    for (k in 1:dim(scaledBootmat)[2]) {
      index[k] <- sum(NIWeights[[j]] * scaledObsPerBsunit[,,k],na.rm = TRUE)
    }
    indices[[j]] <- index
  }

  return(indices)
}
