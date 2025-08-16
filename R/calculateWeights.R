#' Weighting per NIunit
#'
#' Returns weights for the calculation of the Nature index for a set of NIunits.
#'
#' The Nature index and many thematic indices are weighted averages of indicator
#' observations. The weights are calculated \emph{a priori}, i.e. they do not
#' depend on the observations' values. The final weight given to an indicator
#' observation from a BSunit within an NIunit is the product of the observation's
#' BSunit weight and the BSunit's NIunit (area) weight. See the vignette
#' \code{NatureIndexCalculation} for details.
#'
#' Usually, the Nature index and thematic indices are calculated for a whole set
#' of NIunits simultaniously. The function calculates weights for each combination
#' of BSunit and indicator for all NIunits in the set.
#'
#' @seealso \code{\link{calculateBSunitWeights}} and \code{\link{calculateNIunitWeights}}.
#' \cr The vignette \code{NatureIndexCalculation} gives a complete description
#' of the framework for calculating weights for the Nature index.
#'
#' @name calculateWeights
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param BSunitWeights numeric BSunit x indicator matrix with BSunit weights
#'   for each combination of BSunits and indicators, where
#'   \code{rowSums(BSunitWeights,na.rm = TRUE) = rep(1,nrow)}.
#' @param NIunitWeights numeric BSunit x NIunit matrix with NIunit weights for
#'   each combination of BSunits and NIunits, where
#'   \code{colSums(NIunitWeights) = rep(1,n)}.
#' @param enforce_weightSum1 logical. If TRUE, rescales weights for each indicator-
#' year combination to sum to 1. This allows calculating indices representing
#' comparable area-weighted means that are specific to the combination of areas for
#' which data is available. This is so far only relevant for single indicator
#' indices calulated without imputation; the default is therefore set to FALSE.
#' @return A list \code{x} of BSunit x indicator matrices \code{zi}, one matrix for
#'   each NIunit. Each matrix \code{zi} containing weights for each combination of
#'   BSunit and indicator, and satisfying \code{sum(zi,na.rm = TRUE) = 1}.
#'
#' @examples
#' # Test data set
#' nBS <- 4
#' nInd <- 10
#' indicators <- paste("Indic",1:nInd)
#' BSunits <- paste("Unit",1:nBS)
#' NIunits <- cbind(c(1,1,1,1),c(1,1,0,0),c(1,0,0,0),c(1,0,1,1),c(0,0,0,1))
#' NIunitNames <- c("All units","Two units","Unit 1","Some units","Unit 4")
#' dimnames(NIunits) <- list(BSunits,NIunitNames)
#' awBSunit <- runif(4)*1000
#' ICunits <- matrix(1,nrow=nBS,ncol=nInd)
#' for (i in 1:5) {for (j in 1:5) {ICunits[sample(1:nBS,1),sample(1:nInd,1)] <- NA}}
#' dimnames(ICunits) <- list(BSunits,indicators)
#' ff <- sample(c(rep(100,nInd-1),75))
#' tt <- sample(paste("group", 1:3),nInd,replace = TRUE)
#' kk <- rep(FALSE,nInd)
#' kk[sample(1:nInd,3)] <- TRUE
#'
#' x <- calculateWeights(BSunitWeights =
#'                          calculateBSunitWeights(ICunits = ICunits,
#'                          indicators = indicators,fidelity = ff,
#'                          trophicGroup = tt, keyElement = kk),
#'                       NIunitWeights =
#'                          calculateNIunitWeights(BSunits = BSunits,
#'                          NIunits = NIunits, awBSunit = awBSunit))
#' x
#' sum(x$'Some units',na.rm = TRUE)
#'
#'@export
#'

calculateWeights <- function(BSunitWeights = NULL,
                             NIunitWeights = NULL,
                             enforce_weightSum1 = FALSE) {

  # Check input

  if (length(BSunitWeights) == 0) {
    stop("Required argument 'BSunitWeights' missing with no default", call. = TRUE)
  }

  if (length(NIunitWeights) == 0) {
    stop("Required argument 'NIunitWeights' missing with no default", call. = TRUE)
  }

  y <- match.call()
  if (dim(BSunitWeights)[1] != dim(NIunitWeights)[1]) {
    stop(paste("BSunits in '",y[2],"' do not match those in '",y[3],"'.",sep=""), call. = TRUE)
  }

  if (sum((dimnames(BSunitWeights)[[1]] != dimnames(NIunitWeights)[[1]])) > 0) {
    stop(paste("BSunits named in '",y[2],"' do not match those named in '",y[3],"'.",sep=""), call. = TRUE)
  }

  outWeights <- NULL
  for (i in 1:dim(NIunitWeights)[2]) {
    zzz <- BSunitWeights*0
    #is.na(zzz) <- 0
    for (j in 1:dim(BSunitWeights)[2]) {
      zzz[,j] <- BSunitWeights[,j] * NIunitWeights[,i]
    }
    outWeights <- c(outWeights,list(zzz))
    names(outWeights)[length(outWeights)] <- dimnames(NIunitWeights)[[2]][i]
  }


  # Optional: rescale weights to sum to 1
  if(enforce_weightSum1){
    for(i in 1:length(outWeights)){
      for(k in 1:ncol(outWeights[[i]])){

        sumWeights <- sum(outWeights[[i]][,k], na.rm = TRUE)
        if(sumWeights != 1){
          outWeights[[i]][,k] <- outWeights[[i]][,k]/sumWeights
        }
      }
    }
  }

  return(outWeights)
}
