#' NIunit Weights
#'
#' Returns NIunit weights, i.e. weight factors used in the calculation of indices
#' for a set of NIunits.
#'
#' The Nature index and many thematic indices are weighted averages of indicator
#' observations. NIunit (area) weights (\code{Wa}) are relevant when an index is
#' calculated for an NIunit consisting of more than one BSunit. They determine
#' the relative weight given to each BSunit in the weighted average.
#'
#' NIunit weights are calculated \emph{a priori}, i.e. they do not depend on the
#' observations' values. In the Norwegian implementation of the Nature Index
#' (\code{NI}), NIunit weights are based on the area the major habitat in
#' question covers in each BSunit. An alternative is to use the total area or
#' another quality of BSunits. \code{Wa} for a BSunit is the relevant area
#' divided by the corresponding total area of the aggregated NIunit. BSunits
#' outside the aggregate receive weights equal zero.
#'
#' Usually, the Nature index and thematic indices are calculated for a set of
#' NIunits simultaniously, where each unit consists of from one to all BSunits
#' selected from a fixed set of BSunits. The function calculate NIunit weights
#' for the whole set of NIunits.
#'
#' @seealso \code{\link{calculateBSunitWeights}} and \code{\link{calculateWeights}}.
#' \cr The vignette \code{NatureIndexCalculation} gives a complete description
#' of the framework for calculating weights for the Nature index.
#'
#' @name calculateNIunitWeights
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param BSunits character vector with the names of all BSunits included in the
#'   calculation.
#' @param NIunits integer, \code{0/1}, BSunit x NIunit matrix.
#'   Each element \code{NIunits[i,j]} is a logical flag: Is BSunit \code{i} a
#'   member of NIunit \code{j}? \code{0 = FALSE, 1 = TRUE}.
#' @param awbs logical flag: should weights be based on the BSunit variable
#'   \code{awBSunit} (\code{awbs = TRUE}, default), or should all BSunits within
#'   an NIunit be given equal weight (\code{awbs = FALSE})?
#' @param awBSunit numeric vector containing the BSunit variable used to calculate
#'   weights. Required if \code{awbs = TRUE}.
#'
#' @return A numeric BSunit x NIunit matrix \code{x}, with weights per BSunit
#'   for each NIunit, and where \code{colSums(x) = rep(1,ncol)}.
#'
#' @examples
#' # Test data
#' NIunits <- cbind(c(1,1,1,1),c(1,1,0,0),c(1,0,0,0),c(1,0,1,1),c(0,0,0,1))
#' BSunitNames <- c("bs1","bs2","bs3","bs4")
#' NIunitNames <- c("All units","Two units","Unit 1","Some units","Unit 4")
#' dimnames(NIunits) <- list(BSunitNames,NIunitNames)
#' awBSunit <- runif(4)*1000
#'
#' # Default calculation:
#' x <- calculateNIunitWeights(BSunits = BSunitNames, NIunits = NIunits, awBSunit = awBSunit)
#' x
#' colSums(x)
#'
#' # Equal weighting of BSunits:
#' calculateNIunitWeights(BSunits = BSunitNames, NIunits = NIunits, awbs = FALSE)
#'
#'@export
#'

calculateNIunitWeights <- function(BSunits = NULL,
                                   NIunits = NULL,
                                   awbs = TRUE,
                                   awBSunit = NULL) {


  # Check input

  if (length(BSunits) == 0) {
    stop("Required argument 'BSunits' missing with no default", call. = TRUE)
  }

  if (length(NIunits) == 0) {
    stop("Required argument 'NIunits' missing with no default", call. = TRUE)
  }

  missBSu <- which(!(dimnames(NIunits)[[1]] %in% BSunits))
  if (length(missBSu) > 0) {
    mess1 <- paste("\nWeighting data missing for BSunits:\n'",
                   paste(dimnames(NIunits)[[1]][missBSu],collapse = "', '"),
                   "'.",sep="")
  } else {
    mess1 <- ""
  }
  missBSu2 <- which(!(BSunits %in% dimnames(NIunits)[[1]]))
  if (length(missBSu2) > 0) {
    mess2 <- paste("\nNIunits not specified for BSunits:\n'",
                   paste(BSunits[missBSu2],collapse = "', '"),
                   "'.",sep="")
  } else {
    mess2 <- ""
  }
  if (length(missBSu) > 0 | length(missBSu2) > 0) {
    stop(paste(mess1,"\n",mess2), call. = TRUE)
  }

  if (!awbs) {
    awBSunit <- rep(1,length(BSunits))
  } else {
    if (length(awBSunit) == 0) {
      stop("Required argument 'awBSunit' missing with no default.", call. = TRUE)
    }
  }

  # Calculate weights for each NIunit
  ddd <- NIunits*0
  for (i in dimnames(NIunits)[[1]]) {
    j <- which(BSunits==i)
    ddd[i,] <- awBSunit[j]
  }
  ddd <- ddd*NIunits
  NIunitWeights <- t(t(ddd)/colSums(ddd))

  return(NIunitWeights)

}

