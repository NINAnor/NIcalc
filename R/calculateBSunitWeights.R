#' Weighting per BSunit
#'
#' Returns BSunit weights, i.e. weights used in the calculation of indices for a set
#' of BSunits.
#'
#' The Nature index and many thematic indices are weighted averages of indicator
#' observations. The weights are calculated \emph{a priori}, i.e. they do not depend
#' on the observations' values. When estimating indices
#' for a set of BSunits, weights are based on the characteristics of indicators
#' (parameters \code{fidelity}, \code{trophicGroup}, \code{keyElement}) and
#' their occurence in BSunits (parameter \code{ICunits}).
#'
#' \code{calculateBSunitWeights} calculate BSunit weights for each combination of
#' BSunit and indicator. Under default settings, calculation is according to the
#' Norwegian implementation of the Nature index (cf. the vignette
#' \code{NatureIndexCalculation}), where BSunit weights are products of fidelity
#' weights (\code{Wf}) and trophic weights (\code{Wt}). Weights are based on a
#' distinction between key indicators and non-key indicators, and a grouping of
#' non-key indicators, here referred to as trophic groups.
#'
#' \code{Wf} for an indicator observed in a BSunit is the fidelity of the indicator
#' to the ecosystem in question divided by the summed fidelity of indicators
#' belonging to the same trophic group and observed in the BSunit.
#'
#' \code{Wt} for non-key indicators depends on the number of trophic groups (\code{r})
#' observed in the BSunit and whether key indicators are observed in the BSunit or
#' not. \code{Wt = w/r} when key indicators are observed, and  \code{Wt = 1/r} if no
#' key indicators are observed in the BSunit. \code{w} defaults to 0.5. Wt for key
#' indicators equals \code{w} if some non-key indicators are observed in the BSunit,
#' and 1 if only key indicators are observed.
#'
#' There are options for ignoring fidelities and/or grouping of indicators in the
#' calculation of weights, and for alternative treatments of key indicators.
#'
#' @seealso \code{\link{calculateNIunitWeights}} and \code{\link{calculateWeights}}.
#' \cr The vignette \code{NatureIndexCalculation} gives a complete description
#' of the framework for calculating weights for the Nature index.
#'
#' @name calculateBSunitWeights
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param ICunits numeric BSunit x indicator matrix describing the occurrence of
#'   indicators in BSunits. \code{NA}s in a column represent BSunits where
#'   the indicator is not observed.
#' @param indicators character vector with names of all indicators included in the
#'   calculation.
#' @param fidelity numeric vector with the fidelity of each indicator to the major
#'   habitat in question. Required if \code{fids = TRUE}.
#' @param trophicGroup character vector with either name or ID of trophic group for
#'   each indicator. Required if \code{tgroups = TRUE}.
#' @param keyElement logical vector indicating whether corresponding indicator
#'   is a key indicator or not. Required if
#'   \code{keys \%in\% c("asGroup","specialWeight")}.
#' @param fids Logical flag: should fidelities be ignored (\code{fids = FALSE}) in
#'   the calculation of \code{Wi}?
#' @param tgroups Logical flag: should grouping of indicators into trophic and
#'   key indicator groups be ignored (\code{tgroups = FALSE}) in the calculation of
#'   \code{Wf}?
#' @param keys character, one of \code{c("asGroup","ignore","specialWeight")}.
#'   \cr\code{keys = "asGroup"}: Key indicators treated as an ordinary trophic group,
#'   no special weighting of key indicators. \cr\code{keys = "ignore"}: All indicators
#'   treated as non-key indicators and grouped according to \code{trophicGroup}.
#'   \cr\code{keys = "specialWeight"}: Default, special weighting of key indicators
#'   where their trophic weight is set to \code{w}.
#' @param w numeric \code{0 <= w <= 1}, trophic weight given to key indicators if
#'   \code{keys = "specialWeight"}, defaults to \code{0.5}. Automatically reset to
#'   default value if outside \code{[0,1]}.
#'
#' @return A numeric BSunit x indicator matrix \code{x}, where
#'   \code{rowSums(x,na.rm = TRUE) = rep(1,nrow)}, with weights for all indicators
#'   observed in each BSunit and \code{NA}s elsewhere.
#'
#' @examples
#' # Test data
#' nBS <- nInd <- 10
#' BSunits <- paste("unit",1:nBS)
#' indicators <- paste("indic",1:nInd)
#' x <- matrix(1,nrow=nBS,ncol=nInd)
#' for (i in 1:5) {for (j in 1:5) {x[sample(1:nBS,1),sample(1:nInd,1)] <- NA}}
#' dimnames(x) <- list(BSunits,indicators)
#' x
#' ff <- sample(c(rep(100,nInd-1),75))
#' tt <- sample(paste("group", 1:3),nInd,replace = TRUE)
#' kk <- rep(FALSE,nInd)
#' kk[sample(1:nInd,3)] <- TRUE
#' data.frame(indicators = indicators, fidelity = ff,trophicGroup = tt, keyElement = kk)
#'
#' # Default calculation:
#' ww <- calculateBSunitWeights(ICunits = x, indicators = indicators, fidelity = ff,
#'                        trophicGroup = tt, keyElement = kk)
#' ww
#' rowSums(ww,na.rm=TRUE)
#'
#' # Alternatives for key indicators:
#' calculateBSunitWeights(ICunits = x, indicators = indicators, fidelity = ff,
#'                        trophicGroup = tt, keyElement = kk, keys = "asGroup")
#' calculateBSunitWeights(ICunits = x, indicators = indicators, fidelity = ff,
#'                        trophicGroup = tt, keys = "ignore")
#' calculateBSunitWeights(ICunits = x, indicators = indicators, fidelity = ff,
#'                        trophicGroup = tt, keyElement = kk, w = 0.0)
#' calculateBSunitWeights(ICunits = x, indicators = indicators, fidelity = ff,
#'                        trophicGroup = tt, keyElement = kk, w = 1.0)
#'
#' # Ignore fidelities:
#' calculateBSunitWeights(ICunits = x, indicators = indicators, fids = FALSE,
#'                        tgroups = TRUE, trophicGroup = tt, keyElement = kk)
#'
#' # Equal weighting of indicators.
#' # I.e. weights equal to the inverse of the number of indicators observed
#' # in a BSunit:
#' calculateBSunitWeights(ICunits = x, indicators = indicators, fids = FALSE,
#'                        tgroups = FALSE)
#'
#'@export
#'

calculateBSunitWeights <- function(ICunits = NULL,
                                   indicators = NULL,
                                   fidelity = NULL,
                                   trophicGroup = NULL,
                                   keyElement = NULL,
                                   fids = TRUE,
                                   tgroups = TRUE,
                                   keys = "specialWeight",
                                   w = 0.5) {

  # Check input

  if (length(ICunits) == 0) {
    stop("Required argument 'ICunits' missing with no default", call. = TRUE)
  }

  if (length(indicators) == 0) {
    stop("Required argument 'indicators' missing with no default", call. = TRUE)
  }

  missInd <- which(!(dimnames(ICunits)[[2]] %in% indicators))
  if (length(missInd) > 0) {
    mess1 <- paste("\nWeighting data missing for indicators:\n'",
                   paste(dimnames(ICunits)[[2]][missInd],collapse = "', '"),
                   "'.",sep="")
  } else {
    mess1 <- ""
  }
  missInd2 <- which(!(indicators %in% dimnames(ICunits)[[2]]))
  if (length(missInd2) > 0) {
    mess2 <- paste("\nICunits missing for indicators:\n'",
                   paste(indicators[missInd2],collapse = "', '"),
                   "'.",sep="")
  } else {
    mess2 <- ""
  }
  if (length(missInd) > 0 | length(missInd2) > 0) {
    stop(paste(mess1,"\n",mess2), call. = TRUE)
  }

  keyOptions <- c("asGroup","ignore","specialWeight")
  if (!(keys %in% keyOptions)) {
    stop(paste("\"",keys,"\" is not a valid specification of parameter 'keys'. Should be one of \"",
               paste(keyOptions,collapse = "\", \""),"\".",sep=""), call. = TRUE)
  }

  nIndicators <- length(indicators)

  if (!tgroups) {
    keyElement <- rep(FALSE,nIndicators)
    trophicGroup <- rep("Member",nIndicators)
    keys <- "none"
  }

  if (keys == "ignore") {
    keyElement <- rep(FALSE,nIndicators)
    if (length(trophicGroup) == 0) {
      stop("Required argument 'trophicGroup' missing with no default.", call. = TRUE)
    }
  }
  if (keys %in% c("asGroup","specialWeight")) {
    if (length(keyElement) == 0 & length(trophicGroup) == 0) {
      stop("Required arguments 'keyElement' and 'trophicGroup' missing with no default.", call. = TRUE)
    }
    if (length(keyElement) == 0) {
      stop("Required argument 'keyElement' missing with no default.", call. = TRUE)
    }
    if (length(trophicGroup) == 0) {
      stop("Required argument 'trophicGroup' missing with no default.", call. = TRUE)
    }
  }
  if (keys == "asGroup") {
    trophicGroup[keyElement] <- "keyIndicator"
    keyElement <- rep(FALSE,nIndicators)
  }

  if (!fids) {
    fidelity <- rep(100,nIndicators)
  } else {
    if (length(fidelity) == 0) {
      stop("Required argument 'fidelity' missing with no default.", call. = TRUE)
    }
  }

  if (w < 0 | w > 1) {
    w <- 0.5
  }

  # A local function returning the number of unique elements, ignoring NAs, in a vector

  lengthUnique <- function(x) {
    return(length(unique(x[!is.na(x)])))
  }

  # Calculate fidelity weights

  # ddd: fidelity matrix
  # ttt: trophic group matrix
  # key: key indicator matrix

  ddd <- kkk <- presence <- fidelityWeight <- trophicWeight <- ICunits*0
  ttt <- ddd
  ttt[ddd==0] <- "0"
  presence <- !is.na(ICunits)

  for (i in dimnames(ICunits)[[2]]) {
    j <- which(indicators==i)
    ddd[,i] <- fidelity[j]
    ttt[,i] <- trophicGroup[j]
    kkk[,i] <- keyElement[j]
  }

  ddd <- ddd*presence
  kkk <- kkk*presence
  ttt[!presence] <- NA
  ttt[kkk > 0] <- NA
  tttUnique <- unique(as.vector(ttt)[!is.na(as.vector(ttt))])
  ttt[is.na(ttt)] <- "Not applicable"

  fidelityWeight[kkk > 0] <- (ddd*(kkk>0)/rowSums(ddd*(kkk>0),na.rm=TRUE))[kkk > 0]

  for (i in tttUnique) {
    fidelityWeight[ttt == i] <- (ddd*(ttt == i)/rowSums(ddd*(ttt == i),na.rm=TRUE))[ttt == i]
  }

  # Calculate trophic weights

  # rrr: number of trophic groups per BSunit
  # nonkeyPresent: TRUE if nonkey indicators are present in the BSunit
  # keyPresent: TRUE if key indicators are present in the BSunit

  ttt[ttt == "Not applicable"] <- NA
  rrr <- matrix(rep(apply(ttt,1,lengthUnique),dim(ttt)[2]),ncol = dim(ttt)[2])
  nonkeyPresent <- rrr > 0
  nonkeyPresent[!(kkk > 0)] <- NA
  keyPresent <- matrix(rep((rowSums(kkk > 0,na.rm=TRUE) > 0),dim(ttt)[2]),ncol = dim(ttt)[2])
  keyPresent[is.na(ttt)] <- NA
  rrr[is.na(ttt)] <- NA

  trophicWeight[kkk > 0] <- ((kkk*w) + (kkk*(1-w)*(!nonkeyPresent))) [kkk > 0]
  trophicWeight[!is.na(ttt)] <- (((1-w)/rrr) + ((w*(!keyPresent))/rrr)) [!is.na(ttt)]

  # Calculate BSunitWeights

  BSunitWeights <- trophicWeight*fidelityWeight

  return(BSunitWeights)
}

