#' Create or test for Objects of Classes niOutput, niSeries, niValue
#'
#' \code{niOutput}, \code{niSeries}, \code{niValue} return \code{S3} objects of
#' classes \code{niOutput}, \code{niSeries}, \code{niValue} respectively.
#' \cr\code{is.niOutput}, \code{is.niSeries}, \code{is.niValue} tests if a
#' candidate object is of each of these classes.
#'
#' Lists of class \code{niOutput} contain the results from the calculation of
#' a set of (time) series of Nature Index or thematic index values for a set of
#' NIunits. The function \code{\link{calculateIndex}} returns objects of class
#' \code{niOutput}. Each element of an \code{niOutput} list is itself a list of
#' class \code{niSeries} and contains the results from the calculation of
#' a (time) series of index values for a single NIunit.
#' Finally, each element of an \code{niSeries} list is a list of
#' class \code{niValue}. Objects of class \code{niValue} contain the results from
#' the calculation of a single Nature Index or thematic index value.
#'
#' @seealso The following methods are available within \code{NIcalc}:
#' \code{\link{summary.niOutput}}, \code{\link{summary.niSeries}},
#' \code{\link{plot.niSeries}}, \code{\link{summaryWeights}},
#' \code{\link{plot.niValue}}, and \code{\link{plotWeights}}
#' \cr The vignette \code{objectsInNIcalc} gives a detailed description of S3
#' classes defined within \code{NIcalc}.
#'
#' @name niOutput
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param x a list where each element is of class \code{niSeries} (\code{niOutput}),
#'   \cr a list where each element is of class \code{niValue} (\code{niSeries}),
#'   \cr or object to be tested (\code{is.niOutput}, \code{is.niSeries},
#'   \code{is.niValue}).
#' @param indexArea string, name of NIunit.
#' @param call object of mode "call", unevaluated function call to
#'   \code{calculateIndex}.
#' @param calculationParameters list of options chosen for the calculation
#'   of indices in \code{calculateIndex}.
#' @param metadata numeric vector, \code{names(metadata) = c("nIndicators",
#'   "nBSunits","nICunits","nImputations")} containing metadata describing the
#'   input dataset.
#' @param year numeric scalar.
#' @param indicators character vector of indicator names.
#' @param indicatorData data.frame with indicator data.
#' @param ICunits integer vector of ICunit IDs.
#' @param ICunitMatrix \code{BSunit x indicator} matrix of ICunit IDs.
#' @param imputations optional data.frame listing indicators and ICunits with
#'   imputed values.
#' @param BSunits character vector of BSunit names.
#' @param BSunitData data.frame with BSunit data.
#' @param BSunitWeights numeric \code{BSunits x indicator} matrix of
#'   BSunit weights.
#' @param NIunitWeights numeric vector of NIunit weights.
#' @param BSunitIndices numeric \code{BSunit x nsim} matrix of \code{nsim} index
#'   values per BSunit.
#' @param BSunitbbb numeric vector of bbb statistics per BSunit.
#' @param indexWeights numeric \code{BSunit x indicators} matrix of (NI)weights.
#' @param index numeric vector of \code{length = nsim} index values.
#' @param bbb numeric scalar, bbb statistic.
#'
#' @return \code{niOutput} returns a list of class \code{niOutput}, where each
#'   element is of class \code{niSeries}. \cr\code{niSeries} returns a list of
#'   class \code{niSeries}, where each element is of class \code{niValue}.
#'   \cr\code{niValue} returns \code{S3} object of class \code{niValue}
#'   \cr\code{is.niOutput}, \code{is.niSeries}, and \code{is.niValue} returns
#'   \code{TRUE} if their argument is of class \code{niOutput}, \code{niSeries},
#'   and \code{niValue} respectively.
#'
NULL

#' @rdname niOutput
#' @export
niOutput <- function(x) {
  y <- match.call()

  if (sum(unlist(lapply(x,is.niSeries))) < length(x)) {
    stop(paste("Objects '",
               paste(y[2],names(x) [which(!unlist(lapply(x,is.niSeries)))],sep = "$",collapse = "', '"),
               "' not of class 'niSeries'.", sep=""), call. = TRUE)
  }

  niResults <- x

  class(niResults) <- c("niOutput","list")
  return(niResults)
}


#' @rdname niOutput
#' @export
niSeries <- function(x) {
  y <- match.call()

  if (sum(unlist(lapply(x,is.niValue))) < length(x)) {
    stop(paste("Objects '",
               paste(y[2],names(x) [which(!unlist(lapply(x,is.niValue)))],sep = "$",collapse = "', '"),
               "' not of class 'niValue'.", sep=""), call. = TRUE)
  }

  niResults <- x

  class(niResults) <- c("niSeries","list")
  return(niResults)
}

#' @rdname niOutput
#' @export
niValue <- function(
  indexArea =stop("Required argument 'indexArea' missing with no default",
                  call. = TRUE),
  call = stop("Required argument 'call' missing with no default", call. = TRUE),
  calculationParameters =
    stop("Required argument 'calculationParameters' missing",
         call. = TRUE),
  metadata = stop("Required argument 'metadata' missing with no default",
                  call. = TRUE),
  year = stop("Required argument 'year' missing with no default", call. = TRUE),
  indicators = stop("Required argument 'indicators' missing with no default",
                    call. = TRUE),
  indicatorData = stop("Required argument 'indicatorData' missing with no default",
                       call. = TRUE),
  ICunits = stop("Required argument 'ICunits' missing with no default",
                 call. = TRUE),
  ICunitMatrix = stop("Required argument 'ICunitMatrix' missing with no default",
                      call. = TRUE),
  imputations = stop("Required argument 'imputations' missing with no default",
                     call. = TRUE),
  BSunits = stop("Required argument 'BSunits' missing with no default",
                 call. = TRUE),
  BSunitData = stop("Required argument 'BSunitData' missing with no default",
                    call. = TRUE),
  BSunitWeights = stop("Required argument 'BSunitWeights' missing with no default",
                       call. = TRUE),
  NIunitWeights = stop("Required argument 'NIunitWeights' missing with no default",
                       call. = TRUE),
  BSunitIndices = stop("Required argument 'BSunitIndices' missing with no default",
                       call. = TRUE),
  BSunitbbb = stop("Required argument 'BSunitbbb' missing with no default",
                   call. = TRUE),
  indexWeights = stop("Required argument 'indexWeights' missing with no default",
                      call. = TRUE),
  index = stop("Required argument 'index' missing with no default", call. = TRUE),
  bbb = stop("Required argument 'bbb' missing with no default", call. = TRUE)) {


  niResults <- list(indexArea = indexArea,
                    call = call,
                    calculationParameters = calculationParameters,
                    metadata = metadata,
                    year = year,
                    indicators = indicators,
                    indicatorData = indicatorData,
                    ICunits = ICunits,
                    ICunitMatrix = ICunitMatrix,
                    imputations = imputations,
                    BSunits = BSunits,
                    BSunitData = BSunitData,
                    BSunitWeights = BSunitWeights,
                    NIunitWeights = NIunitWeights,
                    BSunitIndices = BSunitIndices,
                    BSunitbbb = BSunitbbb,
                    indexWeights = indexWeights,
                    index = index,
                    bbb = bbb)

  class(niResults) <- c("niValue","list")
  return(niResults)
}

#' @rdname niOutput
#' @export
is.niOutput <- function(x) c("niOutput") %in% class(x)

#' @rdname niOutput
#' @export
is.niSeries <- function(x) c("niSeries") %in% class(x)

#' @rdname niOutput
#' @export
is.niValue <- function(x) c("niValue") %in% class(x)
