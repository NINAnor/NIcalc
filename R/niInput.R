#' Create or test for Object of Class \code{niInput}
#'
#' \code{niInput} assembles arguments and returns an \code{S3} object of class
#' \code{niInput}.
#' \cr\code{is.niInput} tests if a candidate object is of class \code{niInput}.
#'
#' Lists of class \code{niInput} contain data sets that are controlled for consistency
#' and for including all necessary data for calculating the Nature Index or a
#' thematic index. \code{niInput} objects have the particular structure and format
#' required for data input to the function \code{calculateIndex}. The function
#' \code{assembleNiObject} returns objects of class \code{niInput}.
#'
#' @seealso \code{\link{assembleNiObject}}, \code{\link{calculateIndex}}.
#' \cr The vignette \code{objectsInNIcalc} gives a more detailed description
#' of \code{niInput} lists.
#'
#' @name niInput
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param indicators data.frame of indicator data.
#' @param ICunits integer BSunit x indicator matrix of ICunits.
#' @param BSunits data.frame of BSunit data.
#' @param referenceValues data.frame of reference values.
#' @param indicatorValues list of data.frames of indicator values.
#' @param NIunits 0/1 BSunit x NIunit matrix of NIunits.
#' @param x object to be tested (\code{is.niInput})
#'
#' @return \code{niInput} returns a list of class \code{niInput} with elements:
#'    \tabular{ll}{\code{[[1]]} \tab \code{indicators}\cr
#'    \code{[[2]]} \tab \code{ICunits}\cr
#'    \code{[[3]]} \tab \code{BSunits}\cr
#'    \code{[[4]]} \tab \code{referenceValues}\cr
#'    \code{[[5]]} \tab \code{indicatorValues}\cr
#'    \code{[[6]]} \tab \code{NIunits}}
#'    \code{is.niInput} returns \code{TRUE} if the argument is of class
#'    \code{niInput}
#'
NULL

#' @rdname niInput
#' @export
niInput <- function(indicators=NULL,
                    ICunits = NULL,
                    BSunits = NULL,
                    referenceValues = NULL,
                    indicatorValues = NULL,
                    NIunits = NULL) {

  if (length(indicators) == 0) {
    stop("Required argument 'indicators' missing with no default", call. = TRUE)
  }

  if (length(referenceValues) == 0) {
    stop("Required argument 'referenceValues' missing with no default", call. = TRUE)
  }

  if (length(indicatorValues) == 0) {
    stop("Required argument 'indicatorValues' missing with no default", call. = TRUE)
  }

  if (length(ICunits) == 0) {
    stop("Required argument 'ICunits' missing with no default", call. = TRUE)
  }

  if (length(BSunits) == 0) {
    stop("Required argument 'BSunits' missing with no default", call. = TRUE)
  }

  niDataSet <- list("indicators" = indicators, "ICunits" = ICunits, "BSunits" = BSunits,
                    "referenceValues" = referenceValues,
                    "indicatorValues" = indicatorValues,
                    "NIunits" = NIunits)

  class(niDataSet) <- c("niInput","list")
  return(niDataSet)
}

#' @rdname niInput
#' @export
is.niInput <- function(x) c("niInput") %in% class(x)
