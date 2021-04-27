#' Create or test for Objects of Class niDataImport
#'
#' \code{niDataImport} assembles arguments and returns an \code{S3} object of class
#' \code{niDataImport}. \cr\code{is.niDataImport} tests if a candidate object is of
#' class \code{niDataImport}.
#'
#' Lists of class \code{niDataImport} contain a complete data set for calculating
#' the Nature Index or a thematic index. The function \code{\link{importDatasetApi}}
#' returns objects of class \code{niDataImport} after reading the data from the
#' NI database via the
#' [The Nature Index Application Programming Interface](https://www8.nina.no/NaturindeksNiCalc/index.html).
#'
#' @seealso Function \code{\link{importDatasetApi}}. The vignette \code{objectsInNIcalc}
#' gives a more detailed description of \code{niDataImport} lists.
#'
#' @name niDataImport
#'
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param indicators tibble of indicator data.
#' @param referenceValues list of reference value data.
#' @param indicatorObservations list of observation data.
#' @param ICunits tibble of ICunit data.
#' @param BSunits tibble of BSunit data.
#' @param ecosystems tibble of ecosystem data.
#' @param x object to be tested.
#'
#' @return \code{niDataImport} returns a list of class \code{niDataImport} with elements:
#'    \tabular{ll}{\code{[[1]]} \tab \code{indicators}\cr
#'    \code{[[2]]} \tab \code{referenceValues}\cr
#'    \code{[[3]]} \tab \code{indicatorObservations}\cr
#'    \code{[[4]]} \tab \code{ICunits}\cr
#'    \code{[[5]]} \tab \code{BSunits}\cr
#'    \code{[[6]]} \tab \code{ecosystems}}
#'    \code{is.niDataImport} returns \code{TRUE} if the argument is of class
#'    \code{niDataImport}.
#'
NULL

#' @rdname niDataImport
#' @export
niDataImport <- function(indicators = NULL,
                         referenceValues = NULL,
                         indicatorObservations = NULL,
                         ICunits = NULL,
                         BSunits = NULL,
                         ecosystems = NULL) {

  if (length(indicators) == 0) {
    stop("Required argument 'indicators' missing with no default", call. = TRUE)
  }

  if (length(referenceValues) == 0) {
    stop("Required argument 'referenceValues' missing with no default", call. = TRUE)
  }

  if (length(indicatorObservations) == 0) {
    stop("Required argument 'indicatorObservations' missing with no default", call. = TRUE)
  }

  if (length(ICunits) == 0) {
    stop("Required argument 'ICunits' missing with no default", call. = TRUE)
  }

  if (length(BSunits) == 0) {
    stop("Required argument 'BSunits' missing with no default", call. = TRUE)
  }

  if (length(ecosystems) == 0) {
    stop("Required argument 'ecosystems' missing with no default", call. = TRUE)
  }

  niDataSet <- list("indicators" = indicators, "referenceValues" = referenceValues,
                    "indicatorObservations" = indicatorObservations, "ICunits" = ICunits,
                    "BSunits" = BSunits, "ecosystems" = ecosystems)

  class(niDataSet) <- c("niDataImport","list")
  return(niDataSet)
}

#' @rdname niDataImport
#' @export
is.niDataImport <- function(x) c("niDataImport") %in% class(x)
