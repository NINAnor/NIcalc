#' Create or test for Object of Class \code{niImputations}
#'
#' \code{niImputations} assembles arguments and returns an \code{S3} object of class
#' \code{niImputations}.
#' \cr\code{is.niImputations} tests if a candidate object is of class
#' \code{niImputations}.
#'
#' Objects of class \code{niImputations} contain imputed values for missing values
#' in a set of indicator observations.
#'
#' Class \code{niImputations} objects are generated by the function
#' \code{imputeData}.
#'
#' @seealso Functions \code{\link{imputeData}} and \code{\link{imputeDiagnostics}}.
#' \cr The vignette \code{objectsInNIcalc} gives a more detailed description of
#' \code{\link{niImputations}} lists.
#'
#' @name niImputations
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param identifiers data.frame
#' @param imputations numeric matrix of imputed indicator observations
#' @param x object to be tested (\code{is.niImputations}).
#'
#' @return \code{niImputations} returns an \code{S3} object, a list of class
#'    \code{niImputations}.\cr
#'    \cr\code{is.niImputations} returns \code{TRUE} if the argument is of class
#'    \code{niImputations}
#'
NULL

#' @rdname niImputations
#' @export
niImputations <- function(identifiers = NULL,
                          imputations = NULL) {

  if (length(identifiers) == 0) {
    stop("Required argument 'identifiers' missing with no default", call. = TRUE)
  }
  if (length(imputations) == 0) {
    stop("Required argument 'imputations' missing with no default", call. = TRUE)
  }
  x <- list("identifiers" = identifiers,
            "imputations" = imputations)


  class(x) <- c("niImputations","list")
  return(x)
}

#' @rdname niImputations
#' @export
is.niImputations <- function(x) c("niImputations") %in% class(x)
