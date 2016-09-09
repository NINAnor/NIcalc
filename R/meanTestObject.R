#' Methods for class testObject
#'
#' I think this is the wrong way to go about it.
#'
#' I don't know what more to say.
#'
#' @method mean testObject
#' @export

mean.testObject <- function(x) mean(x[["RandomNumbers"]])
