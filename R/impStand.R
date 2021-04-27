#' Standardize Imputations
#'
#' Optional *post hoc* adjustment of sets of multiple imputations for a set of
#' missing indicator observations.
#'
#' In some instances, especially when working with very unbalanced data, the
#' imputation model used in \code{\link{imputeData}} may not produce neutral
#' imputations as desired. In such cases, *post hoc* adjustment of imputed
#' values may be used to avoid that imputations introduce new patterns in the
#' data set. In this context, neutral values are imputations with an expected
#' deviation from the (grand) mean equal to the sum of 1) the mean deviation for
#' observations from the year in question, and 2) the mean deviation from annual
#' means for observations of the indicator in question.
#'
#' @seealso \code{\link{imputeData}}, \code{\link{imputeDiagnostics}},
#' \code{\link[mice:mice]{mice::mice}}. \cr The vignette \code{objectsInNIcalc}
#' gives a detailed description of \code{\link{niImputations}} and
#' \code{\link{niInput}} lists.
#'
#' @name impStand
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @importFrom stats median
#'
#' @param x list of class \code{niInput}.
#' @param imputations list of class \code{niImputations}
#'
#' @return A list of class \code{niImputations}
#'
#' @examples
#' \dontrun{
#' imputedValues <- imputeData(x = themeData,
#'                             nSim = 1000,
#'                             transConst = 0.01,
#'                             maxit = 20,
#'                             printFlag = TRUE)
#' imputedValues <- impStand(x = themeData,
#'                           imputations = imputedValues)
#' }
#'
#' @export

impStand <- function(x = NULL,
                     imputations = NULL) {


  meanImp <- rowMeans(imputations$imputations)
  medImp <- apply(imputations$imputations,1,stats::median)
  refVal <- x$referenceValues$expectedValue
  ICrefVal <- x$referenceValues$ICunitId

  ICunit <- var1 <- var2 <- var3 <- var4 <- var5 <- stvar1 <- NULL
  for (i in names(x$indicatorValues)) {
    ICunit <- c(ICunit,x$indicatorValues[[i]]$ICunitId)
    var1 <- c(var1,x$indicatorValues[[i]]$expectedValue)
    var2 <- c(var2,x$indicatorValues[[i]]$lowerQuantile)
    var3 <- c(var3,x$indicatorValues[[i]]$upperQuantile)
    var4 <- c(var4,x$indicatorValues[[i]]$yearName)
    var5 <- c(var5,x$indicatorValues[[i]]$indName)
  }

  # Some missing values may be given as a negative number in dataset x

  var1[var1 < 0] <- NA
  var2[var2 < 0] <- NA
  var3[var3 < 0] <- NA
  var2[is.na(var1)] <- NA
  var3[is.na(var1)] <- NA

  nMissing <- sum(is.na(var1))

  # Transform variables to be imputed

  standardisation1 <- tapply(var1[!is.na(var1)],ICunit[!is.na(var1)],mean)

  for (i in unique(ICunit)) {
    stvar1[ICunit == i] <- (var1[ICunit == i] - standardisation1[names(standardisation1) == i])/
      standardisation1[names(standardisation1) == i]
  }
  stvar1[is.nan(stvar1)] <- 0

  standardisation22 <- tapply(stvar1[!is.na(var1)],paste(var4[!is.na(var1)],var5[!is.na(var1)]),mean)
  #standardisation2 <- tapply(stvar1[!is.na(var1)],var4[!is.na(var1)],mean)

  nnams <- substr(names(standardisation22),1,4)
  standardisation2 <- tapply(standardisation22,nnams,mean)

  standardisation <- matrix(0, nrow = length(unique(ICunit)),ncol = length(unique(var4)))
  dimnames(standardisation) <- list(unique(ICunit),unique(var4))
  for (i in as.character(unique(ICunit))) {
    for (j in unique(var4)) {
      standardisation[i,j] <- standardisation1[names(standardisation1) == i] *
        (1 + standardisation2[names(standardisation2) == j])
    }
  }

  newboot <- imputations$imputations*0
  for (i in 1:length(imputations$identifiers$ICunitId)) {
    j <- as.character(imputations$identifiers$ICunitId[i])
    k <- imputations$identifiers$year[i]
    newboot[i,] <- imputations$imputations[i,] * standardisation[j,k]/medImp[i]
  }

  newImputes <- imputations
  newImputes$imputations <- newboot

  return(newImputes)
}
