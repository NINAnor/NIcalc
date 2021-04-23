#' Imputation Diagnostics
#'
#' Performs multivariate imputation by chained equations and returns a set of
#' diagnostic plots and statistics together with the imputations.
#'
#' \code{imputeDiagnostics} is essentially the same function as
#' \code{\link{imputeData}}, but returns in addition the complete output from
#' \code{mice::mice} as well as a series of diagnostic plots.
#'
#' \code{imputeData} will generally involve lengthy calculations where the
#' \code{nSim} argument is set equal to the number of bootstrap samples in
#' subsequent calculation of an index using \code{\link{calculateIndex}}.
#'
#' \code{imputeDiagnostics} is intended to be run before \code{imputeData},
#' allowing inspection of the diagnostics using a low value for \code{nSim},
#' and thus facilitating trial runs with different settings of input arguments
#' to \code{mice::mice}.
#'
#' @seealso \code{\link{imputeData}} and \code{\link[mice:mice]{mice::mice}}.
#' \cr The vignette \code{objectsInNIcalc} gives a detailed description of
#' \code{\link{niInput}} lists.
#'
#' @name imputeDiagnostics
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @import mice
#' @import lattice
#' @importFrom grDevices grey
#'
#' @param x list of class \code{niInput}.
#' @param nSim integer, number of imputations, default is 10.
#' @param transConst numeric scalar, \code{0<transConst<=0.1}.
#'   Transformation constant in log-transformation. Default is \code{0.01}.
#' @param ... further arguments passed on to \code{mice::mice}.
#'
#' @return A list of three elements:
#'   \cr\code{identifiers}: a data.frame with variables relating each imputed
#'   indicator observation to a missing observation in the data set \code{x}.
#'   \cr\code{imp}: list with 21 elements containing the complete output
#'   from \code{mice::mice}.
#'   \cr\code{diagnostics}: list of diagnostic outputs including convergence
#'   plots, stripplots showing the distribution of imputed and observed
#'   values, and plots showing the distribution of missing values over years
#'   and indicators.
#'
#' @examples
#' \dontrun{
#' imputeDiagnostics(x = themeData,
#'                  nSim = 10,
#'                  maxit = 100)$diagnostics
#' }
#'
#' @export

imputeDiagnostics <- function(x = NULL,
                              nSim = 10,
                              transConst = 0.01,
                              ...) {

  # Control input

  y <- match.call()

  if (length(x) == 0) {
    stop("Required argument 'x' missing with no default", call. = TRUE)
  }

  if (!is.niInput(x)) {
    stop(paste("Object '",y[2],"' is not a list of class 'niInput'.",sep=""), call. = TRUE)
  }

  if (transConst > 0.1 | transConst <= 0) {
    stop(paste("Variable transConst=",transConst," out of range.",sep=""))
  }

  if (nSim <= 0 | nSim > 20) {
    nSim <- 20
  }

  # Create Imputation dataset; create data.frame with variables
  # lexpected - log of expected value
  # llower - log of lower quartile
  # lID - log of interquartile distance
  # fyears - factor of years
  # findicators - factor of indicators

  refVal <- x$referenceValues$expectedValue
  ICrefVal <- x$referenceValues$ICunitId

  ICunit <- var1 <- var2 <- var3 <- var4 <- var5 <- NULL
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

  for (i in unique(ICunit)) {
    var1[ICunit == i] <- var1[ICunit == i]/refVal[ICrefVal == i]
    var2[ICunit == i] <- var2[ICunit == i]/refVal[ICrefVal == i]
    var3[ICunit == i] <- var3[ICunit == i]/refVal[ICrefVal == i]
  }

  lexpected <- log(var1 + transConst)
  llower <- log(var2 + transConst)
  lID <- log(var3 - var2 + transConst)
  fyears <- as.factor(var4)
  findicators <- as.factor(var5)

  naData <- data.frame(lexpected,llower,lID,fyears,findicators)

  # Plots displaying missing value pattern.

  RRR <- is.na(naData$lexpected)

  missMatrix <- matrix(0,nrow=length(levels(findicators)),ncol=length(levels(fyears)),
                       dimnames = list(levels(findicators),levels(fyears)))


  for (j in 1:length(levels(fyears))) {
    for (k in 1:length(levels(findicators))) {
      missMatrix[k,j] <- sum(RRR[fyears==levels(fyears)[j] & findicators==levels(findicators)[k]]) /
        sum(fyears==levels(fyears)[j] & findicators==levels(findicators)[k])
    }
  }

  plot1 <- lattice::levelplot(t(missMatrix),
                              col.regions=grDevices::grey(15:0/15),aspect = "Fill",
                              xlab = "Year", ylab = "Indicator")

  plot2 <- lattice::histogram(~fyears|RRR, data=naData)
  plot3 <- mice::md.pattern(naData,plot=F)

  mdPatternByYear <- NULL
  for (i in levels(fyears)) {
    if (sum(is.na(naData[fyears == i,])) > 0) {
      mdPatternByYear[[i]] <- mice::md.pattern(naData[fyears == i,],plot=F)
    } else {
      mdPatternByYear[[i]] <- NULL
    }
  }

  # Multiple imputations using MICE

  message("\nMultiple imputations:\nm = ",nSim," imputations for each of ",nMissing,
          " missing indicator observations ..... ")

  nImputations <- nSim

  imp <- suppressWarnings(mice::mice(naData,
                                     m = nImputations,
                                     method = c("pmm", "pmm", "pmm", "", ""),
                                     #                             maxit = 50,
                                     #                             print = FALSE,
                                     ...))


  plot4 <- plot(imp)
  plot5 <- mice::stripplot(imp, lexpected~.imp, pch=20, cex=2)
  plot6 <- mice::stripplot(imp, llower~.imp, pch=20, cex=2)
  plot7 <- mice::stripplot(imp, lID~.imp, pch=20, cex=2)
  diagnostics <- list("mdDistribuiton" = plot1,
                      "mdHistogram" = plot2,
                      "mdPattern" = plot3,
                      "mdPatternByYear" = mdPatternByYear,
                      "convergencePlot" = plot4,
                      "stripplotExpected" = plot5,
                      "stripplotLower" = plot6,
                      "stripplotId" = plot7)

  # Select referance values

  names(refVal) <- ICrefVal
  refss <- refVal[as.character(ICunit[as.numeric(names(imp$where[,1][imp$where[,1]]))])]

  # Construct variables identifying the imputed values

  year <- var4[as.numeric(names(imp$where[,1][imp$where[,1]]))]
  indName <- var5[as.numeric(names(imp$where[,1][imp$where[,1]]))]
  ICunitId <- ICunit[as.numeric(names(imp$where[,1][imp$where[,1]]))]

  # Assemble object with imputations and diagnostics

  impData <- list(identifiers = data.frame(ICunitId,indName,year,refss,stringsAsFactors = FALSE),
                  imp = imp,
                  diagnostics = diagnostics)

  return(impData)

} #End of function imputeDiagnostics
