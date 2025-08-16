#' Multiple Imputations
#'
#' Multivariate imputation by chained equations. Add multiple imputations
#' for missing indicator observations in a Nature Index data set.
#'
#' Two general approaches for imputing multivariate data have
#' emerged: joint modeling (JM) and fully conditional specification (FCS), also
#' known as multivariate imputation by chained equations (MICE). JM involves
#' specifying a multivariate distribution for the missing data, and drawing
#' imputations from their conditional distributions by Markov Chain Monte Carlo
#' (MCMC) techniques. This methodology is attractive if the multivariate
#' distribution is a reasonable description of the data. FCS specifies the
#' multivariate imputation model on a variable-by-variable basis by a set of
#' conditional densities, one for each incomplete variable. Starting from an
#' initial imputation, FCS draws imputations by iterating over the conditional
#' densities.
#'
#' A JM approach using the R-package \code{Amelia} has been tested for missing
#' indicator observations in Nature Index data sets. This approach was, however,
#' not robust when implemented as a general method for all indicators. The
#' routine often crashed when the joint distribution model (multivariate normal)
#' was not suitable and sometimes led to fatal errors in the CPU.
#' \code{imputeData} therefore adopt the FCS approach using the routine
#' \code{\link[mice:mice]{mice::mice}}.
#'
#' \code{imputeData} performs multiple imputations for all missing indicator
#' observations. Each imputation consists of an expected value, a lower quartile
#' and the interquartile distance (ID). The upper quartile of the imputed
#' indicator observation is calculated from the lower quartile and the
#' interquartile distance.
#'
#' Indicator observations are normalized against their corresponding reference
#' value and thereafter log-transformed before imputation modeling.
#' The imputation model includes five variables in the order \code{logmean},
#' \code{loglower}, \code{logID}, \code{year}, \code{indicator}. A common
#' pattern of missing values in the data is that all data for some indicators
#' are missing for some years. This leads to empty cell problems if the
#' imputation model includes interactions between year and indicator.
#' The imputation model therefore does not contain interaction terms.
#'
#' As default, \code{imputeData} uses predictive mean matching as imputation
#' method and calls the function \code{mice} in package \code{mice} with arguments
#' \code{m = nSim} and \code{method = c("pmm", "pmm", "pmm", "", "")}.
#'
#' The argument \code{nSim} determines the number of imputations.
#' A continuous probability distribution is fitted to each imputation by
#' \code{\link{elicitate}}. \code{imputeData} draws and returns one observation
#' from each distribution.
#'
#' @seealso \code{\link{imputeDiagnostics}}, \code{\link{impStand}},
#' \code{\link[mice:mice]{mice::mice}}, \code{\link{elicitate}}, and
#' \code{\link{calculateIndex}}. \cr The vignette \code{objectsInNIcalc} gives a
#' more detailed description of \code{\link{niImputations}} and
#' \code{\link{niInput}} lists.
#'
#' @name imputeData
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @import mice
#'
#' @param x list of class \code{niInput}.
#' @param nSim integer, number of imputations, default is 1000.
#' @param transConst numeric scalar, \code{0<transConst<=0.1}.
#'   Transformation constant in log-transformation. Default is \code{0.01}.
#' @param mice_threshold_alt numeric scalar, alternative threshold to try with
#' `mice::mice()` if the default (0.999) fails. Set to default to 0.9999.
#' @param ... further arguments passed on to \code{mice::mice}.
#'
#' @return A list of class \code{niImputations} containing two elements:
#'   \cr\code{identifiers}: a data.frame with variables relating each imputed
#'   indicator observation to a missing observation in the data set \code{x}.
#'   \cr\code{imputations}: a numeric matrix where each row represents a missing
#'   indicator observation in the corresponding data set and contains single
#'   draws from each of \code{nSim} imputed distributions.
#'
#' @examples
#' \dontrun{
#' imputedValues <- imputeData(x = themeData,
#'                                 nSim = 1000,
#'                                 transConst = 0.01,
#'                                 maxit = 20,
#'                                 printFlag = TRUE)
#' }
#'
#' @export

imputeData <- function(x = NULL,
                       nSim = 1000,
                       transConst = 0.01,
                       mice_threshold_alt = 0.9999,
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

  if (nSim <= 0) {
    stop(paste("Invalid parameter nSim =",nSim,". nSim must be a positive integer"))
  }

  y <- match.call()
  if (!is.niInput(x)) {
    stop(paste("Object '",y[2], "' is not a list of class 'niInput'.",sep=""),
         call. = TRUE)
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

  # Identify indicators reported without uncertainty (expected = llower = lupper)

  indNames <- unique(naData$findicators)
  noVar <- rep(FALSE, length(indNames))
  for(i in 1:length(indNames)){
    indData <- subset(naData, findicators == indNames[i])
    if(identical(indData$lexpected, indData$llower) & all(indData$lID %in% c(log(transConst), NA)))
    noVar[i] <- TRUE
  }
  ind_noVar <- indNames[which(noVar)]

  # Issue warning if dataset contains a mixture of indicators with and without uncertainty
  if(dplyr::between(length(ind_noVar), 1, (length(indNames)-1))){
    warning("Dataset contains a mixture of indicators provided with and without uncertainty. \nThis may lead to bias in imputation of missing values.")
  }

  # Multiple imputations using MICE

  message("\nMultiple imputations:\nm = ",nSim," imputations for each of ",nMissing,
          " missing indicator observations ..... ")

  nImputations <- nSim

  imp <- suppressWarnings(mice::mice(naData,
                                     m = nImputations,
                                     method = c("pmm", "pmm", "pmm", "", ""),
                                     print = TRUE,
                                     ...))

  # Check imputations for NA (= failure) & re-run with alternative threshold if needed
  impute_fail <- ifelse(any(is.na(cbind(imp$imp$lexpected, imp$imp$llower, imp$imp$lID))), TRUE, FALSE)

  if(impute_fail){

    message(paste0("Imputation failed with standard setup. Retrying with threshold = ", mice_threshold_alt))
    imp <- suppressWarnings(mice::mice(naData,
                                       m = nImputations,
                                       method = c("pmm", "pmm", "pmm", "", ""),
                                       threshold = mice_threshold_alt,
                                       print = TRUE,
                                       ...))

    impute_fail2 <- ifelse(any(is.na(cbind(imp$imp$lexpected, imp$imp$llower, imp$imp$lID))), TRUE, FALSE)
    if(impute_fail2){
      stop(paste0("Imputation failed with alternative threshold = ", mice_threshold_alt,
                  ".\n You may try with a higher threshold, but be aware that this issue indicates that mice::mice is not operating the way it should.\n You may instead want to reconsider your input dataset and the need for/sensibility of imputation."))
    }
  }

  # Backtransform imputations

  var1Imp <- (exp(imp$imp$lexpected) - transConst)[,1:nImputations]

  if(all(indNames %in% ind_noVar)){ # If all indicators in set were reported without uncertainty...
    var2Imp <- var1Imp # ... set imputed "lower" to same as "expected"
    var3Imp <- var1Imp # ... set imputed "upper" to same as "expected"
    warning("Dataset consists of only indicators that were reported without uncertainty. \n Imputed distributions are therefore elicited using expected values only (lower and upper bounds assumed equal to expected value). ")
  }else{
    var2Imp <- (exp(imp$imp$llower) - transConst)[,1:nImputations]
    var3Imp <- (exp(imp$imp$lID) - transConst)[,1:nImputations] + var2Imp
  }

  # Multiply with reference values

  names(refVal) <- ICrefVal
  refss <- refVal[as.character(ICunit[as.numeric(names(imp$where[,1][imp$where[,1]]))])]

  var1Imp <- var1Imp*t(refss)
  var2Imp <- var2Imp*t(refss)
  var3Imp <- var3Imp*t(refss)

  # Construct variables that will later help identifying the imputed values when
  # calculating the Nature Index - Year, Indicator and ICunitID

  year <- var4[as.numeric(names(imp$where[,1][imp$where[,1]]))]
  indName <- var5[as.numeric(names(imp$where[,1][imp$where[,1]]))]
  ICunitId <- ICunit[as.numeric(names(imp$where[,1][imp$where[,1]]))]

  # Elicitate imputed indicator observations

  message("\nSampling n = ",1," draw from each of ",nSim,
          " imputed distributions for each of ",length(refss),
          " missing indicator observations ... \n")

  bootmat <- matrix(NA, nrow = length(refss), ncol = nSim, dimnames = NULL)
  for (i in 1:nSim) {
    distributions <- elicitate(expected.value = var1Imp[,i],
                               lower = var2Imp[,i],
                               upper = var3Imp[,i],
                               type.t = c("continuous"), prob.quant = c(0.25,0.75)) [,1:3]

    # Sample one observation from each distribution. May need to be changed
    # samplebootmat is a function in NIcalc

    bootmat[,i] <- samplebootmat(ValueID = 1:length(var1Imp[,i]),
                                 Value = var1Imp[,i],
                                 RefobsID = 1:length(var1Imp[,i]),
                                 DistID = distributions[,1],
                                 mu = distributions[,2],
                                 sig = distributions[,3],
                                 ref.value.code = 0,
                                 Type.of.uncertainty = c("Observations"),
                                 nsim = 1)

  }

  # Assemble object with imputed values

  impData <- niImputations(identifiers = data.frame(ICunitId,indName,year,refss,stringsAsFactors = FALSE),
                           imputations = bootmat)

  return(impData)

} #End of function imputeData
