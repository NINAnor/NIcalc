#' Assemble Data Set
#'
#' Assembles and structures data into a complete and consistent data set
#' for calculating the Nature Index or thematic indices.
#'
#' \code{assembleNiObject} assembles and restructures a data set into a list of
#' class \code{\link{niInput}} or a list of such lists. Most of the data are
#' typically imported from the Nature Index database by use of
#' \code{\link{importDatasetApi}}, except the delineation of NIunits (see below).
#' \code{niInput} objects have the particular structure and format required for
#' data input to the function \code{\link{calculateIndex}}.
#'
#' NIunits are geographical areas for which the indices are to be calculated.
#' NIunits may be defined in two ways, either by choosing from predefined units
#' (argument \code{predefNIunits}), and/or entered as a data.frame with 'user
#' defined' units (argument \code{NIunits}). The data.frame should contain
#' variables named \code{NIunitName} and \code{BSunitId} naming NIunits and
#' defining them in terms of BSunits.
#'
#' The data set to be assembled may be entered either as a list assigned to the
#' argument \code{inputData} containing all data elements (see \code{Arguments} above),
#' or as several objects, one for each element, assigned to other function arguments.
#'
#' The structure of the assembled data set depends on what type of index to be
#' calculated (argument \code{indexType}): the Nature Index for major
#' ecosystem(s) (\code{indexType = "ecosystem"}, default), a thematic index
#' (\code{indexType = "thematic"}), or a thematic index calculated for a set of
#' major ecosystems (\code{indexType = "thematic per ecosystem"}).
#'
#' Parameters \code{part}, \code{total}, and \code{partOfTotal} may be used
#' to restrict the data set to BSunits satisfying: \code{part/total > partOfTotal},
#' where \code{part} and \code{total} are names of variables in the
#' \code{BSunits} data.frame containing area data.
#'
#' \code{assembleNiObject} calls \code{\link{checkInputData}} to check if the
#' data set contains all necessary information for calculating the Nature Index,
#' and whether the data are consistent.
#'
#' \code{assembleNiObject} also calls \code{\link{elicitate}} to elicitate all
#' observations in the data set that are not given as custom distributions.
#'
#' @seealso \code{\link{checkInputData}}, \code{\link{elicitate}},
#' and \code{\link{calculateIndex}}.
#' \cr The vignette \code{objectsInNIcalc} gives a more detailed description
#' of \code{\link{niDataImport}} and \code{\link{niInput}} lists.
#'
#' @name assembleNiObject
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @importFrom stats quantile
#' @importFrom distr r
#' @importFrom distr Lnorm
#' @importFrom distr Pois
#' @importFrom distr DiscreteDistribution
#' @importFrom distr EmpiricalDistribution
#'
#' @param inputData	list with elements named
#'   \tabular{ll}{\code{[[1]]} \tab \code{"indicators"},\cr
#'                \code{[[2]]} \tab \code{"referenceValues"},\cr
#'                \code{[[3]]} \tab \code{"indicatorObservations"},\cr
#'                \code{[[4]]} \tab \code{"ICunits"},\cr
#'                \code{[[5]]} \tab \code{"BSunits"},\cr
#'                \code{[[6]]} \tab \code{"ecosystems"},\cr
#'                \code{[[7]]} \tab \code{"NIunits"},}
#'   where the second and third elements are lists, while the others are data.frames.
#'   The last two element, \code{ecosystems} and \code{NIunits}, may be
#'   omitted or set to \code{NULL}. For example lists of class \code{niDataImport}.
#' @param indicators	data.frame containing indicator characteristics.
#' @param referenceValues	list containing reference value characteristics.
#' @param indicatorObservations	list containing indicator observations characteristics.
#' @param ICunits	data.frame containing ICunits characteristics.
#' @param BSunits	data.frame containing BSunits characteristics.
#' @param ecosystems	data.frame containing ecosystems characteristics.
#' @param NIunits	data.frame containing characteristics of user defined NIunits.
#' @param predefNIunits named logical vector defining NIunits to be associated
#'   with the data set. Default settings are
#'   \code{c(allArea = T, parts = F, counties = F)}
#' @param indexType character. Valid \code{indexTypes} are
#'   \code{"ecosystem"} (default), \code{"thematic"}, \code{"thematic per ecosystem"}.
#' @param part character, one of \code{c("ecosystem","marine","terrestrial")}.
#'   Numerator variable in criterion for selecting BSunits.
#'   \cr\code{"ecosystem"}: area of major ecosystem in BSunit.
#'   \cr\code{"marine"}: marine area \cr\code{"terrestrial"}: terrestrial area.
#' @param total character, one of \code{c("marine","terrestrial","total")}.
#'   Denominator variable in criterion for selecting BSunits.
#'   \cr\code{"total"}: total area of BSunit.
#' @param partOfTotal numerical scalar in \code{[0,1]}
#'
#' @return A list of class \code{\link{niInput}} with elements
#'   \tabular{lll}{\code{[[1]]} \tab \code{indicators} \tab data.frame with indicator data.\cr
#'   \code{[[2]]} \tab \code{ICunits} \tab integer BSunit x indicator matrix of ICunits.\cr
#'   \code{[[3]]} \tab \code{BSunits} \tab data.frame of BSunit data.\cr
#'   \code{[[4]]} \tab \code{referenceValues} \tab data.frame of reference values.\cr
#'   \code{[[5]]} \tab \code{indicatorValues} \tab list of data.frames of indicator values.\cr
#'   \code{[[6]]} \tab \code{NIunits} \tab 0/1 BSunit x NIunit matrix of NIunits.}
#'
#' @examples
#' \dontrun{
#' carnivoreImport <- importDatasetApi(
#'      username = "...",
#'      password = "...",
#'      indic = c("Jerv","Gaupe","Ulv"),
#'      year = c("1990","2000","2010","2014","2019"))
#' carnivoreInput <- assembleNiObject(inputData = carnivoreImport,
#'                     predefNIunits = c(allArea = T, parts = T, counties = F),
#'                     indexType = "thematic")
#'
#' mountainImport <- importDatasetApi(
#'      username = "...",
#'      password = "...",
#'      eco = "Fjell",
#'      year = c("1990","2000","2010","2014","2019"))
#' mountainInput <- assembleNiObject(inputData = mountainImport,
#'                     predefNIunits = c(allArea = T, parts = T, counties = F),
#'                     indexType = "ecosystem",
#'                     part = "ecosystem",
#'                     total = "terrestrial",
#'                     partOfTotal = 0.2))
#' }
#'
#'@export
#'

assembleNiObject <- function(inputData = NULL,
                             indicators = NULL,
                             referenceValues = NULL,
                             indicatorObservations = NULL,
                             ICunits = NULL,
                             BSunits = NULL,
                             ecosystems = NULL,
                             NIunits = NULL,
                             predefNIunits = c(allArea = T, parts = F, counties = F),
                             indexType = "ecosystem",
                             part = "ecosystem",
                             total = "total",
                             partOfTotal = 0) {

  nErrors <- 0
  errorMessages <- NULL
  y <- match.call()

  if (length(inputData) > 0 & (!("list" %in% class(inputData)))) {
    nErrors <- nErrors + 1
    errorMessages[nErrors] <- (paste("inputData ",y["inputData"]," is not a list.",sep=""))
  }

  if (length(indicators) > 0 & (!("data.frame" %in% class(indicators)))) {
    nErrors <- nErrors + 1
    errorMessages[nErrors] <- paste("indicators ",y["indicators"]," is not a dataframe.",sep="")
  }

  if (length(referenceValues) > 0 & (!("list" %in% class(referenceValues)))) {
    nErrors <- nErrors + 1
    errorMessages[nErrors] <- paste("referenceValues ",y["referenceValues"]," is not a list.",sep="")
  }

  if (length(indicatorObservations) > 0 & (!("list" %in% class(indicatorObservations)))) {
    nErrors <- nErrors + 1
    errorMessages[nErrors] <- paste("indicatorObservations ",y["indicatorObservations"]," is not a list.",sep="")
  }

  if (length(ICunits) > 0 & (!("data.frame" %in% class(ICunits)))) {
    nErrors <- nErrors + 1
    errorMessages[nErrors] <- paste("ICunits ",y["ICunits"]," is not a dataframe.",sep="")
  }

  if (length(BSunits) > 0 & (!("data.frame" %in% class(BSunits)))) {
    nErrors <- nErrors + 1
    errorMessages[nErrors] <- paste("BSunits ",y["BSunits"]," is not a dataframe.",sep="")
  }

  if (length(ecosystems) > 0 & (!("data.frame" %in% class(ecosystems)))) {
    nErrors <- nErrors + 1
    errorMessages[nErrors] <- paste("ecosystems ",y["ecosystems"]," is not a dataframe.",sep="")
  }

  if (length(NIunits) > 0 & (!("data.frame" %in% class(NIunits)))) {
    nErrors <- nErrors + 1
    errorMessages[nErrors] <- paste("NIunits ",y["NIunits"]," is not a dataframe.",sep="")
  }

  if (nErrors > 0) {
    stop(paste("\n",nErrors," error(s) found in dataset:\n\n",paste(errorMessages,collapse="\n\n"),"\n",sep=""),file="")
  }

  validIndexTypes <- c("ecosystem","thematic","thematic per ecosystem")
  if (!(indexType %in% validIndexTypes)) {
    stop(paste("'",indexType, "' is not a valid indexType. Should be one of '",
               paste(validIndexTypes,collapse="', '"),"'.",
               sep=""))
  }

  if (length(inputData) == 0) {
    inputData <- list(indicators = indicators, referenceValues = referenceValues,
                      indicatorObservations = indicatorObservations, ICunits = ICunits,
                      BSunits = BSunits, ecosystems = ecosystems, NIunits = NIunits)
  } else if (length(NIunits) != 0) {
    inputData[[length(inputData)+1]] <- NIunits
    names(inputData) <- c(names(inputData)[1:(length(inputData)-1)],"NIunits")
  }

  # Check consistency of input data

  message("\nCheck input data ..... ")

  hhh <- checkInputData(inputData = inputData)

  message("\nElicitate incomplete reference values and indicator observations ..... ")

  # Elicitate reference values and indicator observations which lacks both elicitation results and
  # custom distributions. Also estimate expected value (and quartiles) from custom distributions.

  # Reference values

  noValue <- (is.na(hhh$referenceValues$referenceValues$expectedValue) |
                is.na(hhh$referenceValues$referenceValues$upperQuantile) |
                is.na(hhh$referenceValues$referenceValues$lowerQuantile))

  noValue2 <- is.na(hhh$referenceValues$referenceValues$expectedValue)

  noCustom <- is.na(hhh$referenceValues$referenceValues$customDistributionUUID)

  noDistribution <- is.na(hhh$referenceValues$referenceValues$distributionFamilyName)

  doElicitation <- noCustom & (!noValue)
  doEstimValue <- noValue2 & (!noCustom)

  if (sum(is.na(hhh$referenceValues$referenceValues$distributionFamilyName))==
      (length(hhh$referenceValues$referenceValues$distributionFamilyName))) {
    mode(hhh$referenceValues$referenceValues$distributionFamilyName) <- "character"
    mode(hhh$referenceValues$referenceValues$distParameter1) <- "double"
    mode(hhh$referenceValues$referenceValues$distParameter2) <- "double"
  }

  hhh$referenceValues$referenceValues[doElicitation,c("distributionFamilyName","distParameter1","distParameter2")] <-
    elicitate(expected.value=hhh$referenceValues$referenceValues$expectedValue[doElicitation],
              lower=hhh$referenceValues$referenceValues$lowerQuantile[doElicitation],
              upper = hhh$referenceValues$referenceValues$upperQuantile[doElicitation],
              type.t = c("continuous"), prob.quant = c(0.25,0.75)) [,1:3]

  sampleDistribution <- function(dist, nSamples = 10){
    out <- distr::r(dist)(nSamples)
    return(out)
  }

  meanAndQuantiles <- function(x,lower=0.25,upper = 0.75) {
    out <- c(mean(x),stats::quantile(x,c(lower,upper)))
    return(out)
  }

  onlyQuantiles <- function(x,lower=0.25,upper = 0.75) {
    out <- stats::quantile(x,c(lower,upper))
    return(out)
  }

  if (sum(doEstimValue) > 0) {
    hhh$referenceValues$referenceValues[doEstimValue,c("expectedValue","lowerQuantile","upperQuantile")] <-
      matrix(unlist(
        lapply(
          lapply(
            hhh$referenceValues$customDistributions[
              hhh$referenceValues$referenceValues$customDistributionUUID[doEstimValue]],
            sampleDistribution,nSamples = 10000),
          meanAndQuantiles)),
        nrow=3,byrow = T)
  }

  # Indicator values

  noValue <- (is.na(hhh$indicatorObservations$indicatorValues$expectedValue) |
                is.na(hhh$indicatorObservations$indicatorValues$upperQuantile) |
                is.na(hhh$indicatorObservations$indicatorValues$lowerQuantile))

  noValue2 <- is.na(hhh$indicatorObservations$indicatorValues$expectedValue)

  noQuantile <- (is.na(hhh$indicatorObservations$indicatorValues$upperQuantile) |
                   is.na(hhh$indicatorObservations$indicatorValues$lowerQuantile))

  noCustom <- is.na(hhh$indicatorObservations$indicatorValues$customDistributionUUID)

  noDistribution <- is.na(hhh$indicatorObservations$indicatorValues$distributionFamilyName)

  doElicitation <- noCustom & (!noValue)
  doEstimValue <- noValue2 & (!noCustom)
  doEstimQuantile <- noQuantile & (!noCustom)

  if (sum(is.na(hhh$indicatorObservations$indicatorValues$distributionFamilyName))==
      (length(hhh$indicatorObservations$indicatorValues$distributionFamilyName))) {
    mode(hhh$indicatorObservations$indicatorValues$distributionFamilyName) <- "character"
    mode(hhh$indicatorObservations$indicatorValues$distParameter1) <- "double"
    mode(hhh$indicatorObservations$indicatorValues$distParameter2) <- "double"
  }

  hhh$indicatorObservations$indicatorValues[doElicitation,c("distributionFamilyName","distParameter1","distParameter2")] <-
    elicitate(expected.value=hhh$indicatorObservations$indicatorValues$expectedValue[doElicitation],
              lower=hhh$indicatorObservations$indicatorValues$lowerQuantile[doElicitation],
              upper = hhh$indicatorObservations$indicatorValues$upperQuantile[doElicitation],
              type.t = c("continuous"), prob.quant = c(0.25,0.75)) [,1:3]

  if (sum(doEstimValue) > 0) {
    hhh$indicatorObservations$indicatorValues[doEstimValue,c("expectedValue","lowerQuantile","upperQuantile")] <-
      matrix(unlist(
        lapply(
          lapply(
            hhh$indicatorObservations$customDistributions[
              hhh$indicatorObservations$indicatorValues$customDistributionUUID[doEstimValue]],
            sampleDistribution,nSamples = 100000),
          meanAndQuantiles)),
        ncol=3,byrow = T)
  }

  if (sum(doEstimQuantile) > 0) {
    if (sum(is.na(hhh$indicatorObservations$indicatorValues$lowerQuantile)) ==
        length(hhh$indicatorObservations$indicatorValues$lowerQuantile)) {
      mode(hhh$indicatorObservations$indicatorValues$lowerQuantile) <- "double"
    }
    if (sum(is.na(hhh$indicatorObservations$indicatorValues$upperQuantile)) ==
        length(hhh$indicatorObservations$indicatorValues$upperQuantile)) {
      mode(hhh$indicatorObservations$indicatorValues$upperQuantile) <- "double"
    }
    message("\nFirst attempt to estimate quartiles (fast routine using lapply) ..... ")
    a <- try(matrix(unlist(
      lapply(
        lapply(
          hhh$indicatorObservations$customDistributions[
            hhh$indicatorObservations$indicatorValues$customDistributionUUID[doEstimQuantile]],
          sampleDistribution,nSamples = 100000),
        onlyQuantiles)),
      ncol=2,byrow = T))
    if (length(a)==1) {
      message("\nNew attempt to estimate quartiles (slow routine using for loop) ..... ")
      kkk <- which(doEstimQuantile)
      for (i in kkk) {
        j <- hhh$indicatorObservations$indicatorValues$customDistributionUUID[i]
        hhh$indicatorObservations$indicatorValues[i,c("lowerQuantile","upperQuantile")] <-
          onlyQuantiles(sampleDistribution(hhh$indicatorObservations$customDistributions[[j]],
                                           nSamples = 100000))
      }
    } else {
      hhh$indicatorObservations$indicatorValues[doEstimQuantile,
                                                c("lowerQuantile","upperQuantile")] <- a
    }
  }

  # How many output objects should be assmebled from the input data?
  # One object of type niInput should be returned per ecosystem, except when indexType = "thematic".

  nOutputObjects <- 1
  if (indexType %in% c("ecosystem","thematic per ecosystem")) {
    if (length(hhh$ecosystems) > 0) {
      nOutputObjects <- length(hhh$ecosystems$name)
    }
  }

  # How many years are represented in input data?
  # Determines the options available for treating missing indicator observations.
  # Determines the structure of the assembeled niInput object

  nYears <- length(unique(hhh$indicatorObservations$indicatorValues$yearName[
    !(is.na(hhh$indicatorObservations$indicatorValues$yearName) |
        is.nan(hhh$indicatorObservations$indicatorValues$yearName))]))

  # How to select BSunits

  selectBasicUnits <- function(BSunits = NULL,
                               econame = NULL,
                               part = "ecosystem",
                               total = "total",
                               partOfTotal = 0) {

    validPartTypes <- c("ecosystem","marine","terrestrial")
    if (!(part %in% validPartTypes)) {
      stop(paste("'",part, "' is not valid for parameter 'part'. Should be one of '",
                 paste(validPartTypes,collapse="', '"),"'.",
                 sep=""))
    }

    parts <- rowSums(t(t(BSunits[,econame])))

    if (part %in% c("marine","terrestrial")) {
      validTotalTypes <- c("total")
      if (!(total %in% validTotalTypes)) {
        stop(paste("'",total, "' is not a valid for parameter 'total'. Should be '",
                   paste(validTotalTypes,collapse="', '"),"'.",
                   sep=""))
      }
      parts <- BSunits$terrestrialArea
      if (part == "marine") {parts <- BSunits$marineArea}
      totals <- BSunits$totalArea
    } else {
      validTotalTypes <- c("marine","terrestrial","total")
      if (!(total %in% validTotalTypes)) {
        stop(paste("'",total, "' is not a valid for parameter 'total'. Should be one of '",
                   paste(validTotalTypes,collapse="', '"),"'.",
                   sep=""))
      }
      totals <- BSunits$totalArea
      if (total == "marine") {totals <- BSunits$marineArea}
      if (total == "terrestrial") {totals <- BSunits$terrestrialArea}
    }

    basicAreas <-  BSunits[parts/totals > partOfTotal,]

    return(basicAreas)
  }

  # Split data into 'nOutputObjects' datasets.

  message("\nAssemble dataset ..... \n")

  out <- NULL

  for (ii in 1:nOutputObjects) {
    econame <- "fidelity"
    if (nOutputObjects == 1 & length(hhh$ecosystems) > 0) {
      econame <- hhh$ecosystems$name
    }
    if (nOutputObjects > 1 & length(hhh$ecosystems) > 0) {
      econame <- hhh$ecosystems$name[ii]
    }
    indics <- hhh$indicators[rowSums(t(t(hhh$indicators[,econame]))) > 0,]
    indicAreas <- hhh$ICunits[hhh$ICunits$indId %in% indics$id,]
    refValues <- hhh$referenceValues$referenceValues[hhh$referenceValues$referenceValues$ICunitId %in% indicAreas$id,]
    refCustom <- hhh$referenceValues$customDistributions[
      names(hhh$referenceValues$customDistributions) %in% refValues$customDistributionUUID]
    indicValues <- hhh$indicatorObservations$indicatorValues[
      hhh$indicatorObservations$indicatorValues$ICunitId %in% indicAreas$id,]
    indicCustom <- hhh$indicatorObservations$customDistributions[
      names(hhh$indicatorObservations$customDistributions) %in% indicValues$customDistributionUUID]
    xxx <- unique(indicAreas$BSunitId)
    basicAreas <- hhh$BSunits[hhh$BSunits$id %in% xxx,]
    basicAreas <-  selectBasicUnits(basicAreas,econame,part,total,partOfTotal)
    indicAreas <- indicAreas[indicAreas$BSunitId %in% basicAreas$id,]
    indics <- indics[indics$id %in% indicAreas$indId,]
    refValues <- refValues[refValues$ICunitId %in% indicAreas$id,]
    refCustom <- refCustom[names(refCustom) %in% refValues$customDistributionUUID]
    indicValues <- indicValues[indicValues$ICunitId %in% indicAreas$id,]
    indicCustom <- indicCustom[names(indicCustom) %in% indicValues$customDistributionUUID]
    addNIareas <- FALSE
    if (length(hhh$NIunits) > 0) {
      addNIareas <- TRUE
      NIareas <- hhh$NIunits[hhh$NIunits$BSunitId %in% basicAreas$id,]
    }

    # Assemble data object

    # 1) Indicators

    matchNames <- c("id", "name","keyElement","functionalGroup","functionalGroupId","scalingModel","scalingModelId",econame)
    dataFound <- which(match(matchNames, table = names(indics), nomatch = 0) != 0)

    indics <- indics[sort.list(indics$name),matchNames[dataFound]]

    # 2) ICunits

    ICunitsData <- matrix(data = NA,
                          nrow = length(basicAreas$name),
                          ncol = length(indics$name),
                          byrow = FALSE,
                          dimnames = list(basicAreas$name,indics$name))

    for (j in 1:length(indicAreas$id)) {
      yyy <- basicAreas$name[which(basicAreas$id == indicAreas$BSunitId[j])]
      xxx <- indics$name[which(indics$id == indicAreas$indId[j])]
      ICunitsData[yyy,xxx] <- indicAreas$id[j]
    }

    # 3) BSunits


    # 4) Indicator values

    indicatorValuesNy <- NULL
    for (i in sort(unique(indicValues$yearName))) {

      yyy <- NULL
      for (j in sort(unique(indicAreas$id))) {
        yyy <- rbind(yyy,indicAreas[indicAreas$id==j,][1,c("id","name","indId")])
      }
      names(yyy) <- c("ICunitId","ICunitName","indId")
      indicValuesi <- indicValues[indicValues$yearName == i,]
      yyy <- merge(yyy,indicValuesi,all.x = T)
      zzz1 <- NULL
      zzz2 <- NULL
      zzz3 <- NULL
      for (j in 1:dim(yyy)[[1]]) {
        if (!is.na(yyy$customDistributionUUID[j])) {
          zzz1 <- c(zzz1,indicCustom[yyy$customDistributionUUID[j]])
        } else {
          zzz1 <- c(zzz1,NA)
        }
        zzz2 <- c(zzz2,indics$scalingModelId[indics$id == yyy$indId[j]])
        zzz3 <- c(zzz3,indics$scalingModel[indics$id == yyy$indId[j]])
      }
      yyy$customDistribution <- zzz1
      yyy$scalingModelId <- zzz2
      yyy$scalingModel <- zzz3

      indicatorValuesNy <- c(indicatorValuesNy,list(yyy))
      names(indicatorValuesNy)[length(indicatorValuesNy)] <- i
    }

    # 5) Reference values

    yyy <- NULL
    for (j in sort(unique(indicAreas$id))) {
      yyy <- rbind(yyy,indicAreas[indicAreas$id==j,][1,c("id","name","indId")])
    }
    names(yyy) <- c("ICunitId","ICunitName","indId")
    yyy <- merge(yyy,refValues,all.x = T)
    zzz1 <- NULL
    for (j in 1:dim(yyy)[[1]]) {
      if (!is.na(yyy$customDistributionUUID[j])) {
        zzz1 <- c(zzz1,refCustom[yyy$customDistributionUUID[j]])
      } else {
        zzz1 <- c(zzz1,NA)
      }
    }
    yyy$customDistribution <- zzz1

    referenceValuesNy <- yyy

    # 6) NI areas
    #    predefined NIareas

    wholeArea <- parts <- counties <- NULL

    if (predefNIunits["allArea"]) {
      wholeArea <- rep(1,length(basicAreas$name))
      names(wholeArea) <- basicAreas$name
    }
    if (predefNIunits["counties"] & length(basicAreas$county) > 0) {
      counties <- matrix(data=0,nrow=length(basicAreas$county),ncol = length(unique(basicAreas$county)),
                         dimnames = list(basicAreas$name,unique(basicAreas$county)))
      for (i in 1:length(basicAreas$name)) {
        counties[basicAreas$name[i],basicAreas$county[i]] <- 1
      }
    }
    if (predefNIunits["parts"] & length(basicAreas$part) > 0) {
      parts <- matrix(data=0,nrow=length(basicAreas$part),ncol = length(unique(basicAreas$part)),
                      dimnames = list(basicAreas$name,unique(basicAreas$part)))
      for (i in 1:length(basicAreas$name)) {
        parts[basicAreas$name[i],basicAreas$part[i]] <- 1
      }
    }

    # user supplied NIareas

    others <- NULL
    if (addNIareas) {
      NIareaNames <- unique(NIareas$NIunitName[!is.na(NIareas$NIunitName)])
      others <- matrix(data=0,nrow=length(basicAreas$name),ncol = length(NIareaNames),
                       dimnames = list(basicAreas$name,NIareaNames))
      for (i in NIareaNames) {
        xxx <- NIareas$BSunitId[NIareas$NIunitName==i]
        yyy <- which(basicAreas$id %in% xxx)
        others[yyy,i] <- 1
      }
    }

    NIunits <- cbind(wholeArea,counties,parts,others)

    if (nOutputObjects > 1) {
      out <- c(out,list(niInput(indicators=indics,
                                ICunits = ICunitsData,
                                BSunits = basicAreas,
                                referenceValues = referenceValuesNy,
                                indicatorValues = indicatorValuesNy,
                                NIunits = NIunits)))
      names(out)[length(out)] <- econame
    } else {
      out <- niInput(indicators=indics,
                     ICunits = ICunitsData,
                     BSunits = basicAreas,
                     referenceValues = referenceValuesNy,
                     indicatorValues = indicatorValuesNy,
                     NIunits = NIunits)
    }
  }

  return(out)

} # END OF FUNCTION

