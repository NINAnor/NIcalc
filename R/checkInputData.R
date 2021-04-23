#' Check Data Set
#'
#' Checks the content and consistency of a candidate data set for calculating
#' the Nature Index.
#'
#' \code{checkInputData} runs through an extensive series of tests checking
#' whether the entered dataset contains all the necessary data objects and
#' variables for calculating the Nature Index, and whether the data contain
#' consistent information.
#'
#' The function checks
#' \itemize{
#' \item that all necessary types of data objects are included:
#'       \itemize{\item indicators,
#'                \item reference values, \item indicator observations,
#'                \item ICunits, \item BSunits, \item ecosystems (not required),
#'                \item NIunits (not required)}
#' \item that all required variables characterizing each object type are included.
#' \item that IDs and/or names are unique for all types of objects.
#' \item consistency among input objects of different types.
#' \item for missing values.
#' }
#'
#' While running through the tests, \code{checkInputData} generates error
#' messages or warnings when identifying missing or inconsistent information.
#' The function may make some simple modifications to the data set to mend
#' shortcomings. It issues warning messages explaining such modifications.
#'
#' The data set to be checked may be entered either as a list assigned to the
#' argument \code{inputData} and containing all types of objects, or as several
#' objects, one for each type, assigned to the other function arguments.
#'
#' @seealso \code{\link{assembleNiObject}} and \code{\link{niInput}} for more
#' about the requirements to input data,
#' \code{\link{importDatasetApi}}, \code{\link{getNIdata}}, and
#' \code{\link{niDataImport}} for importing data from the NI database.
#'
#' @name checkInputData
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param inputData	list with elements named
#'   \tabular{ll}{\code{[1]} \tab \code{"indicators"},\cr
#'                \code{[2]} \tab \code{"referenceValues"},\cr
#'                \code{[3]} \tab \code{"indicatorObservations"},\cr
#'                \code{[4]} \tab \code{"ICunits"},\cr
#'                \code{[5]} \tab \code{"BSunits"},\cr
#'                \code{[6]} \tab \code{"ecosystems"},\cr
#'                \code{[7]} \tab \code{"NIunits"},}
#'   where the second and third elements are lists, while the others are data.frames.
#'   The last two element, \code{"ecosystems"} and \code{"NIunits"}, may be
#'   omitted or set to \code{NULL}.
#' @param indicators	data.frame containing indicator characteristics.
#' @param referenceValues	list containing reference value characteristics.
#' @param indicatorObservations	list containing indicator observations characteristics.
#' @param ICunits	data.frame containing ICunits characteristics.
#' @param BSunits	data.frame containing BSunits characteristics.
#' @param ecosystems	data.frame containing ecosystems characteristics.
#' @param NIunits	data.frame containing NIunits characteristics.
#'
#' @return either a list with the same elements that was
#'   assigned to function arguments, \cr\cr or \cr\cr a set of error messages.
#'
#' @examples
#' \dontrun{
#' carnivoreImport <- importDatasetApi(
#'      username = "...",
#'      password = "...",
#'      eco = NULL,
#'      indic = c("Jerv","Gaupe","Ulv"),
#'      year = c("1990","2000","2010","2014","2019"))
#' checkInputData(inputData = carnivoreImport)
#'
#' checkInputData(BSunits = 1)
#' }
#'
#'@export

checkInputData <- function(inputData = NULL,
                           indicators = NULL,
                           referenceValues = NULL,
                           indicatorObservations = NULL,
                           ICunits = NULL,
                           BSunits = NULL,
                           ecosystems = NULL,
                           NIunits = NULL) {

  ##########################################################################################
  # STEP 1. Check that required dataframes are included
  ##########################################################################################

  nErrors <- 0
  nWarnings <- 0
  errorMessages <- NULL
  warningNotifications <- NULL
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

  if (length(inputData) > 0) {
    framesNotFound <- which(match(c("indicators", "referenceValues",
                                    "indicatorObservations",
                                    "ICunits","BSunits"), table = names(inputData),
                                  nomatch = 0) == 0)
    if (length(framesNotFound) > 0) {
      nErrors <- nErrors + 1
      errorMessages[nErrors] <- paste("Required argument(s)",
                                      paste(c("'indicators'", "'referenceValues'",
                                              "'indicatorObservations'",
                                              "'ICunits'","'BSunits'",
                                              "'ecosystems'")[framesNotFound],
                                            collapse = ", "),
                                      "missing, with no default.")
    }
    framesNotFound <- which(match(c("indicators", "referenceValues",
                                    "indicatorObservations",
                                    "ICunits","BSunits","NIunits"),
                                  table = names(inputData), nomatch = 0) == 0)

    inputData <- inputData[match(c("indicators", "referenceValues",
                                   "indicatorObservations",
                                   "ICunits","BSunits","ecosystems","NIunits"),
                                 table = names(inputData), nomatch = 0)]
  } else {
    framesNotFound <- which(c(length(indicators) == 0,length(referenceValues) == 0,
                              length(indicatorObservations) == 0,length(ICunits) == 0,
                              length(BSunits) == 0))
    if (length(framesNotFound) > 0) {
      nErrors <- nErrors + 1
      errorMessages[nErrors] <- paste("Required argument(s)",
                                      paste(c("'indicators'", "'referenceValues'",
                                              "'indicatorObservations'","'ICunits'",
                                              "'BSunits'","'ecosystems'")[framesNotFound],
                                            collapse = ", "),"missing, with no default.")
    }

    framesNotFound <- which(c(length(indicators) == 0,length(referenceValues) == 0,
                              length(indicatorObservations) == 0,length(ICunits) == 0,
                              length(BSunits) == 0,length(NIunits) == 0))

    inputData <- list(indicators = indicators, referenceValues = referenceValues,
                      indicatorObservations = indicatorObservations, ICunits = ICunits,
                      BSunits = BSunits, NIunits = NIunits, ecosystems = ecosystems)

  }

  ##########################################################################################
  # STEP 2. Check that required variables are included, and that IDs and names are unique
  # STEP 3. Check for missing values
  ##########################################################################################

  ##########################################################################################
  # 2.1. Check required variables in 'ecosystems'.
  # 3.1. Check for missing values in 'ecosystems'.

  if (length(inputData$ecosystems) > 0) {
    matchNames <- c("id", "name")
    dataNotFound <- which(match(matchNames, table = names(inputData$ecosystems),
                                nomatch = 0) == 0)
    dataFound <- which(match(matchNames, table = names(inputData$ecosystems),
                             nomatch = 0) != 0)

    # 2.1.1 Check that required variables, which uniquely identify ecosystems,
    # are included in 'ecosystems'

    if (1 %in% dataNotFound & 2 %in% dataNotFound) {
      if ((length(dimnames(inputData$ecosystems)[[1]]) > 0) &
          (length(dimnames(inputData$ecosystems)[[1]]) ==
           length(unique(dimnames(inputData$ecosystems)[[1]])))) {
        inputData$ecosystems$name <- dimnames(inputData$ecosystems)[[1]]
        dataFound <- c(dataFound,2)
      } else {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Required column, 'id' and/or 'name', missing in data frame 'ecosystems', with no default.")
      }
    }

    if (2 %in% dataFound) {
      uniqueTestFails <- length(inputData$ecosystems$name) !=
        length(unique(inputData$ecosystems$name))
      if (uniqueTestFails) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Not all names in 'inputData$ecosystems$name' are unique.")
      } else {
        inputData$ecosystems$name <- as.character(inputData$ecosystems$name)
      }
      xxx <- which(is.na(inputData$ecosystems$name) | is.nan(inputData$ecosystems$name))
      if (length(xxx) > 0) {
        nErrors <- nErrors + 1
        if (1 %in% dataFound) {
          errorMessages[nErrors] <- paste("Names missing for ecosystem(s) '",
                                          paste(inputData$ecosystems$id[xxx],
                                                collapse = "', '"),
                                          "'.",sep="")
        } else {
          errorMessages[nErrors] <- paste("Names missing for ",length(xxx),
                                          " ecosystem(s).",sep="")
        }
      }
    }

    if (1 %in% dataFound) {
      uniqueTestFails <- length(inputData$ecosystems$id) !=
        length(unique(inputData$ecosystems$id))
      if (uniqueTestFails) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Not all IDs in 'inputData$ecosystems$id' are unique.")
      } else if (!(2 %in% dataFound)) {
        inputData$ecosystems$name <- as.character(inputData$ecosystems$id)
      }
      xxx <- which(is.na(inputData$ecosystems$id) | is.nan(inputData$ecosystems$id))
      if (length(xxx) > 0) {
        nErrors <- nErrors + 1
        if (2 %in% dataFound) {
          errorMessages[nErrors] <- paste("Ids missing for ecosystem(s) '",
                                          paste(inputData$ecosystems$name[xxx],
                                                collapse = "', '"),
                                          "'.",sep="")
        } else {
          errorMessages[nErrors] <- paste("Ids missing for ",length(xxx),
                                          " ecosystem(s).",sep="")
        }
      }
    }
  }

  ##########################################################################################
  # 2.2. Check required variables in 'indicators'.
  # 3.2 Check for missing values in 'indicators'.

  if (!(1 %in% framesNotFound)) {
    if (length(inputData$ecosystems) > 0) {
      matchNames <- c("id", "name","keyElement","functionalGroup",
                      "functionalGroupId","scalingModel","scalingModelId")
    } else {
      matchNames <- c("id", "name","keyElement","functionalGroup",
                      "functionalGroupId","scalingModel","scalingModelId",
                      "fidelity")
    }
    dataNotFound <- which(match(matchNames, table = names(inputData$indicators),
                                nomatch = 0) == 0)
    dataFound <- which(match(matchNames, table = names(inputData$indicators),
                             nomatch = 0) != 0)

    # 2.2.1 Check that required variables, which uniquely identify indicators,
    # are included in 'indicators'

    if (1 %in% dataNotFound & 2 %in% dataNotFound) {
      if ((length(dimnames(inputData$indicators)[[1]]) > 0) &
          (length(dimnames(inputData$indicators)[[1]]) ==
           length(unique(dimnames(inputData$indicators)[[1]])))) {
        inputData$indicators$name <- dimnames(inputData$indicators)[[1]]
        dataFound <- c(dataFound,2)
      } else {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Required column, 'id' and/or 'name', missing in data frame 'indicators', with no default.")
      }
    }

    if (2 %in% dataFound) {
      uniqueTestFails <- length(inputData$indicators$name) !=
        length(unique(inputData$indicators$name))
      if (uniqueTestFails) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Not all names in 'inputData$indicators$name'are unique.")
      } else {
        inputData$indicators$name <- as.character(inputData$indicators$name)
      }
      xxx <- which(is.na(inputData$indicators$name) | is.nan(inputData$indicators$name))
      if (length(xxx) > 0) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <- paste("Names and IDs missing for indicator(s) ",
                                        paste(xxx, collapse = ", "),".",sep="")
      }
    }

    if (1 %in% dataFound) {
      uniqueTestFails <- length(inputData$indicators$id) !=
        length(unique(inputData$indicators$id))
      if (uniqueTestFails) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Not all IDs in 'inputData$indicators$id' are unique.")
      } else if (!(2 %in% dataFound)) {
        inputData$indicators$name <- as.character(inputData$indicators$id)
      }
    }

    # 2.2.2 Check that other required variables are included in 'indicators'

    # Key indicators

    if (3 %in% dataNotFound) {
      nWarnings <- nWarnings + 1
      warningNotifications[nWarnings] <-
        paste("Column 'keyElement' missing in data frame 'indicators'.",
              "Set to FALSE in all rows (i.e. no key elements).")
      inputData$indicators$keyElement <- rep(FALSE,dim(inputData$indicators)[1])
    }

    if (3 %in% dataFound) {
      xxx <- which(is.na(inputData$indicators$keyElement) |
                     is.nan(inputData$indicators$keyElement))
      if (length(xxx) > 0) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Key element status not provided for indicator(s) ",
                paste(inputData$indicators$name[xxx], collapse = "', '"),".",sep="")
      }
    }

    # Functional groups

    if (4 %in% dataNotFound) {
      if (5 %in% dataNotFound) {
        nWarnings <- nWarnings + 1
        warningNotifications[nWarnings] <-
          paste("Column 'functionalGroup' missing in data frame 'indicators'.",
                "\nSet to 'nogroup' in all rows (i.e. one single functional group",
                "containing all indicators).")
        inputData$indicators$functionalGroup <-
          rep("nogroup",dim(inputData$indicators)[1])
      } else {
        inputData$indicators$functionalGroup <-
          as.character(inputData$indicators$functionalGroupId)
      }
    } else {
      inputData$indicators$functionalGroup <-
        as.character(inputData$indicators$functionalGroup)
    }

    if (4 %in% dataFound) {
      xxx <- which((is.na(inputData$indicators$functionalGroup) |
                      is.nan(inputData$indicators$functionalGroup)) &
                     (!inputData$indicators$keyElement))
      if (length(xxx) > 0) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Functional group not provided for nonkey indicator(s) ",
                paste(inputData$indicators$name[xxx], collapse = "', '"),".",sep="")
      }
    }

    # Scaling model

    if (6 %in% dataNotFound) {
      if (7 %in% dataNotFound) {
        nWarnings <- nWarnings + 1
        warningNotifications[nWarnings] <-
          paste("Column 'scalingModel' missing in data frame 'indicators'. Set to 'Low' in all rows.")
        inputData$indicators$scalingModel <- rep("Low",dim(inputData$indicators)[1])
      } else {
        inputData$indicators$scalingModel <- as.character(inputData$indicators$scalingModelId)
        inputData$indicators$scalingModel[inputData$indicators$scalingModelId == 1] <- "Low"
        inputData$indicators$scalingModel[inputData$indicators$scalingModelId == 2] <- "Max"
      }
    } else {
      inputData$indicators$scalingModel <- as.character(inputData$indicators$scalingModel)
      xxx <- which(is.na(inputData$indicators$scalingModel) |
                     is.nan(inputData$indicators$scalingModel))
      if (length(xxx) > 0) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Scaling model not provided for indicator(s) ",
                paste(inputData$indicators$name[xxx], collapse = "', '"),".",sep="")
      }
    }

    # Fidelity - no named ecosystems

    if (8 %in% dataNotFound & length(inputData$ecosystems) == 0) {
      nWarnings <- nWarnings + 1
      warningNotifications[nWarnings] <-
        paste("Column 'fidelity' missing in data frame 'indicators'.",
              "\nNo weighting with respect to fidelities in index calculations using this data set.")
      inputData$indicators$fidelity <- rep(100.0,dim(inputData$indicators)[1])
    }
    if (8 %in% dataFound & length(inputData$ecosystems) == 0) {
      xxx <- which(is.na(inputData$indicators$fidelity) |
                     is.nan(inputData$indicators$fidelity))
      if (length(xxx) > 0) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Fidelity not provided for indicator(s) ",
                paste(inputData$indicators$name[xxx], collapse = "', '"),".",sep="")
      }
    }

    # Fidelity - named ecosystems

    if (length(inputData$ecosystems) > 0) {
      dataNotFound <- which(match(inputData$ecosystems$name,
                                  table = names(inputData$indicators),
                                  nomatch = 0) == 0)
      if (length(dataNotFound) == length(inputData$ecosystems$name)) {
        nWarnings <- nWarnings + 1
        warningNotifications[nWarnings] <-
          paste("No columns for fidelities to ", paste(inputData$ecosystems$name, collapse = ", "),
                "in data frame 'indicators'.\nAll indicators assumed to belong to the same indicator set.",
                "\nNo weighting with respect to fidelities in index calculations based on this data set.")
        inputData$ecosystems <- NULL
        inputData$indicators$fidelity <- rep(100.0,dim(inputData$indicators)[1])
      } else if (length(dataNotFound) > 0) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Required indicator fidelities with respect to ecosystem(s) '",
                paste(inputData$ecosystems$name[dataNotFound], collapse = "', '"),
                "' missing in data frame 'indicators'.",sep="")
      } else {
        for (i in inputData$ecosystems$name) {
          j <- which(names(inputData$indicators) == i)
          xxx <- which(is.na(inputData$indicators[,j]) |
                         is.nan(inputData$indicators[,j]))
          if (length(xxx) > 0) {
            nErrors <- nErrors + 1
            errorMessages[nErrors] <-
              paste("Fidelity to ",i, " not provided for indicator(s) ",
                    paste(inputData$indicators$name[xxx], collapse = "', '"),".",sep="")
          }
        }
      }
    }
  }

  ##########################################################################################
  # 2.3. Check required variables in 'referenceValues'.
  # 3.3. Check for missing values in 'referenceValues'.

  if (!(2 %in% framesNotFound)) {

    matchNames <- c("referenceValues","customDistributions")
    dataNotFound <- which(match(matchNames, table = names(inputData$referenceValues),
                                nomatch = 0) == 0)

    # 2.3.1 Check if object with reference values is provided.

    if (1 %in% dataNotFound) {
      nErrors <- nErrors + 1
      errorMessages[nErrors] <-
        paste("Required dataframe 'inputData$referenceValues$referenceValues' missing, with no default.")
    } else {
      matchNames <- c("indId","indName","ICunitId","ICunitName","yearId","yearName","expectedValue","lowerQuantile",
                      "upperQuantile","customDistributionUUID","distributionFamilyId","distributionFamilyName",
                      "distParameter1","distParameter2")
      dataFound <- which(match(matchNames,
                               table = names(inputData$referenceValues$referenceValues), nomatch = 0) != 0)

      # 2.3.2 Check if necessary objects to identify and specify possible custom distribuitons are provided.

      if ((2 %in% dataNotFound) & (10 %in% dataFound)) {
        if (sum(!(is.na(inputData$referenceValues$referenceValues$customDistributionUUID))) > 0) {
          nErrors <- nErrors + 1
          errorMessages[nErrors] <-
            paste("Required object 'inputData$referenceValues$customDistributions' missing, with no default.")
        }
      }

      if ((!(2 %in% dataNotFound)) & (!(10 %in% dataFound))) {
        if (length(inputData$referenceValues$customDistributions ) > 0) {
          nErrors <- nErrors + 1
          errorMessages[nErrors] <-
            paste("Required variable 'inputData$referenceValues$referenceValues$customDistributionUUID' missing, with no default.")
        }
      }

      dataNotFound <- which(match(matchNames,
                                  table = names(inputData$referenceValues$referenceValues), nomatch = 0) == 0)

      # 2.3.3 Check if necessary variables and data to specify reference values are provided.

      infoGiven <- rep(FALSE,dim(inputData$referenceValues$referenceValues)[1])
      if ((7 %in% dataFound) & (8 %in% dataFound) & (9 %in% dataFound)) {
        infoGiven <- infoGiven | (!(is.na(inputData$referenceValues$referenceValues$expectedValue) |
                                      is.na(inputData$referenceValues$referenceValues$lowerQuantile) |
                                      is.na(inputData$referenceValues$referenceValues$upperQuantile)))
      }
      if (10 %in% dataFound) {
        infoGiven <- infoGiven | (!(is.na(inputData$referenceValues$referenceValues$customDistributionUUID)))
      }
      if ((12 %in% dataFound) & (13 %in% dataFound) & (14 %in% dataFound)) {
        infoGiven <- infoGiven |
          (!(is.na(inputData$referenceValues$referenceValues$distributionFamilyName) |
               is.na(inputData$referenceValues$referenceValues$distParameter1) |
               (is.na(inputData$referenceValues$referenceValues$distParameter2) &
                  (inputData$referenceValues$referenceValues$distributionFamilyName != "Poisson"))))

      }
      xxx <- sum(!infoGiven)
      if (xxx == dim(inputData$referenceValues$referenceValues)[1]) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("No reference values are provided in the dataset.")
      } else if (xxx > 50) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("More than 50 reference values are missing.")
      } else if (xxx > 0) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Reference values in rows ", paste(which(!infoGiven),collapse = ", "),
                " of 'inputData$referenceValues$referenceValues' are missing.",sep="")
      }

      # 2.3.4 Check if the reference values´ ICunits are provided.

      if (3 %in% dataNotFound) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Required variable 'inputData$referenceValues$referenceValues$ICunitId' missing, with no default.")
      }

      if (3 %in% dataFound) {
        xxx <- which(is.na(inputData$referenceValues$referenceValues$ICunitId) |
                       is.nan(inputData$referenceValues$referenceValues$ICunitId))
        if (length(xxx) > 0) {
          nErrors <- nErrors + 1
          errorMessages[nErrors] <- paste("Area ID not provided in row(s) '",
                                          paste(xxx, collapse = "', '"),
                                          "' of 'inputData$referenceValues$referenceValues'.",sep="")
        }
      }
    }
  }

  ##########################################################################################
  # 2.4. Check required variables in 'indicatorObservations'.
  # 3.4. Check for missing values in 'indicatorObservations'.

  if (!(3 %in% framesNotFound)) {

    matchNames <- c("indicatorValues","customDistributions")
    dataNotFound <- which(match(matchNames, table = names(inputData$indicatorObservations), nomatch = 0) == 0)

    # 2.4.1 Check if object with indicator values is provided.

    if (1 %in% dataNotFound) {
      nErrors <- nErrors + 1
      errorMessages[nErrors] <-
        paste("Required dataframe 'inputData$indicatorObservations$indicatorValues' missing, with no default.")
    } else {
      matchNames <- c("indId","indName","ICunitId","ICunitName","yearId","yearName","expectedValue","lowerQuantile",
                      "upperQuantile","customDistributionUUID","distributionFamilyId","distributionFamilyName",
                      "distParameter1","distParameter2")
      dataFound <- which(match(matchNames,
                               table = names(inputData$indicatorObservations$indicatorValues), nomatch = 0) != 0)

      # 2.4.2 Check if necessary objects to identify and specify possible custom distributons are provided.

      if ((2 %in% dataNotFound) & (10 %in% dataFound)) {
        if (sum(!(is.na(inputData$indicatorObservations$indicatorValues$customDistributionUUID))) > 0) {
          nErrors <- nErrors + 1
          errorMessages[nErrors] <-
            paste("Required object 'inputData$indicatorObservations$customDistributions' missing, with no default.")
        }
      }

      if ((!(2 %in% dataNotFound)) & (!(10 %in% dataFound))) {
        if (length(inputData$indicatorObservations$customDistributions ) > 0) {
          nErrors <- nErrors + 1
          errorMessages[nErrors] <-
            paste("Required variable 'inputData$indicatorObservations$indicatorValues$customDistributionUUID' missing, with no default.")
        }
      }

      dataNotFound <- which(match(matchNames,
                                  table = names(inputData$indicatorObservations$indicatorValues), nomatch = 0) == 0)

      # 2.4.3 Check if the indicator values´ ICunits are provided.

      if (3 %in% dataNotFound) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Required variable 'inputData$indicatorObservations$indicatorValues$ICunitId' missing, with no default.")
      }

      if (3 %in% dataFound) {
        xxx <- which(is.na(inputData$indicatorObservations$indicatorValues$ICunitId) |
                       is.nan(inputData$indicatorObservations$indicatorValues$ICunitId))
        if (length(xxx) > 0) {
          nErrors <- nErrors + 1
          errorMessages[nErrors] <- paste("Area ID not provided in row(s) '",
                                          paste(xxx, collapse = "', '"),
                                          "' of 'inputData$indicatorObservations$indicatorValues'.",sep="")
        }
      }

      # 3.4.4 Check if there are missing info about years

      if (5 %in% dataFound) {
        xxx <- which(is.na(inputData$indicatorObservations$indicatorValues$yearId) |
                       is.nan(inputData$indicatorObservations$indicatorValues$yearId))
        if (length(xxx) > 0) {
          nErrors <- nErrors + 1
          errorMessages[nErrors] <- paste("Year ID not provided in row(s) '",
                                          paste(xxx, collapse = "', '"),
                                          "' of 'inputData$indicatorObservations$indicatorValues'.",sep="")
        }
      }

      if (6 %in% dataFound) {
        xxx <- which(is.na(inputData$indicatorObservations$indicatorValues$yearName) |
                       is.nan(inputData$indicatorObservations$indicatorValues$yearName))
        if (length(xxx) > 0) {
          nErrors <- nErrors + 1
          errorMessages[nErrors] <- paste("Year not provided in row(s) '",
                                          paste(xxx, collapse = "', '"),
                                          "' of 'inputData$indicatorObservations$indicatorValues'.",sep="")
        }
      }

      # 2.4.4 Check how many years there are indicator observations

      if ((5 %in% dataNotFound) & (6 %in% dataNotFound)) {
        nYears <- 1
      } else if (6 %in% dataNotFound) {
        inputData$indicatorObservations$indicatorValues$yearName <-
          as.character(inputData$indicatorObservations$indicatorValues$yearId)
        xxx <- (!is.na(inputData$indicatorObservations$indicatorValues$yearName)) &
          (!is.nan(inputData$indicatorObservations$indicatorValues$yearName))
        nYears <- length(unique(inputData$indicatorObservations$indicatorValues$yearName[xxx]))
      } else {
        xxx <- (!is.na(inputData$indicatorObservations$indicatorValues$yearName)) &
          (!is.nan(inputData$indicatorObservations$indicatorValues$yearName))
        nYears <- length(unique(inputData$indicatorObservations$indicatorValues$yearName[xxx]))
      }

      # 2.4.5 Check if necessary variables and data to specify indicator values are provided.

      infoGiven <- rep(FALSE,dim(inputData$indicatorObservations$indicatorValues)[1])
      if ((7 %in% dataFound) & (8 %in% dataFound) & (9 %in% dataFound)) {
        infoGiven <- infoGiven | (!(is.na(inputData$indicatorObservations$indicatorValues$expectedValue) |
                                      is.na(inputData$indicatorObservations$indicatorValues$lowerQuantile) |
                                      is.na(inputData$indicatorObservations$indicatorValues$upperQuantile)))
      }
      if (10 %in% dataFound) {
        infoGiven <- infoGiven | (!(is.na(inputData$indicatorObservations$indicatorValues$customDistributionUUID)))
      }
      if ((12 %in% dataFound) & (13 %in% dataFound) & (14 %in% dataFound)) {
        infoGiven <- infoGiven |
          (!(is.na(inputData$indicatorObservations$indicatorValues$distributionFamilyName) |
               is.na(inputData$indicatorObservations$indicatorValues$distParameter1) |
               (is.na(inputData$indicatorObservations$indicatorValues$distParameter2) &
                  (inputData$indicatorObservations$indicatorValues$distributionFamilyName != "Poisson"))))

      }

      xxx <- sum(!infoGiven)
      if (xxx > 0) {
        noObs <- which(table(inputData$indicatorObservations$indicatorValues$ICunitId,infoGiven)[,"FALSE"] ==
                         nYears)
      } else {
        noObs <- NULL
      }

      if (nYears > 1) {
        someObs <- which(table(inputData$indicatorObservations$indicatorValues$ICunitId,infoGiven)[,"TRUE"] <
                           nYears)
      } else {
        someObs <- NULL
      }


      if (xxx == dim(inputData$indicatorObservations$indicatorValues)[1]) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("No indicator values are provided in the dataset.")
      } else if (length(noObs) > 50) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Indicator values for more than 50 areas are missing.")
      } else if (length(noObs) > 0) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Indicator values for areas ", paste(noObs,collapse = ", "),
                " are missing in 'inputData$indicatorObservations$indicatorValues'.",sep="")
      } else if (length(someObs) > 50) {
        nWarnings <- nWarnings + 1
        warningNotifications[nWarnings] <-
          paste("The dataset contains ",length(someObs),
                " time series with at least one missing indicator observation.",sep="")
      } else if (length(someObs) > 0) {
        nWarnings <- nWarnings + 1
        warningNotifications[nWarnings] <-
          paste("Time series from areas ", paste(someObs,collapse = ", "),
                " contain at least one missing indicator observation each.",sep="")
      }
    }
  }

  ##########################################################################################
  # 2.5. Check required variables in 'ICunits'.
  # 3.5. Check for missing values in 'ICunits'.

  if (!(4 %in% framesNotFound)) {
    matchNames <- c("id", "name","BSunitId","indId")

    dataNotFound <- which(match(matchNames, table = names(inputData$ICunits), nomatch = 0) == 0)
    dataFound <- which(match(matchNames, table = names(inputData$ICunits), nomatch = 0) != 0)

    # 2.5.1 Check that required variable, which uniquely identify ICunits, are included in 'ICunits'


    if (1 %in% dataFound) {
      xxx <- which(is.na(inputData$ICunits$id) |
                     is.nan(inputData$ICunits$id))
      if (length(xxx) > 0) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <- paste("Area ID not provided in row(s) '",
                                        paste(xxx, collapse = "', '"),
                                        "' of 'inputData$ICunits$id'.",sep="")
      }
    }

    if (1 %in% dataNotFound) {
      nErrors <- nErrors + 1
      errorMessages[nErrors] <-
        paste("Required column, 'id', missing in data frame 'ICunits', with no default.")
    }

    if (1 %in% dataFound & 2 %in% dataNotFound) {
      inputData$ICunits$name <- as.character(inputData$ICunits$id)
    }

    # 2.5.2 Check that required variable, which delimits ICunits, are included in 'ICunits'

    if (3 %in% dataNotFound) {
      nErrors <- nErrors + 1
      errorMessages[nErrors] <-
        paste("Required column, 'BSunitId', missing in data frame 'ICunits', with no default.")
    }

    if (3 %in% dataFound) {
      xxx <- which(is.na(inputData$ICunits$BSunitId) |
                     is.nan(inputData$ICunits$BSunitId))
      if (length(xxx) > 0) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <- paste("BSunit ID not provided in row(s) '",
                                        paste(xxx, collapse = "', '"),
                                        "' of 'inputData$ICunits$BSunitId'.",sep="")
      }
    }

    # 2.5.2 Check that required variable, which links ICunits to indicators, are included in 'ICunits'

    if (4 %in% dataNotFound) {
      nErrors <- nErrors + 1
      errorMessages[nErrors] <-
        paste("Required column, 'indId', missing in data frame 'ICunits', with no default.")
    }

    if (4 %in% dataFound) {
      xxx <- which(is.na(inputData$ICunits$indId) |
                     is.nan(inputData$ICunits$indId))
      if (length(xxx) > 0) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <- paste("Indicator ID not provided in row(s) '",
                                        paste(xxx, collapse = "', '"),
                                        "' of 'inputData$ICunits$indId'.",sep="")
      }
    }
  }

  ##########################################################################################
  # 2.6. Check required variables in 'BSunits'.
  # 3.6. Check for missing values in 'BSunits'.

  if (!(5 %in% framesNotFound)) {
    matchNames <- c("id", "name")
    dataNotFound <- which(match(matchNames, table = names(inputData$BSunits), nomatch = 0) == 0)
    dataFound <- which(match(matchNames, table = names(inputData$BSunits), nomatch = 0) != 0)

    # 2.6.1 Check that required variable, which uniquely identify BSunits, are included in 'BSunits'

    if (1 %in% dataNotFound & 2 %in% dataNotFound) {
      if ((length(dimnames(inputData$BSunits)[[1]]) > 0) &
          (length(dimnames(inputData$BSunits)[[1]]) == length(unique(dimnames(inputData$BSunits)[[1]])))) {
        inputData$BSunits$name <- dimnames(inputData$BSunits)[[1]]
        dataFound <- c(dataFound,2)
      } else {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <-
          paste("Required column, 'id' and/or 'name', missing in data frame 'BSunits', with no default.")
      }
    }

    if (2 %in% dataFound) {
      uniqueTestFails <- length(inputData$BSunits$name) != length(unique(inputData$BSunits$name))
      if (uniqueTestFails) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <- paste("Not all names in 'inputData$BSunits$name' are unique.")
      } else {
        inputData$BSunits$name <- as.character(inputData$BSunits$name)
      }
      xxx <- which(is.na(inputData$BSunits$name) | is.nan(inputData$BSunits$name))
      if (length(xxx) > 0) {
        nErrors <- nErrors + 1
        if (1 %in% dataFound) {
          errorMessages[nErrors] <- paste("Name missing for BSunit(s) '",
                                          paste(inputData$BSunits$id[xxx], collapse = "', '"),
                                          "'.",sep="")
        } else {
          errorMessages[nErrors] <- paste("Name missing for BSunit(s) ",
                                          paste(xxx, collapse = ", "),".",sep="")
        }
      }
    }

    if (1 %in% dataFound) {
      uniqueTestFails <- length(inputData$BSunits$id) != length(unique(inputData$BSunits$id))
      if (uniqueTestFails) {
        nErrors <- nErrors + 1
        errorMessages[nErrors] <- paste("Not all IDs in 'inputData$BSunits$id' are unique.")
      } else if (!(2 %in% dataFound)) {
        inputData$BSunits$name <- as.character(inputData$BSunits$id)
      }
      xxx <- which(is.na(inputData$BSunits$id) | is.nan(inputData$BSunits$id))
      if (length(xxx) > 0) {
        nErrors <- nErrors + 1
        if (2 %in% dataFound) {
          errorMessages[nErrors] <- paste("ID missing for BSunit(s) '",
                                          paste(inputData$BSunits$name[xxx], collapse = "', '"),
                                          "'.",sep="")
        } else {
          errorMessages[nErrors] <- paste("ID missing for BSunit(s) ",
                                          paste(xxx, collapse = ", "),".",sep="")
        }
      }
    }

    # 2.6.2 Check that variables necessary for calculating area weights are included in 'BSunits'

    if (length(inputData$ecosystems$name) > 0) {
      matchNames <- c(inputData$ecosystems$name)
      dataNotFound2 <- which(match(matchNames, table = names(inputData$BSunits), nomatch = 0) == 0)
      matchNames <- names(inputData$BSunits)
      dataFound2 <- which(match(matchNames, table = inputData$ecosystems$name, nomatch = 0) != 0)

      if (length(dataNotFound2) > 0) {
        nWarnings <- nWarnings + 1
        warningNotifications[nWarnings] <-
          paste("The areal extent per BSunit of ecosystem(s) '",
                paste(inputData$ecosystems$name[dataNotFound2], collapse = "', '"),
                "' missing in data frame 'BSunits'.",sep="")
      }

      if (length(dataFound2) > 0) {
        xxx <- which(is.na(as.matrix(inputData$BSunits[,dataFound2])) |
                       is.nan(as.matrix(inputData$BSunits[,dataFound2])),arr.ind=T)
        if (length(xxx) > 0) {
          nErrors <- nErrors + 1
          if (2 %in% dataFound) {
            errorMessages[nErrors] <- paste("Areal extent of some ecosystem(s) missing for BSunit(s) '",
                                            paste(inputData$BSunits$name[xxx[,1]], collapse = "', '"),
                                            "' in data frame 'inputData$BSunits'.",sep="")
          } else if (1 %in% dataFound) {
            errorMessages[nErrors] <- paste("Areal extent of some ecosystem(s) missing for BSunit(s) '",
                                            paste(inputData$BSunits$id[xxx[,1]], collapse = "', '"),
                                            "' in data frame 'inputData$BSunits'.",sep="")
          } else {
            errorMessages[nErrors] <- paste("Areal extent of some ecosystem(s) missing in row(s) ",
                                            paste(xxx[,1], collapse = ", "),
                                            " in data frame 'inputData$BSunits'.",sep="")
          }
        }
      }
    }
  }

  ##########################################################################################
  # STEP 4. Check consistensy among input objects
  ##########################################################################################

  if (!(4 %in% framesNotFound)) {

    # 4.1. Check consistency between indicators and ICunits. All indicators should have
    #      areas with areadata, all ICunits should have indicatordata

    if (!(1 %in% framesNotFound)) {
      if (("id" %in%  names(inputData$indicators)) & ("indId" %in%  names(inputData$ICunits))) {
        xxx <- inputData$indicators$id
        yyy <- unique(inputData$ICunits$indId)
        if (length(yyy[which(!(yyy %in% xxx))]) > 0) {
          nErrors <- nErrors + 1
          errorMessages[nErrors] <-
            paste("Indicator(s) '",
                  paste(yyy[which(!(yyy %in% xxx))], collapse = "', '"),
                  "' not found in dataframe 'indicators'.",sep="")
        }
        if (length(xxx[which(!(xxx %in% yyy))]) > 0) {
          nWarnings <- nWarnings + 1
          if ("name" %in%  names(inputData$indicators)) {
            warningNotifications[nWarnings] <-
              paste("No ICunits were found for indicator(s) '",
                    paste(inputData$indicators$name[which(!(xxx %in% yyy))], collapse = "', '"),
                    "' in dataframe 'ICunits'.",sep="")
          } else {
            warningNotifications[nWarnings] <-
              paste("No ICunits were found for indicator(s) '",
                    paste(xxx[which(!(xxx %in% yyy))], collapse = "', '"),
                    "' in dataframe 'ICunits'.",sep="")
          }
        }
      }
    }

    # 4.2. Check consistency between reference values and ICunits. All reference values should have
    #      areas with areadata, all ICunits should have reference values

    if (!(2 %in% framesNotFound)) {
      if ("referenceValues" %in%  names(inputData$referenceValues)) {
        if (("ICunitId" %in%  names(inputData$referenceValues$referenceValues)) &
            ("id" %in%  names(inputData$ICunits))) {
          xxx <- unique(inputData$referenceValues$referenceValues$ICunitId)
          yyy <- unique(inputData$ICunits$id)
          if (length(yyy[which(!(yyy %in% xxx))]) > 0) {
            nErrors <- nErrors + 1
            errorMessages[nErrors] <-
              paste("Indicator area(s) '",
                    paste(yyy[which(!(yyy %in% xxx))], collapse = "', '"),
                    "' lack reference values.",sep="")
          }
          if (length(xxx[which(!(xxx %in% yyy))]) > 0) {
            nErrors <- nErrors + 1
            errorMessages[nErrors] <-
              paste("The following indicator area(s) included in 'referenceValues' '",
                    paste(xxx[which(!(xxx %in% yyy))], collapse = "', '"),
                    "' were not found in dataframe 'ICunits'.",sep="")
          }
        }
      }
    }

    # 4.3. Check consistency between indicator observations and ICunits. All observations should have
    #      areas with areadata, all ICunits should have observations for at least some years

    if (!(3 %in% framesNotFound)) {
      if ("indicatorValues" %in%  names(inputData$indicatorObservations)) {
        if (("ICunitId" %in%  names(inputData$indicatorObservations$indicatorValues)) &
            ("id" %in%  names(inputData$ICunits))) {
          xxx <- unique(inputData$indicatorObservations$indicatorValues$ICunitId)
          yyy <- unique(inputData$ICunits$id)
          if (length(yyy[which(!(yyy %in% xxx))]) > 0) {
            nErrors <- nErrors + 1
            errorMessages[nErrors] <-
              paste("Indicator(s) '",
                    paste(yyy[which(!(yyy %in% xxx))], collapse = "', '"),
                    "' lack reference values.",sep="")
          }
          if (length(xxx[which(!(xxx %in% yyy))]) > 0) {
            nErrors <- nErrors + 1
            errorMessages[nErrors] <-
              paste("The following indicator area(s) included in 'indicatorObservations' '",
                    paste(xxx[which(!(xxx %in% yyy))], collapse = "', '"),
                    "' were not found in dataframe 'ICunits'.",sep="")
          }
        }
      }
    }

    # 4.4. Check consistensy between ICunits and BSunits. All BSunits should be in at least
    #      one indicator area. All BSunits within a indicator area should have BSunit data.

    if (!(5 %in% framesNotFound)) {
      if (("id" %in%  names(inputData$BSunits)) & ("BSunitId" %in%  names(inputData$ICunits))) {
        xxx <- inputData$BSunits$id
        yyy <- unique(inputData$ICunits$BSunitId)
        if (length(yyy[which(!(yyy %in% xxx))]) > 0) {
          nErrors <- nErrors + 1
          errorMessages[nErrors] <-
            paste("BSunit(s) not found in dataframe 'BSunits': '",
                  paste(yyy[which(!(yyy %in% xxx))], collapse = "', '"),sep="")
        }
        if (length(xxx[which(!(xxx %in% yyy))]) > 0) {
          nWarnings <- nWarnings + 1
          if ("name" %in%  names(inputData$BSunits)) {
            warningNotifications[nWarnings] <-
              paste("No ICunits were found that included BSunit(s) '",
                    paste(inputData$BSunits$name[which(!(xxx %in% yyy))], collapse = "', '"),
                    "' in dataframe 'ICunits'.",sep="")
          } else {
            warningNotifications[nWarnings] <-
              paste("No ICunits were found that included BSunit(s) '",
                    paste(xxx[which(!(xxx %in% yyy))], collapse = "', '"),
                    "' in dataframe 'ICunits'.",sep="")
          }
        }
      }
    }
  }

  # 4.5. Check consistensy between NIunits and BSunits.

  if (!(6 %in% framesNotFound)) {
    if ("BSunitId" %in%  names(inputData$NIunits)) {
      if (!(5 %in% framesNotFound)) {
        if ("id" %in%  names(inputData$BSunits)) {
          yyy <- inputData$NIunits$BSunitId
          xxx <- unique(inputData$BSunits$id)
          if (length(yyy[which(!(yyy %in% xxx))]) > 0) {
            nErrors <- nErrors + 1
            errorMessages[nErrors] <-
              paste("BSunit(s) in dataframe 'NIunits' not found in dataframe 'BSunits': '",
                    paste(yyy[which(!(yyy %in% xxx))], collapse = "', '"),sep="")
          }
        }
      }
    } else {
      nErrors <- nErrors + 1
      errorMessages[nErrors] <-
        paste("Column 'BSunitId' not found in dataframe 'NIunits'")
    }
  }

  if (nWarnings > 0) {
    warning(paste("\nChecking the dataset generated ",nWarnings," warning(s):\n\n",
                  paste(warningNotifications,collapse="\n\n"),sep=""),file="")
  }

  if (nErrors > 0) {
    stop(paste("\n",nErrors," error(s) found in dataset:\n\n",paste(errorMessages,collapse="\n\n"),"\n",sep=""),file="")
  }

  return(inputData)

} # END OF FUNCTION
