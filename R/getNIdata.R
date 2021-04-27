#' Read a Table in the NI Database
#'
#' Functions that reads data from tables in the NI database via
#' [The Nature Index Application Programming Interface](https://www8.nina.no/NaturindeksNiCalc/index.html)
#' and imports the content as a data.frame.
#'
#' Each function reads one table in the database:
#' \tabular{llll}{
#' Function   \tab Table in NI database   \tab Content
#'      \tab Column names in returned dataframe\cr
#' \code{getBSunits} \tab \code{Kommune}
#'      \tab Characteristics for all BSunits.
#'      \tab \code{[,1]} \code{"id"},\cr
#' \tab \tab \tab \code{[,2]} \code{"name"},\cr
#' \tab \tab \tab \code{[,3]} \code{"area"},\cr
#' \tab \tab \tab \code{[,4]} \code{"terrestrial"},\cr
#' \tab \tab \tab \code{[,5]} \code{"marine"},\cr
#' \tab \tab \tab \code{[,6]} \code{"county"},\cr
#' \tab \tab \tab \code{[,7]} \code{"part"},\cr
#' \tab \tab \tab \code{[,8]} \code{"centerLat"},\cr
#' \tab \tab \tab \code{[,9]} \code{"centerLong"}\cr\cr
#' \code{getEcosystems} \tab \code{T_Okosystem}
#'      \tab Ecosystem names and ids.\tab \code{[,1]} \code{"id"},\cr
#' \tab \tab \tab \code{[,2]} \code{"name"},\cr
#' \tab \tab \tab \code{[,3]} \code{"nameEnglish"}.\cr\cr
#' \code{getEcosystemBSunitData} \tab \code{Kommune_Okosystem}
#'      \tab Area of major ecosystems
#'      \tab \code{[,1]} \code{"ecosystemId"},\cr
#' \tab \tab per BSunit \tab \code{[,2]} \code{"municipalityId"},\cr
#' \tab \tab \tab \code{[,3]} \code{"area"}.\cr\cr
#' \code{getFunctionalGroupData} \tab \code{T_FunksjonellGruppe}
#'      \tab names of functional groups
#'      \tab \code{[,1]} \code{"id"},\cr
#' \tab \tab \tab \code{[,2]} \code{"name"},\cr
#' \tab \tab \tab \code{[,3]} \code{"nameEnglish"}.\cr\cr
#' \code{getICunits} \tab \code{Omraade} \tab ICunit for each indicator
#'      \tab \code{[,1]} \code{"id"},\cr
#' \tab \tab \tab \code{[,2]} \code{"name"},\cr
#' \tab \tab \tab \code{[,3]} \code{"indicatorId"}.\cr\cr
#' \code{getICunitBSunit} \tab \code{Omraade_Kommune} \tab BSunits within each ICunit.
#'      \tab \code{[,1]} \code{"areaId"},\cr
#' \tab \tab \tab \code{[,2]} \code{"municipalityId"}.\cr\cr
#' \code{getIndicators2} \tab \code{Indikator} \tab Characteristics for all indicators
#'      \tab \code{[, 1]} \code{"id"},\cr
#' \tab \tab \tab \code{[, 2]} \code{"active"},\cr
#' \tab \tab \tab \code{[, 3]} \code{"name"},\cr
#' \tab \tab \tab \code{[, 4]} \code{"nameEnglish"},\cr
#' \tab \tab \tab \code{[, 5]} \code{"nameLatin"},\cr
#' \tab \tab \tab \code{[, 6]} \code{"indicatorTypeId"},\cr
#' \tab \tab \tab \code{[, 7]} \code{"taxaDetailId"},\cr
#' \tab \tab \tab \code{[, 8]} \code{"functionalGroupId"},\cr
#' \tab \tab \tab \code{[, 9]} \code{"scalingModel"},\cr
#' \tab \tab \tab \code{[,10]} \code{"taxaKingdomId"},\cr
#' \tab \tab \tab \code{[,11]} \code{"keyElement"},\cr
#' \tab \tab \tab \code{[,12]} \code{"referenceStateId"}.\cr\cr
#' \code{getIndicatorEcosystemData} \tab \code{Indikator_Okosystem}
#'      \tab Fidelity of indicators
#'      \tab \code{[,1]} \code{"indicatorId"},\cr
#' \tab \tab per major ecosystem \tab \code{[,2]} \code{"ecosystemId"},\cr
#' \tab \tab \tab \code{[,3]} \code{"percent"}.\cr\cr
#' \code{getIndicatorValues2} \tab \code{Verdier}
#'      \tab Reference values and
#'      \tab \code{[, 1]} \code{"indicatorId"},\cr
#' \tab \tab indicator observations \tab \code{[, 2]} \code{"indicatorName"},\cr
#' \tab \tab \tab \code{[, 3]} \code{"areaId"},\cr
#' \tab \tab \tab \code{[, 4]} \code{"areaName"},\cr
#' \tab \tab \tab \code{[, 5]} \code{"yearId"},\cr
#' \tab \tab \tab \code{[, 6]} \code{"yearName"},\cr
#' \tab \tab \tab \code{[, 7]} \code{"verdi"},\cr
#' \tab \tab \tab \code{[, 8]} \code{"nedre_Kvartil"},\cr
#' \tab \tab \tab \code{[, 9]} \code{"ovre_Kvartil"},\cr
#' \tab \tab \tab \code{[,10]} \code{"datatypeId"},\cr
#' \tab \tab \tab \code{[,11]} \code{"datatypeName"},\cr
#' \tab \tab \tab \code{[,12]} \code{"unitOfMeasurement"},\cr
#' \tab \tab \tab \code{[,13]} \code{"customDistributionUUID"},\cr
#' \tab \tab \tab \code{[,14]} \code{"distributionName"},\cr
#' \tab \tab \tab \code{[,15]} \code{"distributionId"},\cr
#' \tab \tab \tab \code{[,16]} \code{"distParam1"},\cr
#' \tab \tab \tab \code{[,17]} \code{"distParam2"}.\cr\cr
#' \code{getRefYearData} \tab \code{T_RefAar}
#'      \tab Ids for observation years and
#'      \tab \code{[,1]} \code{"id"},\cr
#' \tab \tab the reference value category\tab \code{[,2]} \code{"name"},\cr
#' \tab \tab \tab \code{[,3]} \code{"nameEnglish"}.\cr\cr
#' \code{getScalingModelData} \tab \code{T_Skaleringsmodell}
#'      \tab names of scaling models
#'      \tab \code{[,1]} \code{"id"},\cr
#' \tab \tab \tab \code{[,2]} \code{"name"}.
#' }
#'
#' @seealso \code{\link{getToken}} for retrieving tokens from the Nature Index API,
#'  \code{\link{extractContentFromNiapi}}, a general purpose utility function called
#'  by the functions described here that retrieves data from the specified tables
#'  in the Nature Index database, and \code{\link{importDatasetApi}} for importing
#'  from the database all required data for calculating the Nature Index for an
#'  ecosystem.
#'
#' @encoding UTF-8
#' @author Bård Pedersen
#' @name getNIdata
#'
#' @importFrom plyr ldply
#'
#' @param BSunitIds	integer, optional.
#'   \cr\code{getBSunits}: Data for specified BSunits will be retrieved from the
#'   \code{Kommune} table. The default (\code{NULL}) is to retrieve data for all BSunits.
#' @param ecosystemIds	integer, optional.
#'   \cr\code{getIndicatorEcosystemData}: Fidelity of all indicators (or of
#'   those specified in \code{indicatorIds}) to the specified ecosystems, will
#'   be retrieved from the \code{Indikator_Okosystem} table. The default
#'   (\code{ecosystemIds = NULL}) is to retrieve fidelity of all indicators
#'   to all ecosystems.
#' @param indicatorIds	integer, optional. \cr\code{getIndicators2}: Data for specified indicators
#'   will be retrieved from the \code{Indikator} table. The default (\code{NULL}) is to
#'   retrieve data for all indicators.
#'   \cr\code{getIndicatorEcosystemData}: Fidelity of specified indicators to all
#'   ecosystems or those given in \code{ecosystemIds}, will be retrieved from the
#'   \code{Indikator_Okosystem} table.
#'   \cr\code{getICunits}: ICunits for specified indicators will be retrieved from
#'   \code{Omraade}. The default (\code{NULL}) is to retrieve all ICunits.
#'   \cr\code{getIndicatorValues2}: Observations for specified indicators will be
#'   retrieved from \code{Verdier}. The default (\code{NULL}) \emph{is to retrieve no data}.
#' @param icunits	integer, optional.
#'   \cr\code{getICunitBSunit}: Data for specified ICunits will be retrieved from the
#'   \code{Omraade_Kommune} table. The default (\code{NULL}) is to retrieve BSunits for all ICunits.
#' @param refYear	character, optional.
#'   \cr\code{getIndicatorValues2}: Years of the observations to retrieve and
#'   whether to retreive reference values. E.g. \code{refYear = c("2010","Referanseverdi")}
#'   returns observations from 2010 together with reference values. Default = (\code{NULL}),
#'   which means you get all observations as well as reference values.
#'
#' @return All functions except \code{getIndicatorValues2} returns a data frame
#'   with content according to the above table, or an error message if extraction of data
#'   from the database fails.
#'   \cr\cr\code{getIndicatorValues2} returns a list of two elements:
#'   \cr 1) a data frame \code{indicatorValues} of indicator observations content
#'   according to the above table, and
#'   \cr 2) a list \code{customDistributions} of optional distribution objects associated
#'   with some of the observations. Distribution objects are identified through
#'   the variable \code{indicatorValues$customDistributionUUID}
#'
#' @section Note:
#' Column names of returned objects are generated by the API and differ somewhat from the
#' column headings in the corresponding table in the NI database.
#'
#' @examples
#' \dontrun{
#' getBSunits()
#' getEcosystems()
#' getEcosystemBSunitData()
#' getFunctionalGroupData()
#' getICunits()
#' getICunitBSunit()
#' getIndicators2()
#' getIndicatorEcosystemData()
#' getIndicatorValues2(indicatorIds = 1:2,refYear = c("2010","Referanseverdi"))
#' getRefYearData()
#' getScalingModelData()
#' }
#'
NULL
#' @rdname getNIdata
#' @export
getBSunits <- function(BSunitIds = NULL) {

  # Location of resource - API for Kommune_Okosystem table in Norwegian NI database

  APIUrl = .url
  token = .niToken
  methodPath <- "/api/Calculation/municipalities"

  if (!is.null(BSunitIds)) {

    BSunitData <- NULL

    for (bsui in 1:length(BSunitIds)) {

      methodPath <- paste0("/api/Calculation/municipalities/",BSunitIds[bsui])

      # Extract content from API

      rawContent <- extractContentFromNiapi(APIUrl,methodPath,token,as = "text")

      # Issue a warning if content received from API is empty

      if (length(rawContent) == 0) {
        warning(paste0("No data received from API for BSunitId = ", BSunitIds[bsui]," at\n",
                       APIUrl,methodPath, sep=""),
                call. = FALSE)
      }

      # Parse the content into a suitable R object - a data.frame using column names set by the API

      rawResult <- data.frame(RJSONIO::fromJSON(rawContent,simplifyDataFrame = TRUE,nullValue = NA),
                              stringsAsFactors = FALSE)

      BSunitData <- rbind(BSunitData,rawResult)

    }

  } else {

    # Extract content from API

    rawContent = extractContentFromNiapi(APIUrl,methodPath,token,as="text")

    # Issue a warning if content received from API is empty

    if (length(rawContent) == 0) {
      warning(paste0("No data received from API\n",
                     APIUrl,methodPath, sep=""),
              call. = FALSE)
    }

    # Parse the content into a suitable R object - a data.frame using column names set by the API

    BSunitData <- plyr::ldply(RJSONIO::fromJSON(rawContent,simplifyDataFrame = TRUE,nullValue = NA),
                              data.frame, stringsAsFactors = F)
  }

  return(BSunitData)

}

#' @rdname getNIdata
#' @export
getEcosystems <- function() {

  # Location of resource - API for ecosystem table in Norwegian NI database

  APIUrl = .url
  token = .niToken
  methodPath <- "/api/Calculation/ecosystems"

  # Extract content from API

  rawContent = extractContentFromNiapi(APIUrl,methodPath,token,as="text")

  # Issue a warning if content received from API is empty

  if (length(rawContent) == 0) {
    warning(paste0("No data received from API\n",
                   APIUrl, methodPath, sep=""),
            call. = FALSE)
  }

  # Parse the content into a suitable R object - a data.frame using column names set by the API

  ecosystems <- jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE)

  return(ecosystems)
}

#' @rdname getNIdata
#' @export
getEcosystemBSunitData <- function() {

  # Location of resource - API for Indikator_Okosystem table in Norwegian NI database

  APIUrl = .url
  token = .niToken
  methodPath <- "/api/Calculation/municipalityEcosystems"

  # Extract content from API

  rawContent = extractContentFromNiapi(APIUrl,methodPath,token,as="text")

  # Issue a warning if content received from API is empty

  if (length(rawContent) == 0) {
    warning(paste0("No data received from API\n",
                   APIUrl,methodPath, sep=""),
            call. = FALSE)
  }

  # Parse the content into a suitable R object - a data.frame using column names set by the API

  BSunitEcosystem <- jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE)

  return(BSunitEcosystem)
}

#' @rdname getNIdata
#' @export
getFunctionalGroupData <- function() {

  # Location of resource - API for T_FunksjonellGruppe table in Norwegian NI database

  APIUrl = .url
  token = .niToken
  methodPath <- "/api/Calculation/functionalGroups"

  # Extract content from API

  rawContent = extractContentFromNiapi(APIUrl,methodPath,token,as="text")

  # Issue a warning if content received from API is empty

  if (length(rawContent) == 0) {
    warning(paste0("No data received from API\n",
                   APIUrl,methodPath, sep=""),
            call. = FALSE)
  }

  # Parse the content into a suitable R object - a data.frame using column names set by the API

  functionalGroups <- jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE)

  return(functionalGroups)
}

#' @rdname getNIdata
#' @export
getICunits <- function(indicatorIds = NULL) {

  # Location of resource - API for Omraade table in Norwegian NI database

  APIUrl = .url
  token = .niToken
  methodPath <- "/api/Area"

  if (!is.null(indicatorIds)) {

    icunitData <- NULL

    for (ind in 1:length(indicatorIds)) {

      methodPath <- paste0("/api/Area/indicator/",indicatorIds[ind])

      # Extract content from API

      rawContent <- extractContentFromNiapi(APIUrl,methodPath,token,as="text")


      # Issue a warning if content received from API is empty

      if (length(rawContent) == 0) {
        warning(paste0("No data received from API for indicatorId = ", indicatorIds[ind]," at\n",
                       APIUrl,methodPath, sep=""),
                call. = FALSE)
      }

      # Parse the content into a suitable R object - a data.frame using column names set by the API

      icunitData <- rbind(icunitData,jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE))

    }

  } else {

    # Extract content from API

    rawContent = extractContentFromNiapi(APIUrl,methodPath,token,as="text")

    # Issue a warning if content received from API is empty
    if (length(rawContent) == 0) {
      warning(paste0("No data received from API\n",
                     APIUrl,methodPath, sep=""),
              call. = FALSE)
    }

    # Parse the content into a suitable R object - a data.frame using column names set by the API

    icunitData <- jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE)

  }

  icunitData$definitinonArea[is.na(icunitData$definitinonArea)] <- 0
  icunitData$common[is.na(icunitData$common)] <- 0

  icunitData <- icunitData[icunitData$definitinonArea != 1 & icunitData$common != 1,c("id","name","indicatorId")]

  return(icunitData)
}

#' @rdname getNIdata
#' @export
getICunitBSunit <- function(icunits = NULL) {

  # Location of resource - API for Kommune_Okosystem table in Norwegian NI database

  serverUrl <- "https://ninweb17.nina.no"
  apiPath <- "/NaturindeksAPI/api/Area/municipalities"

  APIUrl = .url
  token = .niToken
  methodPath <- "/api/Area/municipalities"

  if (!is.null(icunits)) {

    icunitData <- NULL

    for (icu in 1:length(icunits)) {

      methodPath <- paste0("/api/Area/",icunits[icu],"/municipalities")

      # Extract content from API

      rawContent <- extractContentFromNiapi(APIUrl,methodPath,token,as="text")


      # Issue a warning if content received from API is empty

      if (length(rawContent) == 0) {
        warning(paste0("No data received from API for ICunit = ",icunits[icu]," at\n",
                       APIUrl,methodPath, sep=""),
                call. = FALSE)
      }

      # Parse the content into a suitable R object - a data.frame using column names set by the API

      icunitData <- rbind(icunitData,jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE))

    }

  } else {

    # Extract content from API

    rawContent = extractContentFromNiapi(APIUrl,methodPath,token,as="text")

    # Issue a warning if content received from API is empty
    if (length(rawContent) == 0) {
      warning(paste0("No data received from API\n",
                     APIUrl,methodPath, sep=""),
              call. = FALSE)
    }

    # Parse the content into a suitable R object - a data.frame using column names set by the API

    icunitData <- jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE)

  }

  #  icunitData <- icunitData[icunitData$indId %in% indicators$id,]

  return(icunitData)
}

#' @rdname getNIdata
#' @export
getIndicators2 <- function(indicatorIds = NULL) {

  # Location of resource - API for Indikator table in Norwegian NI database

  APIUrl = .url
  token = .niToken
  methodPath <- "/api/Calculation/indicators"

  if (!is.null(indicatorIds)) {

    indicatorData <- NULL

    for (ind in 1:length(indicatorIds)) {

      methodPath <- paste0("/api/Calculation/indicators/",indicatorIds[ind])

      # Extract content from API

      rawContent <- extractContentFromNiapi(APIUrl,methodPath,token,as = "text")

      # Issue a warning if content received from API is empty

      if (length(rawContent) == 0) {
        warning(paste0("No data received from API for indicatorId = ", indicatorIds[ind]," at\n",
                       APIUrl,methodPath, sep=""),
                call. = FALSE)
      }

      # Parse the content into a suitable R object - a data.frame using column names set by the API

      rawResult <- data.frame(RJSONIO::fromJSON(rawContent,simplifyDataFrame = TRUE,nullValue = NA),
                              stringsAsFactors = FALSE)

      indicatorData <- rbind(indicatorData,rawResult)

    }

  } else {

    # Extract content from API

    rawContent = extractContentFromNiapi(APIUrl,methodPath,token,as = "text")

    # Issue a warning if content received from API is empty
    if (length(rawContent) == 0) {
      warning(paste0("No data received from API\n",
                     APIUrl,methodPath, sep=""),
              call. = FALSE)
    }

    # Parse the content into a suitable R object - a data.frame using column names set by the API

    indicatorData <- plyr::ldply(RJSONIO::fromJSON(rawContent,simplifyDataFrame = TRUE,nullValue = NA),
                                 data.frame, stringsAsFactors = F)
  }

  return(indicatorData)

}

#' @rdname getNIdata
#' @export
getIndicatorEcosystemData <- function(ecosystemIds = NULL, indicatorIds = NULL) {

  # Location of resource - API for Indikator_Okosystem table in Norwegian NI database

  APIUrl = .url
  token = .niToken
  methodPath <- "/api/Calculation/indicatorEcosystems"
  methodPath2 <- "/api/Calculation/indicatorEcosystems"

  if (!is.null(ecosystemIds)) {

    indicatorEcosystem <- NULL

    for (eco in 1:length(ecosystemIds)) {

      methodPath <- paste0("/api/Calculation/indicatorEcosystems/ecosystem/",ecosystemIds[eco])

      # Extract content from API

      rawContent = extractContentFromNiapi(APIUrl,methodPath,token,as="text")

      # Issue a warning if content received from API is empty

      if (length(rawContent) == 0) {
        warning(paste0("No data received from API for ecosystemId = ", ecosystemIds[eco]," at\n",
                       APIUrl,methodPath, sep=""),
                call. = FALSE)
      }

      # Parse the content into a suitable R object - a data.frame using column names set by the API

      indicatorEcosystem <- rbind(indicatorEcosystem,jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE))

    }

    # Remove items with zero fidelities

    indicatorEcosystem <- indicatorEcosystem[indicatorEcosystem$percent > 0,]

    if (!is.null(indicatorIds)) {
      indicatorEcosystem <- indicatorEcosystem[indicatorEcosystem$indicatorId %in% indicatorIds,]

      if (length(indicatorEcosystem$indicatorId) == 0) {
        warning(paste0("No nonzero fidelities received from API for indicators ",
                       paste0(indicatorIds,collapse=" , ")," in ecosystems ",
                       paste0(ecosystemIds,collapse=" , "), " at\n",APIUrl,methodPath2, sep=""),
                call. = FALSE)
      }
    }

  } else if (!is.null(indicatorIds)) {

    indicatorEcosystem <- NULL

    for (ind in 1:length(indicatorIds)) {

      methodPath <- paste0("/api/Calculation/indicatorEcosystems/indicator/",indicatorIds[ind])

      # Extract content from API

      rawContent = extractContentFromNiapi(APIUrl,methodPath,token,as="text")

      # Issue a warning if content received from API is empty

      if (length(rawContent) == 0) {
        warning(paste0("No data received from API for indicatorId = ", indicatorIds[ind]," at\n",
                       APIUrl,methodPath, sep=""),
                call. = FALSE)
      }

      # Parse the content into a suitable R object - a data.frame using column names set by the API

      indicatorEcosystem <- rbind(indicatorEcosystem,jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE))
    }

    # Remove items with zero fidelities

    indicatorEcosystem <- indicatorEcosystem[indicatorEcosystem$percent > 0,]

  } else {

    # Extract content from API

    rawContent = extractContentFromNiapi(APIUrl,methodPath,token,as="text")

    # Issue a warning if content received from API is empty

    if (length(rawContent) == 0) {
      warning(paste0("No data received from API\n",
                     APIUrl,methodPath, sep=""),
              call. = FALSE)
    }

    # Parse the content into a suitable R object - a data.frame using column names set by the API

    indicatorEcosystem <- jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE)

    # Remove items with zero fidelities

    indicatorEcosystem <- indicatorEcosystem[indicatorEcosystem$percent > 0,]

  }

  return(indicatorEcosystem)
}

#' @rdname getNIdata
#' @export
getIndicatorValues2 <- function(indicatorIds = NULL, refYear = NULL) {

  APIUrl = .url
  token = .niToken
  methodPath2 <- "/api/Calculation/indicator/values"

  if (is.null(indicatorIds)) {

    indicatorValues <- NULL

  } else if (is.null(refYear)) {

    rawResult <- NULL

    for (ind in 1:length(indicatorIds)) {
      message(paste("\nReading values for indicator",indicatorIds[ind]))

      methodPath <- paste0("/api/Calculation/indicator/",indicatorIds[ind],"/values")

      # Extract content from API

      rawContent <- extractContentFromNiapi(APIUrl,methodPath,token,as ="text")

      # Issue a warning if content received from API is empty

      if (length(rawContent) == 0) {

        # Issue a warning if content received from API is empty
        warning(paste0("No data received from API for indicator = ",indicatorIds[ind]," at\n",
                       APIUrl,methodPath, sep=""),
                call. = FALSE)

      } else {

        # Parse the content into a suitable R object - a data.frame using column names set by the API
        xxx <- jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE)
        rm(rawContent)

        # Add parsed content to the rawResult data.frame
        rawResult <- rbind(rawResult,xxx)
      }

    }

    # Restructure the parsed object into an object of type indicatorData

    indicatorValues <- list("indicatorValues" = rawResult[!(names(rawResult) %in% c("customDistributionObject"))])

    customDistributions <- as.list(rawResult$customDistributionObject[!is.na(rawResult$customDistributionObject)])
    names(customDistributions) <- rawResult$customDistributionUUID[!is.na(rawResult$customDistributionUUID)]

    backToDist <- function(x){
      unserialize(charToRaw(x[[1]]))
    }

    indicatorValues$customDistributions <- lapply(customDistributions, backToDist)

    class(indicatorValues) <- c("indicatorData", "list")

  } else {

    rawResult <- NULL

    for (ind in 1:length(indicatorIds)) {
      message(paste("\nReading values for indicator",indicatorIds[ind]))
      for (reff in 1:length(refYear)) {

        methodPath <- paste0("/api/Calculation/indicator/",indicatorIds[ind],"/values/years/",refYear[reff])

        # Extract content from API

        rawContent <- extractContentFromNiapi(APIUrl,methodPath,token,as="text")

        # Issue a warning if content received from API is empty

        if (length(rawContent) == 0) {

          # Issue a warning if content received from API is empty
          warning(paste0("No data received from API for indicator = ",indicatorIds[ind]," at\n",
                         APIUrl,methodPath, sep=""),
                  call. = FALSE)

        } else {

          # Parse the content into a suitable R object - a data.frame using column names set by the API
          xxx <- jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE)
          rm(rawContent)

          # Add parsed content to the rawResult data.frame
          rawResult <- rbind(rawResult,xxx)
        }

      }
    }

    # Restructure the parsed object into an object of type indicatorData

    indicatorValues <- list("indicatorValues" = rawResult[!(names(rawResult) %in% c("customDistributionObject"))])

    customDistributions <- as.list(rawResult$customDistributionObject[!is.na(rawResult$customDistributionObject)])
    names(customDistributions) <- rawResult$customDistributionUUID[!is.na(rawResult$customDistributionUUID)]

    backToDist <- function(x){
      unserialize(charToRaw(x[[1]]))
    }

    indicatorValues$customDistributions <- lapply(customDistributions, backToDist)

    class(indicatorValues) <- c("indicatorData", "list")
  }

  if (length(indicatorValues$indicatorValues) == 0) {

    # Issue a warning if content received from API is empty
    warning(paste0("No data received from API for indicators ",indicatorIds," at\n",
                   APIUrl,methodPath2, sep=""),
            call. = FALSE)

  } else if (length(which(!(indicatorIds %in% indicatorValues$indicatorValues$indicatorId))) > 0) {

    # Issue a warning if data for some indicators are missing from content received from API
    xxx <- which(!(indicatorIds %in% indicatorValues$indicatorValues$indicatorId))
    warning(paste0("No data received from API for indicators ",indicatorIds[xxx]," at\n",
                   APIUrl,methodPath2, sep=""),
            call. = FALSE)

  }

  return(indicatorValues)
}

#' @rdname getNIdata
#' @export
getRefYearData <- function() {

  # Location of resource - API for T_RefAar table in Norwegian NI database

  APIUrl = .url
  token = .niToken
  methodPath <- "/api/Calculation/refYears"

  # Extract content from API

  rawContent = extractContentFromNiapi(APIUrl, methodPath,token,as="text")

  # Issue a warning if content received from API is empty

  if (length(rawContent) == 0) {
    warning(paste0("No data received from API\n",
                   APIUrl, methodPath, sep=""),
            call. = FALSE)
  }

  # Parse the content into a suitable R object - a data.frame using column names set by the API

  refYearTable <- jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE)

  return(refYearTable)
}

#' @rdname getNIdata
#' @export
getScalingModelData <- function() {

  # Location of resource - API for T_Skaleringsmodell table in Norwegian NI database

  serverUrl <- "https://ninweb17.nina.no"
  apiPath <- "/NaturindeksAPI/api/Calculation/scalingModels"

  APIUrl = .url
  token = .niToken
  methodPath <- "/api/Calculation/scalingModels"

  # Extract content from API

  rawContent = extractContentFromNiapi(APIUrl,methodPath,token,as="text")

  # Issue a warning if content received from API is empty

  if (length(rawContent) == 0) {
    warning(paste0("No data received from API\n",
                   APIUrl,methodPath, sep=""),
            call. = FALSE)
  }

  # Parse the content into a suitable R object - a data.frame using column names set by the API

  scalingModels <- jsonlite::fromJSON(rawContent, simplifyDataFrame = TRUE)

  return(scalingModels)
}
