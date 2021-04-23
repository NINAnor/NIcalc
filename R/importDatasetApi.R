#' Import Dataset
#'
#' Imports a data set from the Norwegian Nature Index database via API
#'
#' \code{importDatasetApi} imports a complete data set for calculating the Nature
#' Index or a thematic index by reading various tables in the NI database via the
#' [The Nature Index Application Programming Interface](https://www8.nina.no/NaturindeksNiCalc/index.html)
#' and returns the data set as a list of class \code{\link{niDataImport}}.
#'
#' The function includes options for assembling complete data sets for an ecosystem,
#' for e.g. subsequent calculation of the Nature Index. It also includes options
#' for assembling a data set for a particular set of indicators, for e.g. subsequent
#' calculation of a thematic index. Further, it includes options for selecting data
#' from particular years, and for choosing between Norwegian (default) and English
#' ecosystem and indicator names.
#'
#' \code{importDatasetApi} contains some simple procedures for data washing.
#' It removes from the data set
#' \itemize{
#' \item passive indicators and indicators with zero fidelities to ecosystems in
#' question.
#' \item redundant BSunits and ICunits, i.e. units with no indicator observations
#' from the ecosystem in question.
#' \item ICunits without reference values or with reference values equal zero.
#' \item ICunits with only missing observations or with only reference values
#' and no other observations.
#' }
#'
#' @seealso \code{\link{getNIdata}} for functions called by \code{importDatasetApi}
#' that import data from specific tables in the NI database, and
#' \code{\link{niDataImport}}.
#'
#' @name importDatasetApi
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @import tibble
#'
#' @param username	character. Your username for accessing the NI database.
#' @param password	character. Your password for accessing the NI database.
#' @param eco	character; optional vector specifying ecosystems to include in the
#'   dataset. Ignored with nonempty \code{indic}.
#' @param indic	character; optional vector specifying indicators to include in
#'   the dataset.
#' @param year	character; optional vector specifying years to include in the
#'   dataset.
#' @param norwegian	logical; use norwegian (\code{TRUE}, default) or english
#'   (\code{FALSE}) names for indicators and ecosystems.
#' @param refYearCode	integer; code used for reference values. Defaults to
#'   \code{0} which is the code used in the NI database.
#'
#' @return A list of class \code{\link{niDataImport}} with
#'   the following elements:
#'   \tabular{lll}{\code{[[1]]} \tab \code{$indicators} \tab a data.frame with indicator data.\cr
#'      \code{[[2]]} \tab \code{$referenceValues} \tab a list of reference values.\cr
#'      \code{[[3]]} \tab \code{$indicatorObservations} \tab a list of indicator observations.\cr
#'      \code{[[4]]} \tab \code{$ICunits} \tab a data.frame describing ICunits.\cr
#'      \code{[[5]]} \tab \code{$BSunits} \tab a data.frame with BSunit data.\cr
#'      \code{[[6]]} \tab \code{$ecosystems} \tab a data.frame with ecosystem names and IDs.}
#'   The vignette \code{objectsInNIcalc} gives a more detailed description of
#'   \code{niDataImport} lists.
#'
#' @examples
#' \dontrun{
#' # Import data for the calculation of a Nature Index for forests in Norway
#' # covering the years 1990, 2000, 2010, 2014, 2019.
#'
#' forestImport <- importDatasetApi(username = "your username",
#'                                  password = "your password",
#'                                  eco = "Skog",
#'                                  indic = NULL,
#'                                  year = c("1990","2000","2010","2014","2019"),
#'                                  norwegian = TRUE,
#'                                  refYearCode = 0)
#'
#' # Import data for the calculation of a thematic index for game birds in Norway
#' # in 2000
#'
#' amphibImport <- importDatasetApi(username = "your username",
#'                                  password = "your password",
#'                                  eco = NULL,
#'                                  indic = c("Fjellrype","Lirype","Orrfugl","Storfugl"),
#'                                  year = c("2000"),
#'                                  norwegian = TRUE,
#'                                  refYearCode = 0)
#'
#' # Import data for the calculation of an indicator index
#'
#' cormorImport <- importDatasetApi(username = "your username",
#'                                  password = "your password",
#'                                  eco = NULL,
#'                                  indic = c("Toppskarv"),
#'                                  year = c("1990","2000","2010","2014","2019"),
#'                                  norwegian = TRUE,
#'                                  refYearCode = 0)
#'
#' }
#'
#' @export

importDatasetApi <- function(
  username = NULL,
  password = NULL,
  eco = NULL,
  indic = NULL,
  year = NULL,
  norwegian = TRUE,
  refYearCode = 0) {

  if (!is.null(indic)) {eco <- NULL}

  # Retrieve token from The Nature Index API to get access to the NI database

  getToken(username = username, password = password)

  # Read and reformat ecosystem table from the T_Okosystem table in the Nature index SQL database

  message("\nImporting from 'T_Okosystem' table ..... ")

  dataImport <- getEcosystems()

  if (norwegian) {
    ecosystems <- tibble::tibble(id = dataImport$id,
                                 name = dataImport$name)
  } else {
    ecosystems <- tibble::tibble(id = dataImport$id,
                                 name = dataImport$nameEnglish)
  }

  if (!is.null(eco)) {
    notEco <- which (!(eco %in% ecosystems$name))
    if (length(notEco) > 0) {
      ecosystems <- NULL
      stop(paste0("'",eco[notEco], "' is not an ecosystem in NI database.\n       ", sep=""),
           call. = FALSE)
    }
    ecosystems <- ecosystems[ecosystems$name %in% eco,]
  }

  remove(dataImport)

  # Read and reformat municipality statistics from the Kommune and Kommune_Okosystem tables in the Nature index
  # SQL database

  message("\nImporting from 'Kommune_Okosystem' table ..... ")

  dataImport <- getEcosystemBSunitData()

  muniEco <- tibble::tibble(munId = as.character(dataImport$municipalityId),
                            ecoId = dataImport$ecosystemId,
                            area = dataImport$area)

  xxx <- sort(unique(dataImport$municipalityId))
  yyy <- as.character(trunc(as.integer(xxx)/100))

  parts <- c(rep(1,7),rep(2,3),rep(3,4),rep(4,3),rep(5,3),rep(6,10))
  zzz <- as.character(parts[as.integer(yyy)])

  municipalities <- tibble::tibble(id = as.character(xxx),
                                   countyId = yyy,
                                   partId = zzz)

  if (length(eco) > 0) {
    muniEco <- muniEco[muniEco$ecoId %in% ecosystems$id & muniEco$area > 0,]
  }

  for (i in ecosystems$name) {
    xxx <- dimnames(municipalities)[[2]]
    iId <- ecosystems$id[ecosystems$name == i]
    yyy <- muniEco[muniEco$ecoId == iId,c("munId","area")]
    municipalities <- merge(municipalities, yyy, by.x = "id", by.y = "munId", all.x = TRUE)
    dimnames(municipalities)[[2]] <- c(xxx,i)
    municipalities[,i][is.na(municipalities[,i])] <- 0
  }

  municipalities <- municipalities[rowSums(as.matrix(municipalities[,ecosystems$name])) > 0,]

  remove(dataImport, muniEco)

  message("\nImporting from 'Kommune' table ..... ")

  dataImport <- getBSunits(municipalities$id)

  municipalities2 <- tibble::tibble(id = dataImport$id,
                                    name = as.character(dataImport$name),
                                    totalArea = dataImport$area,
                                    terrestrialArea = dataImport$terrestrial,
                                    marineArea = dataImport$marine,
                                    county = dataImport$county,
                                    part = dataImport$part,
                                    centerLat = dataImport$centerLat,
                                    centerLong = dataImport$centerLong)
  municipalities <- merge(municipalities, municipalities2, by.x = "id", by.y = "id", all.x = TRUE)
  municipalities$id <- as.integer(municipalities$id)
  municipalities <- municipalities[,!(names(municipalities) %in% c("countyId","partId"))]

  remove(dataImport, municipalities2)

  # Read and reformat indicator data from the Indikator, Indikator_Okosystem, T_Skaleringsmodell and
  # T_FunksjonellGruppe tables in the Nature index SQL database,

  # Import indicator data

  message("\nImporting from 'Indikator' table ..... ")

  dataImport <- getIndicators2()


  if (norwegian) {
    indicators <- tibble::tibble(id = dataImport$id,
                                 active = dataImport$active,
                                 name = as.character(dataImport$name),
                                 functionalGroupId = dataImport$functionalGroupId,
                                 scalingModelId = dataImport$scalingModel,
                                 keyElement = dataImport$keyElement)
  } else {
    indicators <- tibble::tibble(id = dataImport$id,
                                 active = dataImport$active,
                                 name = as.character(dataImport$nameEnglish),
                                 functionalGroupId = dataImport$functionalGroupId,
                                 scalingModelId = dataImport$scalingModel,
                                 keyElement = dataImport$keyElement)
  }
  remove(dataImport)

  # Remove passive indicators

  indicators <- indicators[indicators$active,]

  # Import fidelity data

  message("\nImporting from 'Indikator_Okosystem' table ..... ")

  dataImport <- getIndicatorEcosystemData(ecosystemIds = ecosystems$id)

  indiEco <- tibble::tibble(indId = dataImport$indicatorId,
                            ecoId = dataImport$ecosystemId,
                            fidelity = dataImport$percent)

  # Remove zero fidelities and fidelities to ecosystems not included in dataset

  indiEco <- indiEco[indiEco$ecoId %in% ecosystems$id & indiEco$fidelity > 0,]

  # Remove indicators with zero fidelities to ecosystems in dataset

  indicators <- indicators[indicators$id %in% unique(indiEco$indId),]

  # Add fidelities to indicators data.frame

  for (i in ecosystems$name) {
    xxx <- dimnames(indicators)[[2]]
    iId <- ecosystems$id[ecosystems$name == i]
    yyy <- indiEco[indiEco$ecoId == iId,c("indId","fidelity")]
    indicators <- merge(indicators, yyy, by.x = "id", by.y = "indId", all.x = TRUE)
    dimnames(indicators)[[2]] <- c(xxx,i)
  }

  indicators[,ecosystems$name][is.na(indicators[,ecosystems$name])] <- 0

  remove(dataImport,indiEco)

  # If nonempty indic, keep only indicators in indic, their ecosystems and municipalities.

  if (!is.null(indic)) {
    notIndic <- which (!(indic %in% indicators$name))
    if (length(notIndic) > 0) {
      stop(paste0("'",indic[notIndic], "' is not an indicator in NI database.\n       ", sep=""),
           call. = FALSE)
    }
    indicators <- indicators[indicators$name %in% indic,]
    removeEcosystems <- names(which(colSums(indicators[,ecosystems$name]) <= 0))
    indicators <- indicators[,which(!(dimnames(indicators)[[2]] %in% removeEcosystems))]
    ecosystems <- ecosystems[!(ecosystems$name %in% removeEcosystems),]
    municipalities <- municipalities[,which(!(dimnames(municipalities)[[2]] %in% removeEcosystems))]
    municipalities <- municipalities[rowSums(as.matrix(municipalities[,ecosystems$name])) > 0,]
    remove(removeEcosystems)
  }

  # Import functional group data

  message("\nImporting from 'T_FunksjonellGruppe' table ..... ")

  dataImport <- getFunctionalGroupData()

  if (norwegian) {
    trophicLabels <- tibble::tibble(id = dataImport$id,
                                    functionalGroup = as.character(dataImport$name))

  } else {
    trophicLabels <- tibble::tibble(id = dataImport$id,
                                    functionalGroup = as.character(dataImport$nameEnglish))
  }

  # Add functional group names to indicators data.frame

  indicators <- merge(indicators, trophicLabels,
                      by.x = "functionalGroupId", by.y = "id", all.x = TRUE)
  # indicators <- indicators[,names(indicators)[names(indicators) != "functionalGroupId"]]

  remove(dataImport, trophicLabels)

  # Import scaling model data

  message("\nImporting from 'T_Skaleringsmodell' table ..... ")

  dataImport <- getScalingModelData()

  scalingModels <- tibble::tibble(id = dataImport$id,
                                  scalingModel = as.character(dataImport$name))

  # Add scaling model names to indicators data.frame

  indicators <- merge(indicators, scalingModels,
                      by.x = "scalingModelId", by.y = "id", all.x = TRUE)
  # indicators <- indicators[,names(indicators)[names(indicators) != "scalingModelId"]]

  remove(dataImport, scalingModels)

  # Read area data from the Omraade and Omraade_Kommune tables in the Nature index SQL database,
  #	remove definition areas, predefined areas and areas for passive indicators from the dataframe

  # Import ICunit by BCunit data

  message("\nImporting from 'Omraade_Kommune' table ..... ")

  dataImport <- getICunitBSunit()

  indicatorAreas <- tibble::tibble(id = dataImport$areaId,
                                   BSunitId = dataImport$municipalityId)
  remove(dataImport)

  # Import indicator by ICunit data

  message("\nImporting from 'Omraade' table ..... ")

  dataImport <- getICunits(indicatorIds = indicators$id)

  indicArea <- tibble::tibble(areaId = dataImport$id,
                              name = dataImport$name,
                              indId = dataImport$indicatorId)

  # Remove ICunits for indicators not in dataset from indicArea and indicatorAreas.
  # Remove BSunits not in dataset from indicatorAreas.

  indicArea <- indicArea[indicArea$indId %in% indicators$id,]

  indicatorAreas <- indicatorAreas[(indicatorAreas$id %in% indicArea$areaId) &
                                     (indicatorAreas$BSunitId %in% municipalities$id),]

  indicatorAreas <- merge(indicatorAreas, indicArea, by.x = "id", by.y = "areaId")

  remove(dataImport, indicArea)

  # Read and reformat values data from the Verdier-, T_Distributions-,
  # and T_RefAar tables in the Nature index SQL database.
  #	Remove values for passive indicators
  # Remove observations which is not used in NI-calculation.

  # Import indicator observations

  message("\nImporting from 'T_RefAar' table ..... ")

  refYearTable <- getRefYearData()

  message("\nImporting from 'Verdier' table ..... ")

  if (!is.null(year)) {

    refYear <- c(year,refYearTable$name[refYearTable$id == refYearCode])
    dataImport <- getIndicatorValues2(indicatorIds = indicators$id, refYear = refYear)

  } else {

    dataImport <- getIndicatorValues2(indicatorIds = indicators$id, refYear = NULL)

  }

  indicatorValues <- tibble::tibble(indId = dataImport$indicatorValues$indicatorId,
                                    indName = dataImport$indicatorValues$indicatorName,
                                    ICunitId = dataImport$indicatorValues$areaId,
                                    ICunitName = dataImport$indicatorValues$areaName,
                                    yearId = dataImport$indicatorValues$yearId,
                                    yearName = dataImport$indicatorValues$yearName,
                                    expectedValue = dataImport$indicatorValues$verdi,
                                    lowerQuantile = dataImport$indicatorValues$nedre_Kvartil,
                                    upperQuantile = dataImport$indicatorValues$ovre_Kvartil,
                                    customDistributionUUID = dataImport$indicatorValues$customDistributionUUID,
                                    distributionFamilyId = dataImport$indicatorValues$distributionId,
                                    distributionFamilyName = dataImport$indicatorValues$distributionName,
                                    distParameter1 = dataImport$indicatorValues$distParam1,
                                    distParameter2 = dataImport$indicatorValues$distParam2)
  customDistributions <- dataImport$customDistributions

  op <- options()
  options(warn = -1)
  remove(dataImport)
  options(op)

  if (!norwegian) {
    indicatorValues$yearName[indicatorValues$yearId == refYearCode] <-
      refYearTable$nameEnglish[refYearTable$id == refYearCode]
  }

  indicatorValues$expectedValue[indicatorValues$expectedValue < 0] <- NA
  indicatorValues$lowerQuantile[indicatorValues$lowerQuantile < 0] <- NA
  indicatorValues$upperQuantile[indicatorValues$upperQuantile < 0] <- NA

  # Remove indicator observations not in dataset from indicatorValues

  indicatorValues <- indicatorValues[indicatorValues$ICunitId %in% unique(indicatorAreas$id),]

  # Split indicatorValues into two objects, one for true indicator observations and one for reference values.

  referenceValues <- indicatorValues[indicatorValues$yearId == refYearCode,]
  indicatorValues <- indicatorValues[indicatorValues$yearId != refYearCode,]

  # Remove areas without reference values or with reference values equal to zero
  # Remove areas with only missing observations from the data set.
  # This part relies on all data being non-negative and missing.value.code being a negative number.
  # Remove areas with only reference values and no other observations from the data set.

  referenceValues <- referenceValues[referenceValues$expectedValue > 0,]

  taMed <- unique(indicatorValues$ICunitId[indicatorValues$expectedValue >= 0])
  indicatorValues <- indicatorValues[indicatorValues$ICunitId %in% taMed,]

  indicatorValues <- indicatorValues[indicatorValues$ICunitId %in% referenceValues$ICunitId,]
  referenceValues <- referenceValues[referenceValues$ICunitId %in% indicatorValues$ICunitId,]

  # Remove ICunits, BSunits and indicators with no remaining data

  indicatorAreas <- indicatorAreas[indicatorAreas$id %in% referenceValues$ICunitId,]
  taMed <- unique(indicatorAreas$BSunitId)
  municipalities <- municipalities[municipalities$id %in% taMed,]
  taMed <- unique(indicatorAreas$indId)
  indicators <- indicators[indicators$id %in% taMed,]

  # Create reference and indicatorObservation lists

  reference <- list("referenceValues" = referenceValues,
                    "customDistributions" =
                      customDistributions[which(names(customDistributions) %in% referenceValues$customDistributionUUID)])

  indicatorObservations <- list("indicatorValues" = indicatorValues,
                                "customDistributions" =
                                  customDistributions[which(names(customDistributions) %in% indicatorValues$customDistributionUUID)])

  # Create niDataSet object as a list of class niDataImport

  op <- options()
  options(warn = -1)
  niDataSet <- niDataImport(indicators = indicators,
                            referenceValues = reference,
                            indicatorObservations = indicatorObservations,
                            ICunits = indicatorAreas,
                            BSunits = municipalities,
                            ecosystems = ecosystems)
  options(op)

  return(niDataSet)
}
