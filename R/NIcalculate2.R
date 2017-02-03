#' Calculate Matrix of Draws of NI
#'
#' Function for calculating \code{nsim} draws of NI from \code{nsim} sets of
#' (N.omr) scaled observations for (N.ind) indicators and corresponding
#' areaweights, trophic weights and fidelity weights for a set of (N.kom) basic
#' spatial units or a set of (N.reg) aggregated NI-areas each consisting of one
#' or several basic spatial units. Scaled indicatorobservations are calculated
#' in \code{scaleobs.fct}, while area weights are calculated in
#' \code{areaweights.fct}, and the product of trophic weights and fidelity
#' weights in \code{munweights.fct}.
#'
#' NI is calculated for a set of NI-areas, while scaled indicator observations,
#' on the other hand, refer to indicator specific areas that may vary in
#' geographical extent among indicators, and will in general not correspond to
#' the NI-areas. Thus, \code{NIcalculate} needs input about the spatial extent
#' of each indicator area and NI-area.
#'
#' \code{NIcalculate} returns a matrix of \code{nsim} draws of NI for each
#' NI-area (or basic spatial unit).
#'
#' @name NIcalculate2
#' @author Bård Pedersen
#'
#' @param Indicator.area.ind (optional) data frame containing named columns IndicatorareaID and Indicator_name, detailing vector of unique
#'   indicator area IDs and vector of indicator names corresponding to the entries in \code{Areaind.Name}, respectively.
#'   These column can be given separately, but  precedence is given to the data frame.
#' @param Indicator.area (optional) data frame containing named columns IndicatorareaID and Basicunits, detailing vector of unique
#'   indicator area IDs and vector of names of corresponding basic spatial units, respectively. These column can be given separately, but
#'   precedence is given to the data frame.
#' @param Observations  (optional) data frame containing named column IndicatorareaID with data on indicator area IDs
#'   corresponding to the sequence of indicator observations in scaled.bootmat. This can be optionallly be provided as FK_OmraadeID, but
#'   precedence is given to the data frame.
#' @param IndicatorareaID	(optional) integer,	length = \code{N.omr}, vector of unique
#'   indicator area IDs. A warning is given if this data is also provided as Indicator.area.ind, informing that this input is ignored.
#' @param Indicator_name (optional) character,		length = \code{N.omr}, vector of
#'   indicator names corresponding to the entries in \code{Areaind.Name}. A warning is given if this data is also provided as Indicator.area.ind, informing that this input is ignored.
#' @param Area.Name		(optional) integer,		length=length(Area.Name)	vector of indicator area
#'   IDs, where each indicator area ID is repeated for each basic spatial unit
#'   it consist of.  A warning is given if this data is also provided as Indicator.area, informing that this input is ignored.
#' @param Area.Municipality (optional) character,		length=length(Area.Name)	vector of names
#'   of corresponding basic spatial units.  A warning is given if this data is also provided as Indicator.area, informing that this input is ignored.
#' @param FK_OmraadeID	(optional) integer,		length = N.omr			vector of indicator area IDs
#'   corresponding to the sequence of indicator observations in scaled.bootmat.  A warning is given if this data is also provided as Observations, informing that this input is ignored.
#' @param nsim			integer,		length = 1				number of draws (simulations) in the
#'   estimation of NI
#' @param scaled.bootmat	double matrix,	dim = N.omr x nsim,			nsim random draws
#'   of scaled indicator observations or nsim copies of scaled expected values
#' @param Weights.trof	double matrix,	dim = N.kom x N.ind	,		trophic weights *
#'   fidelity weights
#' @param Weights.reg.area	double array,	dim = N.kom x N.ind x N.reg	array of
#'   area weights
#'
#' @return \code{NI}, double matrix,	dim=(N.kom or N.reg) x \code{nsim} 	matrix
#'   of NI draws
#' @export
#' @examples
#' \dontrun{
#'  data(Observations)
#'
#'  obs <- data.frame(
#'  Observations,
#'  elicitate(
#'    expected.value=Observations$Expected.value,
#'    lower=Observations$Lower,
#'    upper = Observations$Upper)[,1:3])
#'
#'  bootmat <- samplebootmat(
#'    ValueID=obs$ValueID,
#'    Value=obs$Expected.value,
#'    RefobsID = obs$ReferenceYearID,
#'    DistID=obs$FK_DistID,
#'    mu=obs$mu,
#'    sig=obs$sig,
#'    nsim=10)
#'
#'  scaledBootmat <- scaleobs(
#'    ValueID=obs$ValueID,
#'    FK_OmraadeID=obs$IndicatorareaID,
#'    FK_IndicatorID=obs$IndicatorID,
#'    FK_RefAarID=obs$ReferenceYearID,
#'    nsim=10,
#'    bootmat=bootmat,
#'    ref.value.code=0,
#'    IndicatorID=Indic$IndicatorID,
#'    FK_Scalingmodel=Indic$Scalingmodel)
#'
#'
#'  weightsTrof <- munweights(
#'    Municipalities=BSunits$Basicunit,
#'    Indicators=Indic$Indicator_name,
#'    FK_TrophicgroupID=Indic$TrophicgroupID,
#'    Key.indicators=Indic$Key.indicators,
#'    Fidelity=Indic$Fidelity,
#'    Areaind.Name=Indicator.area.ind$IndicatorareaID,
#'    Areaind.Indicator=Indicator.area.ind$Indicator_name,
#'    Area.Name=Indicator.area$IndicatorareaID,
#'    Area.Municipality=Indicator.area$Basicunit)
#'
#'
#'
#' ##To loop over several years and present summary statistics
#'
#'  NIresult <- NULL
#'
#'  years <- unique(Observations$ReferenceYearID[Observations$ReferenceYearID!=0])
#'
#'  for (i in years) {
#'      scaled.bootmatyeari <- scaledBootmat[which(Observations$ReferenceYearID == i),]
#'
#'    NIi <- summary(NIcalculate2(
#'      Indicator.area.ind = Indicator.area.ind,
#'      Indicator.area = Indicator.area,
#'      Observations = Observations[which(Observations$ReferenceYearID == i),],
#'      nsim = 10,
#'      scaled.bootmat = scaled.bootmatyeari,
#'      Weights.trof = munweightsforall)
#'      )
#'
#'      year <- rep(i,dim(NIi) [1])
#'      iresult <- cbind(year,NIi)
#'      NIresult <- rbind(NIresult,iresult)
#'    }
#'
#'    NIresult
#'
#' ##To calculate for single year
#'
#'  obs <- obs[obs$ReferenceYearID %in% c(0,2), ]
#'
#'  bootmat <- samplebootmat(
#'    ValueID=obs$ValueID,
#'    Value=obs$Expected.value,
#'    RefobsID = obs$ReferenceYearID,
#'    DistID=obs$FK_DistID,
#'    mu=obs$mu,
#'    sig=obs$sig,
#'    nsim=10)
#'
#'  scaledBootmat <- scaleobs(
#'    ValueID=obs$ValueID,
#'    FK_OmraadeID=obs$IndicatorareaID,
#'    FK_IndicatorID=obs$IndicatorID,
#'    FK_RefAarID=obs$ReferenceYearID,
#'    nsim=10,
#'    bootmat=bootmat,
#'    ref.value.code=0,
#'    IndicatorID=Indic$IndicatorID,
#'    FK_Scalingmodel=Indic$Scalingmodel)
#'
#'  scaled.bootmatyear2 <- scaledBootmat[which(obs$ReferenceYearID == 2),]
#'
#'  NIresYear2 <- NIcalculate2(
#'    Indicator.area.ind = Indicator.area.ind,
#'    Indicator.area = Indicator.area,
#'    Observations = Observations[which(Observations$ReferenceYearID == 2),],
#'    nsim = 10,
#'    scaled.bootmat = scaled.bootmatyear2,
#'    Weights.trof = munweightsforall)
#'
#'    NIresYear2
#'
#'    ##To specify input arguments separately
#'  NIcalculate2(
#'    IndicatorareaID = Indicator.area.ind$IndicatorareaID,
#'    Indicator_name = Indicator.area.ind$Indicator_name,
#'    Area.Name=Indicator.area$IndicatorareaID,
#'    Area.Municipality=Indicator.area$Basicunit,
#'    FK_OmraadeID=Observations$IndicatorareaID[Observations$ReferenceYearID == 2],
#'    nsim=10,
#'    scaled.bootmat=scaled.bootmatyeari,
#'    Weights.trof=munweightsforall)
#'
#'}
#' @section Notes: Area weights should not be entered to \code{NIcalculate} if
#'   NI is calculated for basic spatial units.
#'
#' @seealso \code{\link{NIestimate}}
#'   NIcalculation()


NIcalculate2 <- function(
  Indicator.area.ind = NULL,
  Indicator.area = NULL,
  Observations = NULL,
  nsim,
  scaled.bootmat,
  Weights.trof,
  Weights.reg.area = NULL,
  IndicatorareaID = NULL,
  Indicator_name = NULL,
  Area.Name = NULL,
  Area.Municipality = NULL,
  FK_OmraadeID=NULL) {

  if(!is.null(Indicator.area.ind)){
    areaindName = Indicator.area.ind$IndicatorareaID
    areaindIndicator = Indicator.area.ind$Indicator_name
  } else
  {
    if(is.null(IndicatorareaID)) stop("Must provide input of Area indicator IDs either in Indicator.ind.areas dataframe or as IndicatorareaID!")
    if(is.null(Indicator_name)) stop("Must provide input of Area indicator names either in Indicator.ind.areas dataframe or as Indicator_name!")

    areaindName = IndicatorareaID
    areaindIndicator = Indicator_name}


  if(!is.null(Indicator.area)){
    areaName = Indicator.area$IndicatorareaID
    areaMunicipality = Indicator.area$Basicunit
  } else
  {
    if(is.null(Area.Name)) stop("Must provide input of indicator area IDs either in Indicator.areas or as Area.Name")
    if(is.null(Area.Municipality)) stop("Must provide input of names of corresponding basic spatial units either in Indicator.areas or as Area.Municipality")
    areaName = Area.Name
    areaMunicipality = Area.Municipality
    }

  if(!is.null(Observations)){
    fkOmraadeID = Observations$IndicatorareaID
  } else
  {
    if(is.null(FK_OmraadeID)) stop("Must provide input on vector of indicator area IDs corresponding to the \nsequence of indicator observations in scaled.bootmat either in Observations or as FK_OmraadeID")
    fkOmraadeID = FK_OmraadeID
    }

  if(!is.null(Indicator.area.ind) & !is.null(IndicatorareaID)) warning("Area indicator IDs provided both from Indicator.area.ind and in IndicatorareaID Values used from Indicator.area.ind!")
  if(!is.null(Indicator.area.ind) & !is.null(Indicator_name)) warning("Area indicator names provided both from Indicator.area.ind and in Indicator_name Values used from Indicator.area.ind!")

  if(!is.null(Indicator.area) & !is.null(Area.Name)) warning("Indicator area IDs provided both from Indicator.area.ind and in Areaind.Name. Values used from Indicator.area!")
  if(!is.null(Indicator.area) & !is.null(Area.Municipality)) warning("Input of names of corresponding basic spatial units provided both from Indicator.area.ind and in Areaind.Indicator. Values used from Indicator.area!")

  if(!is.null(Observations) & !is.null(FK_OmraadeID)) warning("Indicator area IDs corresponding to the \nsequence of indicator observations in scaled.bootmat \nprovided both from Indicator.area.ind and in Areaind.Indicator. Values used from Indicator.area!")



  N.kom <- dim(Weights.trof)[[1]]
  N.ind <- dim(Weights.trof)[[2]]
  N.omr <- length(areaindIndicator)

  Scaled.ind.per.mun <- array(rep(Weights.trof*0.0,nsim),dim = c(N.kom,N.ind,nsim))
  dimnames(Scaled.ind.per.mun) [[1]] <- dimnames(Weights.trof) [[1]]
  dimnames(Scaled.ind.per.mun) [[2]] <- dimnames(Weights.trof) [[2]]
  dimnames(Scaled.ind.per.mun) [[3]] <- 1:nsim

  for (j in 1:N.omr) {
    for (k in which(areaName == areaindName[j])) {
      Scaled.ind.per.mun[as.character(areaMunicipality[k]),as.character(areaindIndicator[j]),] <-
        scaled.bootmat[fkOmraadeID == areaindName[j],]
    }
  }


  if (length(Weights.reg.area) == 0) {
    NI <- matrix(0.0,nrow=N.kom,ncol=nsim)
    dimnames(NI) [[1]] <- dimnames(Weights.trof) [[1]]
    for (j in 1:nsim) {
      NI[,j] <- rowSums(Scaled.ind.per.mun[,,j] * Weights.trof)
    }
  } else {
    N.reg <- dim(Weights.reg.area)[3]
    Regions <- dimnames(Weights.reg.area) [[3]]
    NI <- matrix(0.0,nrow=N.reg,ncol=nsim)
    if (length(dimnames(Weights.reg.area) [[3]]) > 1) {
      dimnames(NI) [[1]] <- dimnames(Weights.reg.area) [[3]]
    } else {
      dimnames(NI) [[1]] <- list(dimnames(Weights.reg.area) [[3]])
    }
    for (j in 1:nsim) {
      for (k in Regions) {NI[k,j] <- sum(Scaled.ind.per.mun[,,j] * Weights.trof * Weights.reg.area[,,k])}
    }
  }

  class(NI) <- c("NIresult", "matrix")
  return(NI)

}
