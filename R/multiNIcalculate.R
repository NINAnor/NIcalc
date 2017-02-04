#' Calculate NI index for multiple years
#'
#' Wrapper function that calls NIcalculate multiple times to calculate the index for multiple years in batch
#'
#' @param Observations  data frame containing named column IndicatorareaID with data on indicator area IDs
#'   corresponding to the sequence of indicator observations in scaled.bootmat.
#' @param scaled.bootmat	double matrix,	dim = N.omr x nsim,			nsim random draws
#'   of scaled indicator observations or nsim copies of scaled expected values
#'
#' @param ... Further arguments to be passed to \code{\link{NIcalculate}}
#'
#'
#' @return List of class multiNIresult
#'
#'
#' @examples
#'
#' data(Observations)
#' data(Indicator.area.ind)
#' data(Indicator.area)
#'
#' obs <- data.frame(
#'   Observations,
#'   elicitate(
#'     expected.value=Observations$Expected.value,
#'     lower=Observations$Lower,
#'     upper = Observations$Upper)[,1:3])
#'
#' bootmat <- samplebootmat(
#'   ValueID=obs$ValueID,
#'   Value=obs$Expected.value,
#'   RefobsID = obs$ReferenceYearID,
#'   DistID=obs$FK_DistID,
#'   mu=obs$mu,
#'   sig=obs$sig,
#'   nsim=10)
#'
#' scaledBootmat <- scaleobs(
#'   ValueID=obs$ValueID,
#'   FK_OmraadeID=obs$IndicatorareaID,
#'   FK_IndicatorID=obs$IndicatorID,
#'   FK_RefAarID=obs$ReferenceYearID,
#'   nsim=10,
#'   bootmat=bootmat,
#'   ref.value.code=0,
#'   IndicatorID=Indic$IndicatorID,
#'   FK_Scalingmodel=Indic$Scalingmodel)
#'
#'
#' weightsTrof <- munweights(
#'   Municipalities=BSunits$Basicunit,
#'   Indicators=Indic$Indicator_name,
#'   FK_TrophicgroupID=Indic$TrophicgroupID,
#'   Key.indicators=Indic$Key.indicators,
#'   Fidelity=Indic$Fidelity,
#'   Areaind.Name=Indicator.area.ind$IndicatorareaID,
#'   Areaind.Indicator=Indicator.area.ind$Indicator_name,
#'   Area.Name=Indicator.area$IndicatorareaID,
#'   Area.Municipality=Indicator.area$Basicunit)
#'
#'
#'
#'
#' tt<-multiNIcalculate(Indicator.area.ind = Indicator.area.ind,
#'                      Indicator.area = Indicator.area,
#'                      Observations = Observations,
#'                      nsim = 10,
#'                      scaled.bootmat =  scaledBootmat,
#'                      Weights.trof = weightsTrof)
#'
#' summary(tt)
#' @export
#'
multiNIcalculate <- function (Observations, scaled.bootmat, ...){

  out<-list()

  years <- unique(Observations$ReferenceYearID[Observations$ReferenceYearID!=0])

    for (i in years) {
        scaled.bootmatyeari <- scaled.bootmat[which(Observations$ReferenceYearID == i),]
        observationsi <-  Observations[which(Observations$ReferenceYearID == i),]

      NIi <- NIcalculate2(Observations = observationsi
                          , scaled.bootmat = scaled.bootmatyeari
                          , ...)

        out[[paste("year:", i)]] <-  NIi

      }


  class(out) <- c("multiNIresult")
return(out)

}
