#'  Scaling of observations
#'
#'  The function scaleobs scales a set of indicator observations according to chosen scaling model (LOW or MAX) and
#' 	associated reference values.
#'
#'  Each indicator observation should be entered either as nsim random draws from distributions fitted to the
#' 	original observations that were provided as expected values and upper and lower quartiles, or as nsim
#' 	repetitions of the original expected values provided by experts. In the same way, reference values should
#' 	be entered either as nsim random draws from distributions fitted to the original reference values, or as
#' 	nsim repetitions of the original expected values.
#'
#' 	Reference values and indicator observations should be entered together in the same matrix bootmat, together
#' 	with a vector of length equal to the number of indicator observations and reference values with codes
#' 	identifying reference values from indicator observations.
#'
#' 	Further, vectors of the same length containing the observations/reference values area IDs and indicator IDs
#' 	should be entered.
#'
#' 	Scaling models are specific for each indicator. They should be entered as a vector of scaling model codes
#' 	where 1 = LOW and 2 = MAX, together with a vector of associated indicator IDs of the same length.
#'
#' @param	ValueID		integer		length=length(ValueID)		vector of indicator observation IDs
#' @param	FK_OmraadeID	integer		length=length(ValueID)		vector of indicator area IDs
#' @param	FK_IndicatorID	integer		length=length(ValueID)		vector of indicator IDs
#' @param	FK_RefAarID		integer		length=length(ValueID)		vector of IDs for observation years / reference values
#' @param	nsim			integer		length=1				number of draws (simulations) in the estimation of NI
#' @param	bootmat		double matrix	dim=length(ValueID) x nsim	nsim random draws from fitted distributions or nsim copies of expected values
#' @param	ref.value.code	integer		length=1				code for reference values used in FK_RefAarID
#' @param	IndicatorID		integer		length=length(IndicatorID)	vector of indicator IDs
#' @param	FK_Scalingmodel	integer		length=length(IndicatorID)	vector of scaling model IDs, 1 = LOW and 2 = MAX
#'
#' @return	scaled.bootmat	double matrix	dim=length(ValueID) x nsim	nsim scaled draws from fitted distributions
#' or copies of expected values
#'
#' @section Notes:
#'
#' Note that the scaled.bootmat that is returned from this function is of the same dimension as bootmat.
#' This means that scaled.bootmat contains nsim draws of scaled reference values which all are equal to 1.
#' These must be removed from scaled.bootmat before using NIcalculation.fct to calculate random draws of the nature index.
#'
#' Further, if the input bootmat matrix contains indicator observations for several years, so will the
#' scaled.bootmat. However, the index is calculated for one year, so the output matrix scaled.bootmat must be
#' split accordingly into a set of matrices, where each matrix contains draws of scaled indicator observations
#' for one single year only.
#'
#' @examples
#' \dontrun{
#' ##Add example, here, possibly as a don't run
#' }
#'
#' @export
#'
#' @author programmed by Bard Pedersen



scaleobs<-function(ValueID=1:2,FK_OmraadeID=rep(1,2),FK_IndicatorID=rep(1,2),FK_RefAarID=c(0,1),nsim=100,
                       bootmat=matrix(1,nrow = 2,ncol = nsim),ref.value.code=0,IndicatorID=1,FK_Scalingmodel=1){


  Reference.values <- FK_RefAarID == ref.value.code
  refmat <- 0*bootmat
  valuemat <- bootmat
  N.values <- length(ValueID)

  for (i in 1:N.values) {
    refmat[i,] <- bootmat[Reference.values & FK_OmraadeID == FK_OmraadeID[i],]
  }

  scaled.bootmat <- valuemat/refmat
  dimnames(scaled.bootmat)[[1]] <- ValueID

  model.mat <- matrix(1,nrow=N.values,ncol=nsim)
  FK_Scalingmodel <- as.character(FK_Scalingmodel)
  for (i in 1:N.values) {
    model.mat[i,] <- rep(FK_Scalingmodel[IndicatorID == FK_IndicatorID[i]],nsim)
  }
  scaled.bootmat[model.mat == 2] <- (-1)*scaled.bootmat[model.mat == 2] + 2
  scaled.bootmat[scaled.bootmat < 0] <- 0.0
  scaled.bootmat[scaled.bootmat > 1] <- 1.0
  if (!(is.matrix(scaled.bootmat))) {scaled.bootmat <- t(matrix(scaled.bootmat))}

  return(scaled.bootmat)
}

