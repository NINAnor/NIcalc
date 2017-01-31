#' Weighting for municipalities
#'
#' Calculates fidelity weights (Wi), trophic weights (Wf) and their product for each combination
#' of municipality and indicator.
#'
#' Wi is the fidelity of each indicator to each ecosystem, divided by the summed fidelity
#' of indicators belonging to the same trophic group occuring in a municipality.
#' Wf for non-key indicators equals 0.5/number of trophic groups observed in the municipality if also
#' key indicators are observed in the municipality, and 1/number of trophic groups observed in the municipality
#' if no key elements are observed in the municipality. Wf for key indicators equals 0.5 if also
#' non-key indicators are observed in the municipality, and 1 if only key indicators are observed.
#' Weights.trof : Matrix Double (N.kom x N.ind) Wi * Wf
#'
#' @param Municipalities	character vector of length=length(Municipality) with names of basic spatial units.
#' @param Indicators		character	vector of	length=length(Indicators)	with the names of all indicators included in the calculation.
#' @param FK_TrophicgroupID	integer	vector of	length=length(Indicators) with trophic group IDs that the corresponding indicator belongs to.
#' @param Key.indicators	logical	vector of	length=length(Indicators), indicating whether the corresponding indicator is a key indicator or not.
#' @param Fidelity		numeric vector of length=length(Indicators)	with fidelities of each indicator to the major habitat in question.
#' @param Areaind.Name	integer	vector of	length=length(Areaind.Name)	with unique indicator area IDs.
#' @param Areaind.Indicator character	vector of length=length(Areaind.Name)	with indicator names corresponding to the entries in Areaind.Name.
#' @param Area.Name		integer	vector of length=length(Area.Name)	with indicator area IDs, where each indicator area ID is
#' repeated for each spatial unit it consist of.
#' @param Area.Municipality character		length=length(Area.Name)	vector of names of basic spatial units corresponding to the entries in Area.Name.
#'
#' @return Weights.trof numeric matrix of	dim=length(Municipality) x length(Indicators) Wi * Wf.
#'
#' @author Bård Pedersen
#'
#' @section Notes:
#'
#'
#' @examples
#' \dontrun{
#' ##Add example, here, possibly as a don't run
#' tt <- munweights.fct(
#'   Municipalities=paste("Mun", 1:36, sep=""),
#'   Indicators=c("Ind1","Ind2","Ind3","Ind4","Ind5","Ind6"),
#'   FK_TrophicgroupID=c(1,1,2,2,0,0),
#'   Key.indicators=c(F,F,F,F,T,T),
#'   Fidelity=rep(1,6),
#'   Areaind.Name=1:36,
#'   Areaind.Indicator=rep(c("Ind1","Ind2","Ind3","Ind4","Ind5","Ind6"),each=6),
#'   Area.Name=rep(1:36,each=5),
#' Area.Municipality=paste("Mun", 1:36, sep="")[c(sample(1:36,30), sample(1:36,30), sample(1:36,30),
#'                                                 sample(1:36,30), sample(1:36,30), sample(1:36,30))])
#' }
#'
#' @export
#'

munweights.fct <- function(
  Municipalities=paste("Mun", 1:36, sep=""),
  Indicators=c("Ind1","Ind2","Ind3","Ind4","Ind5","Ind6"),
  FK_TrophicgroupID=c(1,1,2,2,0,0),
  Key.indicators=c(F,F,F,F,T,T),
  Fidelity=rep(1,6),
  Areaind.Name=1:36,
  Areaind.Indicator=rep(c("Ind1","Ind2","Ind3","Ind4","Ind5","Ind6"),each=6),
  Area.Name=rep(1:36,each=5),
  Area.Municipality=paste("Mun", 1:36, sep="")[c(sample(1:36,30), sample(1:36,30),
                                                 sample(1:36,30), sample(1:36,30),
                                                 sample(1:36,30), sample(1:36,30))]) {


  Municipalities <- as.character(Municipalities)
  Indicators <- as.character(Indicators)
  N.omr <- length(Areaind.Indicator)
  N.kom <- length(Municipalities)
  N.ind <- length(Indicators)
  trof.mis.code <- max(FK_TrophicgroupID)+1

  # Calculate presence-matrix - meaning: Identify all combinations of indicator and municipality included in the
  # data and calculation. Will be used in the calculation of fidelity weights as they depend on the fidelity
  # of other indicators present in a municipality.
  # Presence : Matrix Logical (N.kom.akt.aar x N.ind.akt.aar)


  Presence <- matrix(FALSE,nrow=N.kom,ncol=N.ind,dimnames = list(Municipalities, Indicators))
  for (j in 1:N.omr) {
    for (k in which(Area.Name == Areaind.Name[j])) {
      Presence[as.character(Area.Municipality[k]),as.character(Areaind.Indicator[j])] <- TRUE
    }
  }

  # Calculate Wi for each combination of municipality and indicator
  #
  # Fidelity.mat : Matrix Double (N.kom x N.ind)
  #	Fidelity to ecosystem in question for all indicators present in the different municipalities.
  #	rowSums(Fidelity.mat) is the total fidelity in each municipality
  #	rowSums(Fidelity.mat*Func.mat.j) is the total fidelity for trophic group j in each municipality where
  #		Func.mat.j is a 1/0 vector signaling whether trophic group j is present in the various municipalities.
  # Weights.fidel : Matrix Double (N.kom x N.ind)
  #	Wi for non-key indicators.
  #	Wi for non-key indicators are zero for key indicators and in municipalities where the indicator is not observed.
  #	rowSums(Weights.fidel) is the number of trophic groups observed in each municipality.
  # Weights.fidel.key : Matrix Double (N.kom x N.ind)
  #	Wi for key indicators.
  #	Wi for key indicators are zero for non-key indicators and in municipalities where the indicator is not observed.
  #	rowSums(Weights.fidel.key) is a 1/0 vector signaling whether any key indicators are observed in the different municipalities.
  # Func.group : Vector Integer (N.ind)
  #	Recoding of Indikatorer$FK_FunksjonellGruppeID where trof.mis.code codes for key indicator.

  Fidelity.mat <- t(matrix(rep(Fidelity,N.kom),ncol=N.kom))*Presence
  dimnames(Fidelity.mat) <- dimnames(Presence)
  Weights.fidel <- Fidelity.mat*0.0
  Weights.fidel.key <- Weights.fidel
  Func.group <- FK_TrophicgroupID
  Func.group[Key.indicators] <- trof.mis.code
  Func.groups.present <- unique(Func.group[Func.group != trof.mis.code])
  N.Func.groups.present <- length(Func.groups.present)

  for (j in Func.groups.present) {
    Weights.fidel[,Func.group == j] <- Fidelity.mat[,Func.group == j] /
      rowSums(Fidelity.mat * t(matrix(rep(Func.group == j,N.kom),ncol=N.kom)))
  }

  Weights.fidel[is.na(Weights.fidel)] <- 0.0

  Weights.fidel.key[,Func.group == trof.mis.code] <- Fidelity.mat[,Func.group == trof.mis.code] /
    rowSums(Fidelity.mat * t(matrix(rep(Func.group == trof.mis.code,N.kom),ncol=N.kom)))
  Weights.fidel.key[is.na(Weights.fidel.key)] <- 0.0

  # For each indicator and municipality, calculate weights determined by trophic group, key/nonkey indicator and fidelity.
  # Wf for non-key indicators equals 0.5/number of trophic groups observed in the municipality if also
  # key indicators are observed in the municipality, and 1/number of trophic groups observed in the municipality
  # if no key elements are observed in the municipality. Wf for key indicators equals 0.5 if also
  # non-key indicators are observed in the municipality, and 1 if only key indicators are observed.
  #
  # N.troph.groups : Matrix Double (N.kom x N.ind) NB!!Identical N.ind columns
  #	Number of trophic groups observed in each municipality.
  # Weights.trof.group : Matrix Double (N.kom x N.ind) NB!!Identical N.ind columns
  #	Wf for non-key indicators
  # Where.there.are.keys : Matrix Integer (N.kom x N.ind) NB!!Identical N.ind columns
  #	1/0 matrix signaling whether any key indicators are observed in the different municipalities.
  # Weights.key : Matrix Double (N.kom x N.ind) NB!!Identical N.ind columns
  # 	Wf for key indicators
  # Weights.trof : Matrix Double (N.kom x N.ind) Wi * Wf

  N.troph.groups <- matrix(rep(rowSums(Weights.fidel),N.ind),ncol=N.ind)
  Where.there.are.keys <- matrix(rep(rowSums(Weights.fidel.key),N.ind),ncol=N.ind)

  Weights.trof.group <- (1-(0.5*Where.there.are.keys))/N.troph.groups
  Weights.trof.group[Weights.trof.group==Inf] <- 0.0

  Weights.key <- (1-(0.5*((N.troph.groups > 0)*1)))* Where.there.are.keys
  Weights.key[is.na(Weights.key)] <- 0.0

  Weights.trof <- ((Weights.trof.group*Weights.fidel) + (Weights.key*Weights.fidel.key))

  return(Weights.trof)
}
