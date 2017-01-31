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
#' @name NIcalculate
#' @author Bård Pedersen
#'
#' @param Areaind.Name	integer,	length = \code{N.omr},			vector of unique
#'   indicator area IDs
#' @param Areaind.Indicator character,		length = \code{N.omr},vector of
#'   indicator names corresponding to the entries in \code{Areaind.Name}
#' @param Area.Name		integer,		length=length(Area.Name)	vector of indicator area
#'   IDs, where each indicator area ID is repeated for each basic spatial unit
#'   it consist of
#' @param Area.Municipality character,		length=length(Area.Name)	vector of names
#'   of corresponding basic spatial units
#' @param FK_OmraadeID	integer,		length = N.omr			vector of indicator area IDs
#'   corresponding to the sequence of indicator observations in scaled.bootmat
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
#'
#' @section Notes: Area weights should not be entered to \code{NIcalculate} if
#'   NI is calculated for basic spatial units.
#'
#' @seealso \code{\link{NIestimate}}
#'   NIcalculation()


NIcalculate <- function(
  Areaind.Name=c(1,2),
  Areaind.Indicator=rep("Ind1",2),
  Area.Name=rep(1:2,each=5),
  Area.Municipality=c("Mun1","Mun2","Mun3","Mun4","Mun5","Mun6","Mun7","Mun8","Mun9","Mun10"),
  FK_OmraadeID=c(1,2),
  nsim=100,
  scaled.bootmat=matrix(1,nrow = 2,ncol = nsim,dimnames = list(c(1,2))),
  Weights.trof=matrix(1,nrow=10,ncol=1,dimnames = list(c("Mun1","Mun2","Mun3","Mun4","Mun5","Mun6","Mun7","Mun8","Mun9","Mun10"),c("Ind1"))),
  Weights.reg.area=NULL) {

  N.kom <- dim(Weights.trof)[[1]]
  N.ind <- dim(Weights.trof)[[2]]
  N.omr <- length(Areaind.Indicator)

  Scaled.ind.per.mun <- array(rep(Weights.trof*0.0,nsim),dim = c(N.kom,N.ind,nsim))
  dimnames(Scaled.ind.per.mun) [[1]] <- dimnames(Weights.trof) [[1]]
  dimnames(Scaled.ind.per.mun) [[2]] <- dimnames(Weights.trof) [[2]]
  dimnames(Scaled.ind.per.mun) [[3]] <- 1:nsim

  for (j in 1:N.omr) {
    for (k in which(Area.Name == Areaind.Name[j])) {
      Scaled.ind.per.mun[as.character(Area.Municipality[k]),as.character(Areaind.Indicator[j]),] <-
        scaled.bootmat[FK_OmraadeID == Areaind.Name[j],]
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

  return(NI)
}
