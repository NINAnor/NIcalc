
NIcalculation.fct <- function(
  Areaind.Name=c(1,2),
  Areaind.Indicator=rep("Ind1",2),
  Area.Name=rep(1:2,each=5),
  Area.Municipality=c("Mun1","Mun2","Mun3","Mun4","Mun5","Mun6","Mun7","Mun8","Mun9","Mun10"),
  FK_OmraadeID=c(1,2),
  nsim=100,
  scaled.bootmat=matrix(1,nrow = 2,ncol = nsim,dimnames = list(c(1,2))),
  Weights.trof=matrix(1,nrow=10,ncol=1,dimnames = list(c("Mun1","Mun2","Mun3","Mun4","Mun5","Mun6","Mun7","Mun8","Mun9","Mun10"),c("Ind1"))),
  Weights.reg.area=NULL) {

  #	Function for calculating nsim draws of NI from nsim sets of (N.omr) scaled observations for (N.ind) indicators and
  #	corresponding areaweights, trophic weights and fidelity weights for a set of (N.kom) basic spatial units
  #	or a set of (N.reg) aggregated NI-areas each consisting of one or several basic spatial units.
  #	Scaled indicatorobservations are calculated in scaleobs.fct, while area weights are calculated in areaweights.fct,
  #	and the product of trophic weights and fidelity weights in munweights.fct.
  #
  #	NI is calculated for a set of NI-areas, while scaled indicator observations, on the other hand, refer to indicator
  #	specific areas that may vary in geographical extent among indicators, and will in general not correspond to the NI-areas.
  #	Thus, NIcalculation.fct needs input about the spatial extent of each indicator area and NI-area.
  #
  #	Area weights should not be entered to NIcalculation.fct if NI is calculated for basic spatial units.
  #
  #	NIcalculation returns a matrix of nsim draws of NI for each NI-area (or basic spatial unit).
  #
  #	Input arguments:
  #	Areaind.Name	integer		length=N.omr			vector of unique indicator area IDs
  #	Areaind.Indicator character		length=N.omr			vector of indicator names corresponding to
  #												the entries in Areaind.Name
  #	Area.Name		integer		length=length(Area.Name)	vector of indicator area IDs, where each indicator
  #												area ID is repeated for each basic spatial unit it consist of
  #	Area.Municipality character		length=length(Area.Name)	vector of names of corresponding basic spatial units
  #	FK_OmraadeID	integer		length=N.omr			vector of indicator area IDs corresponding to the
  #												sequence of indicator observations in scaled.bootmat
  #	nsim			integer		length=1				number of draws (simulations) in the estimation of NI
  #	scaled.bootmat	double matrix	dim=N.omr x nsim			nsim random draws of scaled indicator observations or
  #												nsim copies of scaled expected values
  #	Weights.trof	double matrix	dim=N.kom x N.ind			trophic weights * fidelity weights
  #	Weights.reg.area	double array	dim=N.kom x N.ind x N.reg	array of area weights
  #
  #	Output:
  #	NI			double matrix	dim=(N.kom or N.reg) x nsim 	matrix of NI draws
  #
  #	Function programmed by Bård Pedersen
  #

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
