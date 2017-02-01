#' Weighting for areas
#'
#'  Function that calculates relevant area weights (Wk) for chosen spatial resolution of the NI output.
#' I.e. areaweights.fct calculates Wk for a set of aggregated, nonoverlapping, spatial units each
#' consisting of one to many basic spatial units (municipalities).
#'
#' In the Norwegian implementation Wks are weights based on the area the major habitat in question covers
#' in each basic spatial unit. An alternative is to use the total area of the spatial unit, etc.
#' Wk is the relevant municipality area divided by the corresponding total area of the aggregate.
#' Municipalities outside the aggregate receive weights equal zero
#' Area weights are relevant when NI is calculated for aggregates of basic spatial units.
#'
#' The weights are stored in a three-dimensional array to be included in the calculation of the nature index in
#' a subsequent call to NIcalculation.fct.
#' In the array weights are repeated for each indicator included in the current calculation of NI.
#'
#'
#' @param Municipalities	character		length=length(Municipality)	vector of names of basic spatial units
#' @param Regions		character		length=length(Municipality)	vector of names of aggregated spatial units that the corresponding municipality is a part of.
#' @param Area.municipality	double		length=length(Municipality)	vector of relevant basic spatial unit areas
#' @param Indicators		character		length=length(Indicators)	vector of the names of all indicators included in the calculation
#'
#' @return Weights.reg.area	double array	dim=length(Municipality) x length(Indicators) x length(unique(Regions)) array of area weights.
#'
#' @author Bård Pedersen
#'
#' @section Notes:
#'
#'
#' @examples
#' \dontrun{
#' ##Add example, here, possibly as a don't run
#' tt <- areaweights.fct(Municipalities=as.character(1:10),
#' Regions=as.character(rep(1:5,2)),
#' Area.municipality=runif(10),
#' Indicators=c("ind1","ind2"))
#' }
#'
#' @export
#'

areaweights <- function(Municipalities=as.character(1:10),
                            Regions=as.character(rep(1:5,2)),
                            Area.municipality=runif(10),
                            Indicators=c("ind1","ind2")) {



  Municipalities <- as.character(Municipalities)
  Regions <- as.character(Regions)
  Aggregates <- unique(Regions)
  Indicators <- as.character(Indicators)
  N.reg <- length(Aggregates)
  N.kom <- length(Municipalities)
  N.ind <- length(Indicators)

  Weights.reg.area <- array(rep(0.0,N.kom * N.ind * N.reg),dim = c(N.kom,N.ind,N.reg))
  dimnames(Weights.reg.area) [[1]] <- Municipalities
  dimnames(Weights.reg.area) [[2]] <- Indicators
  dimnames(Weights.reg.area) [[3]] <- Aggregates
  for (k in Aggregates) {
    Tot.area.reg <- sum(Area.municipality[Regions == k])
    Weights.reg.area[,,k] <- matrix(rep((Regions == k) * Area.municipality / Tot.area.reg,N.ind),ncol=N.ind)
  }

  return(Weights.reg.area)
}
