#' Barplot showing Weights per Indicator, trophic Group, BSunit or ICunit.
#'
#' Plot method for objects of class \code{\link{niValue}} which creates a horizontal barplot.
#'
#' @seealso \code{\link{calculateIndex}}, \code{\link{niValue}},
#' \code{\link{niSeries}}, and \code{\link{niOutput}} for cumputation
#' and generation of \code{niValue}, \code{niSeries}, and \code{niOutput} objects.
#'
#' @importFrom graphics barplot
#' @import grDevices
#'
#' @name plotWeights
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param x an \code{niValue} object.
#' @param group character. Should summary be
#'   \cr per indicator (\code{group = "indic"}, default),
#'   \cr per trophic group (\code{group = "troph"}),
#'   \cr per BSunit (\code{group = "BSunit"}), or
#'   \cr per ICunit (\code{group = "ICunit"})?
#' @param allBars logical. Whether to plot bars for all indicators or only a selection (default, specified via argumen "barnum")
#' @param nrBars integer. Number of indicators for which to display bars when allBars = FALSE. Default = 35.
#' @param ... further arguments controlling the appearance of the plot,
#'   some passed further to \code{\link{par}}:
#'   \cr\code{lwd}, \code{cex}, \code{cex.axis}, \code{main}, \code{cex.main},
#'   \cr\code{barnum} - maximum number of bars (default = 35),
#'   \cr\code{barcol} - color for the bars (defaults to \code{gray(0.75)}),
#'   \cr\code{keys} - whether key indicators should be shown in a separate color (\code{T}, default),
#'   \cr\code{keycol} color for key indicator bars.
#'
#' @examples
#' \dontrun{
#'
#' plotWeights(calculateIndex(x = themeData,
#'                            nsim = 1000,
#'                            fids = FALSE,
#'                            tgroups = FALSE,
#'                            keys = "ignore",
#'                            w = 0,
#'                            awbs = TRUE,
#'                            awBSunit = "Skog")$E$`1990`,
#'              group = "ICunit",
#'              barcol = 3)
#' }
#'
#' @export
#'

plotWeights <- function(x = NULL, group = "indic", allBars = FALSE, nrBars = 35, ...) {

  z <- match.call()

  if (!is.niValue(x)) {
    stop(paste("'",z[2],"' is not of class 'niValue'.", sep=""), call. = TRUE)
  }

  validPlots <- c("indic","troph","BSunit","ICunit")

  if (!(group %in% validPlots)) {
    stop(paste("'",group,"' is not a valid plot type.\nValid types are '",
               paste(validPlots,collapse = "', '"),"'.", sep=""), call. = TRUE)
  }

  if(allBars){
    nrBars <- length(x$indicators)
  }

  setPlotParameters <- function(barnum = nrBars,
                                keys = TRUE,
                                barcol = grDevices::gray(0.75),
                                keycol = grDevices::rgb(red=0, green=0.65, blue=0.65*100/105, alpha = 0.3),
                                lwd = 1.5,
                                cex = 0.5,
                                cex.axis = par("cex.axis"),
                                cex.main = 1.75,
                                main = "") {

    return(list(barnum = barnum,
                keys = keys,
                barcol = barcol,
                keycol = keycol,
                lwd = lwd,
                cex = cex,
                cex.axis = cex.axis,
                cex.main = cex.main,
                main = main))
  }

  param <- setPlotParameters(...)

  barnum <- param$barnum
  keys <- param$keys
  barcol <- param$barcol
  keycol <- param$keycol
  lwd <- param$lwd
  cex <- param$cex
  cex.axis <- param$cex.axis
  cex.main <- param$cex.main
  main <- param$main

  if (group == "BSunit") {
    y <- x$NIunitWeights
  }

  if (group %in% c("indic","troph")) {
    indics <- x$indicators
    indics <- sort(indics)
    nindics <- length(indics)
    y <- rep(NA,nindics)
    names(y) <- indics
    if (is.array(x$indexWeights)) {
      yy <- colSums(x$indexWeights,na.rm = TRUE)
    } else {
      yy <- x$indexWeights
    }

    y[names(yy)] <- yy

    if (group == "troph") {
      indicatorData <- x$indicatorData

      matchNames <- c("id", "name","keyElement","functionalGroup","functionalGroupId","scalingModel","scalingModelId")
      dataFound <- which(match(names(indicatorData), table = matchNames, nomatch = 0) == 0)
      matchNames <- c("name","keyElement","functionalGroup")
      dataFound2 <- which(match(names(indicatorData), table = matchNames, nomatch = 0) != 0)
      indicatorData <- indicatorData[c(dataFound2,dataFound)]

      y <- cbind(indicatorData[match(names(y), table = indicatorData$name, nomatch = 0),],y)
      dimnames(y)[[1]] <- y$name

      y <- y[,dimnames(y)[[2]][-which(dimnames(y)[[2]] == "name")]]

      newGroup <- ifelse(y$keyElement,"Key indicator",y$functionalGroup)
      ytg <- rep(NA,length(unique(newGroup)))
      names(ytg) <- unique(newGroup)
      ytg <- tapply(y[,"y"],as.factor(newGroup),sum,na.rm=TRUE)[names(ytg)]
      y <- ytg
    }
  }

  if (group == "ICunit") {
    k <- 0
    y <- ind <- NULL
    for (i in x$ICunits) {
      k <- k + 1
      y[k] <- sum(x$indexWeights[x$ICunitMatrix == i],na.rm = TRUE)
      ind[k] <- names(which(colSums(x$ICunitMatrix == i,na.rm = TRUE) > 0))
    }
    names(y) <- paste(ind,x$ICunits)
  }

  if (main == "") {
    mainName <- paste(x$indexArea,x$year)
  } else {
    mainName <- main
  }


  ytg <- y
  if (group == "ICunit") {
    ind <- rev(ind[sort.list(as.vector(ytg))])
  }
  ytg <- rev(ytg[sort.list(as.vector(ytg))])

  barnum <- min(barnum,length(ytg))
  substringEnd <- 15
  cexn <- cex
  if (barnum <= 20) {
    #substringEnd <- 9
    cexn <- cex*0.75/0.5
  }

  xlim2 <- (min(which(seq(0,1,0.0001) > max(ytg,na.rm = TRUE))) - 1)*0.0001

  ytg <- rev(ytg[1:barnum])
  if (group == "ICunit") {
    ind <- rev(ind[1:barnum])
  }

  farger <- rep(barcol,barnum)
  if (group == "troph") {
    if (keys) {farger[names(ytg)=="Key indicator"] <- keycol}
  }
  if (group == "ICunit") {
    if (keys) {
      yy <- x$indicatorData
      newGroup <- ifelse(yy$keyElement,"Key indicator",yy$functionalGroup)
      names(newGroup) <- yy$name
      newGroup <- newGroup[ind]
      farger[newGroup=="Key indicator"] <- keycol
    }
  }

  if (group == "indic") {
    if (keys) {
      yy <- x$indicatorData
      newGroup <- ifelse(yy$keyElement,"Key indicator",yy$functionalGroup)
      names(newGroup) <- yy$name
      newGroup <- newGroup[names(ytg)][1:barnum]
      farger[newGroup=="Key indicator"] <- keycol
    }
  }

  graphics::barplot(height = ytg,
          width = 1,
          space = NULL,
          names.arg = substring(names(ytg),1,substringEnd),
          legend.text = NULL,
          beside = FALSE,
          horiz = TRUE,
          col = farger, border = par("fg"),
          main = mainName, sub = NULL,
          xlab = "Weight", ylab = NULL,
          xlim = c(0,xlim2), ylim = NULL, xpd = TRUE, log = "",
          axes = TRUE, axisnames = TRUE,
          cex.axis = cex.axis,
          cex.names = cexn,
          cex.main = cex.main,
          args.legend = NULL,
          lwd = lwd,
          las = 2)

}
