#' Plot a Time Series of Index Values.
#'
#' Plot method for objects of class \code{\link{niSeries}}.
#'
#' @seealso \code{\link{calculateIndex}}, \code{\link{niSeries}}, and
#' \code{\link{niOutput}} for cumputation
#' and generation of \code{niSeries} and \code{niOutput} objects.
#'
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom graphics lines
#' @importFrom graphics axis
#' @importFrom graphics title
#'
#' @import grDevices
#'
#' @name plot.niSeries
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param x an \code{niSeries} object.
#' @param ... further arguments controlling the appearance of the plot,
#'   some passed further to \code{\link{par}}:
#'   \cr\code{type}, \code{linecol}, \code{lwd}, \code{cex}, \code{cex.axis}, \code{main}, \code{cex.main},
#'   \cr\code{gridlines} - whether to include gridlines (\code{T}, default) or not (\code{F}),
#'   \cr\code{gridcol} - grid line colour (defaults to \code{gray(0.6)}),
#'   \cr\code{showInterval} - weather to include interval estimates (\code{T}, default) in the plot.
#'   \cr\code{limits} - limit quantiles for interval estimate (defaults to \code{c(0.025,0.975)})
#'   \cr\code{shade} - weather to include a shading (\code{T}) to represent interval estimate or
#'      not (\code{F}, default),
#'   \cr\code{polygoncol} color of shade,
#'   \cr\code{whiskerEnds} weather to draw ends of the interval estimate, with (\code{T}, default)
#'      or without (\code{F}) whisker ends.
#'
#' @examples
#' \dontrun{
#' # Plotting the results from the calculation of a thematic index:
#'
#' plot(calculateIndex(x = themeData,
#'                              nsim = 1000,
#'                              fids = FALSE,
#'                              tgroups = FALSE,
#'                              keys = "ignore",
#'                              w = 0,
#'                              awbs = TRUE,
#'                              awBSunit = "Skog")$E,
#'     cex=1.5,
#'     whiskerEnds = TRUE)
#' }
#'
#' @export
#'

plot.niSeries <- function(x = NULL, ...) {

  z <- match.call()

  if (!is.niSeries(x)) {
    stop(paste("'",z[2],"' is not of class 'niSeries'.", sep=""), call. = TRUE)
  }

  setPlotParameters <- function(type = "o",
                                gridcol = grDevices::gray(0.6),
                                polygoncol = grDevices::rgb(red=0, green=0.65, blue=0.65*100/105, alpha = 0.3),
                                linecol = "#006964",
                                lwd = 3,
                                cex = 2.5,
                                cex.axis = 1.5,
                                cex.main = 1.75,
                                showInterval = TRUE,
                                limits = c(0.025,0.975),
                                shade = FALSE,
                                gridlines = TRUE,
                                main = "",
                                whiskerEnds = TRUE) {

    return(list(type = type,
                gridcol = gridcol,
                polygoncol = polygoncol,
                linecol = linecol,
                lwd = lwd,
                cex = cex,
                cex.axis = cex.axis,
                cex.main = cex.main,
                showInterval = showInterval,
                limits = limits,
                shade = shade,
                gridlines = gridlines,
                main = main,
                whiskerEnds = whiskerEnds))
  }

  param <- setPlotParameters(...)

  type <- param$type
  gridcol <- param$gridcol
  polygoncol <- param$polygoncol
  linecol <- param$linecol
  lwd <- param$lwd
  cex <- param$cex
  cex.axis <- param$cex.axis
  cex.main <- param$cex.main
  showInterval <- param$showInterval
  limits <- param$limits
  shade <- param$shade
  gridlines <- param$gridlines
  main <- param$main
  whiskerEnds = param$whiskerEnds

  yearNames <- names(x)
  nYears <- length(yearNames)
  years <- as.integer(yearNames)
  y <- y.low <- y.high <- NULL
  for (j in 1:nYears) {
    y[j] <- stats::median(x[[j]]$index)
    y.low[j] <- stats::quantile(x[[j]]$index,limits[1])
    y.high[j] <- stats::quantile(x[[j]]$index,limits[2])
  }

  xxlim <- c(min(years)-1, max(years)+1)

  if (gridlines) {
    plot(years,y,type="n",main="",xlab="",ylab="",xlim=xxlim,ylim=c(0,1),xaxp=c(1000,3000,1), yaxp=c(-1,3,1),axes=F)
    for(j in 1:length(years)) graphics::lines(rep(years[j],2),c(-0.025,0),col=gridcol,lwd=lwd)
    for(j in seq(0,1,0.25)) graphics::lines(xxlim,c(j,j),col=gridcol,lwd=lwd)
    graphics::axis(1, pos=-0.05, at = years, labels = yearNames, cex.axis=cex.axis,lwd=lwd,col="white")
    graphics::axis(2, pos=xxlim[1], at = seq(0,1,0.25), labels = c("0,00","0,25","0,50","0,75","1,00"),
                   cex.axis=cex.axis,lwd=lwd,las=1,col="white")
  } else {
    plot(years,y,xlab="",ylab="",xlim=xxlim,ylim=c(0,1),xaxp=c(1000,3000,1), yaxp=c(-1,3,1),axes=T)
    graphics::axis(1, at = years, labels = yearNames, cex.axis=cex.axis)#,lwd=lwd)
    graphics::axis(2, at = seq(0,1,0.25), labels = c("0,00","0,25","0,50","0,75","1,00"),
                   cex.axis=cex.axis,las=1)
  }
  if (shade) {graphics::polygon(c(years,rev(years)),c(y.low,rev(y.high)),col=polygoncol,border=NA)}
  graphics::lines(years,y,type=type,pch=21,cex=cex,col=linecol,bg=linecol,lwd=1.5*lwd)
  if (showInterval) {
    for(j in 1:length(years)) graphics::lines(rep(years[j],2),c(y.low[j],y.high[j]),col=linecol,lwd=1.25*lwd)
    if (whiskerEnds) {
      for(jj in 1:length(years)) graphics::lines(c(years[jj]-0.25,years[jj]+0.25),c(y.low[jj],y.low[jj]),col=linecol,lwd=lwd)
      for(jj in 1:length(years)) graphics::lines(c(years[jj]-0.25,years[jj]+0.25),c(y.high[jj],y.high[jj]),col=linecol,lwd=lwd)
    }
  }
  graphics::title(main=main,cex.main=cex.main)

}
