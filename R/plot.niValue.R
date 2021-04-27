#' Plot the Distribution of an Index Estimate.
#'
#' Plot method for objects of class \code{\link{niValue}}.
#'
#' Plots the cumulative distribution of an index value.
#'
#' @seealso \code{\link{calculateIndex}}, \code{\link{niValue}}, \code{\link{niSeries}},
#' and \code{\link{niOutput}} for computation and generation of \code{niValue},
#' \code{niSeries}, and \code{niOutput} objects.
#'
#' @importFrom stats quantile
#' @import graphics
#' @import grDevices
#'
#' @name plot.niValue
#' @encoding UTF-8
#' @author Bård Pedersen
#'
#' @param x \code{niValue} object.
#' @param ... further arguments controlling the appearance of the plot,
#'   some passed further to \code{\link{par}}:
#'   \cr\code{linecol}, \code{lwd}, \code{xlab}, \code{ylab}, \code{cex.lab}, \code{cex.axis}, \code{main},
#'      \code{cex.main},
#'   \cr\code{q} - vector of quantiles displayed along first (as \code{quantile(q)}) and second axis (as \code{q})
#'      (defaults to \code{c(0.025,0.500,0.975)}),
#'   \cr\code{gridcol} - grid line colour (defaults to \code{gray(0.6)}) where gridlines are defined by \code{q}),
#'   \cr\code{range01} - whether the x-axis should cover the full index range (\code{c(0,1)},\code{T}) or
#'      not (\code{F}).
#'
#' @examples
#' \dontrun{
#' # Plotting the cumulative distribution function of a thematic index value:
#'
#' plot(calculateIndex(x = themeData,
#'                     nsim = 1000,
#'                     fids = FALSE,
#'                     tgroups = FALSE,
#'                     keys = "ignore",
#'                     w = 0,
#'                     awbs = TRUE,
#'                     awBSunit = "Skog")$E$`1990`,
#'      range01 = F)
#' }
#'
#' @export
#'

plot.niValue <- function(x = NULL, ...) {


  z <- match.call()

  if (!is.niValue(x)) {
    stop(paste("'",z[2],"' is not of class 'niValue'.", sep=""), call. = TRUE)
  }

  setPlotParameters <- function(q = c(0.025,0.500,0.975),
                                gridcol = grDevices::gray(0.6),
                                linecol = "#006964",
                                lwd = 1,
                                xlab = "q",
                                ylab = "p(NI < q)",
                                cex.lab = 1.2,
                                cex.axis = 1,
                                cex.main = 1.75,
                                range01 = TRUE,
                                main = "") {

    return(list(q = q,
                gridcol = gridcol,
                linecol = linecol,
                lwd = lwd,
                xlab = xlab,
                ylab = ylab,
                cex.lab = cex.lab,
                cex.axis = cex.axis,
                cex.main = cex.main,
                range01 = range01,
                main = main))
  }

  param <- setPlotParameters(...)

  q <- param$q
  gridcol <- param$gridcol
  linecol <- param$linecol
  lwd <- param$lwd
  xlab <- param$xlab
  ylab <- param$ylab
  cex.lab <- param$cex.lab
  cex.axis <- param$cex.axis
  cex.main <- param$cex.main
  range01 <- param$range01
  main <- param$main

  xx <- sort(x$index)
  xxlim <- c(0,1)

  if (range01) {
    plot(c(0,xx,1),c(0,1:length(xx)/length(xx),1),type="l",
         main="",xlab=xlab,ylab=ylab,xlim=xxlim,ylim=c(0,1),axes=F,col="white",cex.lab=cex.lab)
    graphics::lines(c(0,0),c(0,1),col=gridcol,lwd=lwd)
    graphics::axis(1, pos=0, at = c(0,stats::quantile(xx,q),1),
                   labels = format(c(0,round(stats::quantile(xx,q)*100)/100,1),digits=2),
                   cex.axis=cex.axis,lwd=lwd,col=gridcol)
    graphics::axis(2, pos=0, at = q, labels = format(q,digits=3),
                   cex.axis=cex.axis,lwd=lwd,las=1,col=gridcol)
    graphics::lines(c(0,xx,1),c(0,1:length(xx)/length(xx),1),lwd=2*lwd,col=linecol)
    for (i in q) {
      graphics::lines(c(stats::quantile(xx,i),stats::quantile(xx,i)),c(0,i),col=gridcol)
      graphics::lines(c(0,stats::quantile(xx,i)),c(i,i),col=gridcol)
    }
  } else {
    plot(xx,1:length(xx)/length(xx),type="l",
         xlab=xlab,ylab=ylab,ylim=c(0,1),axes=F,col="white",cex.lab=cex.lab)
    graphics::lines(c(xx[1],xx[1]),c(0,1),col=gridcol,lwd=lwd)
    graphics::axis(1, pos=0, at = c(xx[1],stats::quantile(xx,q),xx[length(xx)]),
                   labels = c("",format(stats::quantile(xx,q),digits=3),""),
                   cex.axis=cex.axis,lwd=lwd,col=gridcol)
    graphics::axis(2, pos=xx[1], at = q, labels = format(q,digits=3),
                   cex.axis=cex.axis,lwd=lwd,col=gridcol,las=1)
    graphics::lines(xx,1:length(xx)/length(xx),lwd=2*lwd,col=linecol)
    for (i in q) {
      graphics::lines(c(stats::quantile(xx,i),stats::quantile(xx,i)),c(0,i),col=gridcol)
      graphics::lines(c(xx[1],stats::quantile(xx,i)),c(i,i),col=gridcol)
    }
  }
  graphics::title(main=main,cex.main=cex.main)
}
