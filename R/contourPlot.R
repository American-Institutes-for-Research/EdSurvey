#' @title Overlaid Scatter and Contour Plots
#'
#' @description Diagnostic plots for regressions can become too dense to interpret.
#'              This function helps by adding a contour plot over the points to 
#'              allow the density of points to be seen, even when an area is entirely covered in points.
#'
#' @param x numeric vector of the \code{x} data to be plotted
#' @param y numeric vector of the \code{y} data to be plotted
#' @param m integer value of the number of \code{x} and \code{y} grid points
#' @param xrange numeric vector of length two indicating \code{x}-range of plot;
#'               defaults to range(x)
#' @param yrange numeric vector of length two indicating \code{y}-range of plot;
#'               defaults to range(y)
#' @param xkernel numeric indicating the standard deviation of Normal
#'                \code{x} kernel to use in generating contour plot
#' @param ykernel numeric indicating the standard deviation of Normal
#'                \code{y} kernel to use in generating contour plot
#' @param nlevels integer with the number of levels of the contour plot
#' @param densityColors colors to use, specified as in \ifelse{latex}{\code{par}}{\code{\link[graphics]{par}}}.
#'               Defaults to the \ifelse{latex}{\code{heat.colors}}{\code{\link[grDevices]{heat.colors}}}
#'               with \code{nlevels}. When specified, \code{colors} overrides \code{nlevels}.
#' @param pointColors color for the plot points
#'               
#' @param \dots additional arguments to be passed to a plot call that generates the
#'             scatter plot and the contour plot
#' @author Yuqi Liao and Paul Bailey
#' @example man/examples/contourPlot.R
#' @importFrom stats dnorm sd
#' @importFrom graphics plot contour
#' @importFrom grDevices heat.colors
#' @export
contourPlot <- function(x, y,
                        m=30L,
                        xrange, yrange,
                        xkernel, ykernel,
                        nlevels=9L,
                        densityColors=heat.colors(nlevels),
                        pointColors="gray",
                        ...) {
  z <- matrix(NA, nrow=m, ncol=m)
  xydf <- data.frame(x=as.numeric(x), y=as.numeric(y))
  xydf <- subset(xydf, !is.na(x) & !is.na(y))
  if(nrow(xydf) < 2) {
    stop("There must be at least two x,y pairs that are nonmissing.")
  }
  x <- xydf$x
  y <- xydf$y
  if(missing(xrange)) {
    xrange <- range(x)
    xrange <- xrange + c(-1,1) * max(0.04,2/m) * (xrange[2]-xrange[1])
  }
  if(missing(yrange)) {
    yrange <- range(y)
    yrange <- yrange + c(-1,1) * max(0.04,2/m) * (yrange[2]-yrange[1])
  }
  xi <- seq(xrange[1], xrange[2], len=m+1)
  yi <- seq(yrange[1], yrange[2], len=m+1)
  dx <- xi[2] - xi[1]
  dy <- yi[2] - yi[1]
  if(missing(xkernel)) {
    if(length(unique(x)) < 20) {
      xkernel <- dx
    } else {
      xkernel <- max(c(sd(x)/10, dx*2))
    }
  }
  if(missing(ykernel)) {
    if(length(unique(x)) < 20) {
      ykernel <- dy
    } else {
      ykernel <- max(c(sd(y)/10, dy*2))
    }
  }
  if(dx > xkernel) {
    warning(paste0(sQuote("xkernel"), " set too small. Increase ", sQuote("m"), " argument."))
  }
  if(dy > ykernel) {
    warning(paste0(sQuote("ykernel"), " set too small. Increase ", sQuote("m"), " argument."))
  }
  # xc/yc are the x/y grid centroids
  # z is the contours and is the height of a kernel smoothed density at (xc,yc)
  xc <- xi[-length(xi)] + dx/2
  yc <- yi[-length(yi)] + dy/2
  for(i in 1:nrow(z)) {
    for(j in 1:ncol(z)) {
      sm <- sum(dnorm(x, mean=xc[i], sd=xkernel) * 
                dnorm(y, mean=yc[j], sd=ykernel))
      z[i,j] <- sm
    }
  }
  # normalize z
  z <- z * 1 / sum(z)
  # plot points
  plot(x, y, col=pointColors, ...)
  # add contour plot
  contour(xc,yc,z, col=densityColors, add=TRUE, lwd=2, nlevels=length(densityColors))
}
