
#' Plot histogram
#'
#' @param borders the borders of the bins of the histogram. Should be a numeric
#'     vector with length one longer than the number of bins.
#' @param h the values of the bins of the histogram (usually the number of
#'     objects that fall inside the bin. 
#' @param add add the plot to an existing one or create a new plot.
#' @param col the color of the bars
#' @param zero the base line of the histogram
#' @param force_zero ensure that the histogram starts a zero, e.g. that the zero
#'     is includes in the plot
#' @param border color of the border of the bars
#' @param sep whether or not the borders should be drawn between the bars
#' @param ... additional arguments are passed on to \code{\link{polygon}}.
#'
#' @seealso \code{\link{hist}}
#' @examples
#' histplot(0:10, 1:10)
#' 
#' # Using hist
#' h <- hist(rnorm(1000), 50, plot=FALSE)
#' histplot(h$breaks, h$counts, col="steelblue", border="white", sep=TRUE)
#' @export
histplot <- function(borders, h, add=FALSE, col="black", zero=0, force_zero=TRUE, border=NA, sep=FALSE, ...) {
  x <- rep(borders, each=2)
  y <- c(zero, rep(h, each=2), zero)
  if (add == FALSE) {
    if (force_zero) {
      plot(x, y, type='n', ylim=c(zero, max(y)*1.04), yaxs='i')
    } else {
      plot(x, y, type='n')
    }
  }
  if (sep) {
    polygon(rbind(
	 borders[1:(length(borders)-1)], 
	 borders[1:(length(borders)-1)],
	 borders[2:(length(borders))],
	 borders[2:(length(borders))],
	 borders[1:(length(borders)-1)], 
	 rep(NA, length(borders)-1)),
       rbind(
	 rep(0, length(borders)-1),
	 h,
	 h,
	 rep(0, length(borders)-1),
	 rep(0, length(borders)-1),
	 rep(NA, length(borders)-1)),
       col=col, border=border, ...)
  } else {
    polygon(x, y, col=col, border=border, ...)
  }
  abline(zero, 0)
}



#
histplot2 <- function(hist, borders.x=seq(0, ncol(hist)), borders.y=seq(0, nrow(hist)), 
    type="area", add=FALSE, add.lines=TRUE, draw.empty=FALSE, col="black", 
    line.col="#00000030", background.col="white", lty=1, text.cex=0.6, col.range=heat.colors, ...) {
  # create new plot
  rangex <- c(min(borders.x), max(borders.x))
  rangey <- c(min(borders.y), max(borders.y))  
  if (add == FALSE) {
    plot(rangex, rangey, type='n', xaxs='i', yaxs='i', ...)
    rect(rangex[1], rangey[1], rangex[2], rangey[2], col=background.col, border=NA)
  }
  # prepare data
  nx <- length(borders.x)-1
  ny <- length(borders.y)-1 
  wx <- rep(diff(borders.x), each=ny)
  wy <- rep(diff(borders.y), times=nx)
  sx <- 0.95*min(wx)/sqrt(max(hist))
  sy <- 0.95*min(wy)/sqrt(max(hist))
  x  <- rep((borders.x[-length(borders.x)] + borders.x[-1])/2, each=ny)
  y  <- rep((borders.y[-length(borders.y)] + borders.y[-1])/2, times=nx)
  h  <- as.numeric(hist)
  # plot type "area" 
  if (type == "area") {
    dx <- sqrt(h)*sx*0.5
    dy <- sqrt(h)*sy*0.5
    rect(x-dx, y-dy, x+dx, y+dy, col=col, border=NA)
  # plot type "text" 
  } else if (type == "text") {
    if (draw.empty) {
      text(x, y, format(h), cex=text.cex, col=col)
    } else {
      text(x[h!=0], y[h!=0], format(h[h!=0]), cex=text.cex, col=col)
    }  
  # plot type "color" 
  } else if (type == "color") {
    h   <- h/(wx*wy)
    col <- col.range(200)
    col <- col[floor(h/max(h)*200*(1-.Machine$double.eps))+1]
    sel <- rep(TRUE, length(x))
    if (!draw.empty)
      sel <- h > 0
    rect(x[sel]-wx[sel]/2, y[sel]-wy[sel]/2, x[sel]+wx[sel]/2, y[sel]+wy[sel]/2, 
        col=col[sel], border=NA)
  } else {
    stop("Unknown plot type: options are 'area', 'text' and 'color'.")
  }
  # add lines
  if (add.lines) {
    lines(rbind(borders.x, borders.x, NA), 
        rbind(rep(rangey[1], nx+1), rep(rangey[2], nx+1), NA), col=line.col, lty=lty)
    lines(rbind(rep(rangex[1], ny+1), rep(rangex[2], ny+1), NA), 
        rbind(borders.y, borders.y, NA), col=line.col, lty=lty)
  }
  # add border
  if (add == FALSE) {
    box()
  }
}


