
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
#' 
#' @examples
#' histplot(0:10, 1:10)
#' 
#' # Using hist
#' h <- hist(rnorm(1000), 50, plot=FALSE)
#' histplot(h$breaks, h$counts, col="steelblue", border="white", sep=TRUE)
#' @export
histplot <- function(borders, h, add=FALSE, col="black", zero=0, 
        force_zero=TRUE, border=NA, sep=FALSE, ...) {
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



#' Plot two dimensional histogram
#' 
#' @param hist matrix or two dimensional array containing the number of counts 
#'     in each of the bins.
#' @param borders_x the x-borders of the bins in the histogram. Should be a
#'     numeric vector with lenght one longer than the number of columns of
#'     \code{hist}
#' @param borders_y the y-borders of the bins in the histogram. Should be a
#'     numeric vector with lenght one longer than the number of rows of
#'     \code{hist}
#' @param type a character specifying the type of plot. Valid values are "text",
#'     "area" and "color". See details for more information.
#' @param add add the plot to an existing one or create a new plot.
#' @param add_lines logical specifying whether or not lines should be drawn
#'     between the bins.
#' @param draw_empty if \code{FALSE} empty bins (numer of counts equal to zero)
#'     are not drawn. They are shown using the background color. 
#' @param col for types "area" and "text" the color of the boxes and text.
#' @param line_col the color of the lines between the bins.
#' @param background_col the background color of the bins.
#' @param lty the line type of the lines between the bins.
#' @param text_cex the text size used for type "text". See \code{\link{par}} for
#'     more information.
#' @param col_range the color scale used for type "color". Should be a function
#'     which accepts as first argument the number of colors that should be
#'     generated. The first color generated is used for the zero counts; the 
#'     last color for the highest number of counts in the histogram. 
#' @param ... additional arguments are passed on to \code{\link{plot}}.
#'
#' @details
#' There are three plot types: "area", "text", and "color". In case of "area"
#' rectangles are drawn inside the bins with area proportional to the number of
#' counts in the bins. In case of text the number of counts is shown as text in
#' the bins. In case of color a color scale is used (by default heat.colors) to
#' show the number of counts. 
#'
#' @seealso \code{\link{image}} which can be used to create plots similar to
#' type "color". \code{\link{contour}} may also be of interest. 
#' 
#' @examples
#' histplot2(volcano - min(volcano), type="color")
#' histplot2(volcano - min(volcano), add_lines=FALSE, type="area")
#' histplot2(volcano - min(volcano), type="text", text_cex=0.5)
#' 
#' @export
histplot2 <- function(hist, borders_x=seq(0, ncol(hist)), 
        borders_y=seq(0, nrow(hist)), type="area", add=FALSE, add_lines=TRUE, 
        draw_empty=FALSE, col="black", line_col="#00000030", 
        background_col="white", lty=1, text_cex=0.6, col_range=heat.colors, ...) {
    # create new plot
    rangex <- c(min(borders_x), max(borders_x))
    rangey <- c(min(borders_y), max(borders_y))  
    if (add == FALSE) {
        plot(rangex, rangey, type='n', xaxs='i', yaxs='i', ...)
        rect(rangex[1], rangey[1], rangex[2], rangey[2], col=background_col, 
            border=NA)
    }
    # prepare data
    nx <- length(borders_x)-1
    ny <- length(borders_y)-1 
    wx <- rep(diff(borders_x), each=ny)
    wy <- rep(diff(borders_y), times=nx)
    sx <- 0.95*min(wx)/sqrt(max(hist))
    sy <- 0.95*min(wy)/sqrt(max(hist))
    x  <- rep((borders_x[-length(borders_x)] + borders_x[-1])/2, each=ny)
    y  <- rep((borders_y[-length(borders_y)] + borders_y[-1])/2, times=nx)
    h  <- as.numeric(hist)
    # plot type "area" 
    if (type == "area") {
        dx <- sqrt(h)*sx*0.5
        dy <- sqrt(h)*sy*0.5
        rect(x-dx, y-dy, x+dx, y+dy, col=col, border=NA)
    # plot type "text" 
    } else if (type == "text") {
        if (draw_empty) {
            text(x, y, format(h), cex=text_cex, col=col)
        } else {
            text(x[h!=0], y[h!=0], format(h[h!=0]), cex=text_cex, col=col)
        }  
    # plot type "color" 
    } else if (type == "color" | type == "colour") {
        h   <- h/(wx*wy)
        col <- col_range(200)
        col <- col[floor(h/max(h)*200*(1-.Machine$double.eps))+1]
        sel <- rep(TRUE, length(x))
        if (!draw_empty) sel <- h > 0
        rect(x[sel]-wx[sel]/2, y[sel]-wy[sel]/2, x[sel]+wx[sel]/2,
            y[sel]+wy[sel]/2, col=col[sel], border=NA)
    } else {
        stop("Unknown plot type: options are 'area', 'text' and 'color'.")
    }
    # add_lines
    if (add_lines) {
        lines(rbind(borders_x, borders_x, NA), 
            rbind(rep(rangey[1], nx+1), rep(rangey[2], nx+1), NA), 
            col=line_col, lty=lty)
        lines(rbind(rep(rangex[1], ny+1), rep(rangex[2], ny+1), NA), 
            rbind(borders_y, borders_y, NA), col=line_col, lty=lty)
    }
    # add border
    if (add == FALSE) box()
}

