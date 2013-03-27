
#' Draw lines for groups
#' 
#' @param x x-coordinates of points to join with line
#' @param y y-coordinates of points to join with line
#' @param by vector (converted to factor) defining the groups for which the 
#'     lines should be drawn.
#' @param col line colors these are recycled over the levels of \code{by}
#' @param lty line types these are recycled over the levels of \code{by}
#' @param lwd line widths these are recycled over the levels of \code{by}
#' @param ... additional arguments are passed on to \code{\link{lines}}
#'
#' @details
#' The routine does not start a new plot. 
#'
#' @examples
#' x <- c(1,2,3,6,1,2,3,4,5,6)
#' y <- c(2,3,4,7,1,2,3,4,5,6)
#' z <- c(1,1,1,1,2,2,2,2,2,2)
#' plot(x, y, type='n')
#' linesby(x, y, x, col="steelblue")
#'
#' @export
linesby <- function(x, y, by, col = "black", lty = 1, lwd = 1, ...) {
    by   <- as.factor(by)
    lvls <- levels(by)
    col  <- rep(col, length.out = nlevels(by))
    lty  <- rep(lty, length.out = nlevels(by))
    lwd  <- rep(lwd, length.out = nlevels(by))
    for (i in seq_along(lvls)) {
        lvl  <- lvls[i]
        xlvl <- x[by == lvl]
        ylvl <- y[by == lvl]
        o    <- order(xlvl)
        lines(xlvl[o], ylvl[o], col=col[i], lwd=lwd[i], lty=lty[i], ...)
    }
}


#' Add an alpha value to a colour
#' 
#' @param col a vector with colors
#' @param alpha the alpha value to add (should be a single value between 
#'     0=transparent and 1=opaque)
#'
#' @examples
#' plot(rnorm(5000), rnorm(5000), pch=20, col=add_alpha("red", 0.3))
#'
#' @export
add_alpha <- function(col, alpha) {
    col <- col2rgb(col)
    if (identical(alpha, 1)) {
        res <- apply(col, 2, function(c) {
            rgb(c[1], c[2], c[3], maxColorValue=255)
        })
    } else {
        res <- apply(col, 2, function(c) {
            rgb(c[1], c[2], c[3], alpha=alpha*255, maxColorValue=255)
        })
    }
    return(res)
}

#' Generate a color palette for categorical variables using \code{\link{hcl}}
#'
#' @par n size of the palette; number of colors to generate.
#' @par ordered if FALSE the distance between subsequent colors is choosen as
#'   large as possible. If true the colors follow the color wheel.
#' @par offset the starting position on the color wheel; the hue. This 
#'   determines the first color. The default value of 40 generates an orangy 
#'   color. The value should be between 0 and 360. 
#' @par c The chroma of the colors; see \code{\link{hcl}}.
#' @par l The luminance of the colors; see \code{\link{hcl}}.
#' @par alpha The transparency of the colors; see \code{\link{hcl}}. This 
#'   should be a value between 0 (=tranparent) and 1 (=opaque). 
#' @par plot If TRUE a plot is generated showing the palette. 
#'
#' @examples
#' hcl_palette(10, plot = TRUE)
#'
#' @export
hcl_palette <- function(n, ordered = FALSE, offset=40, 
        c = 150, l = 60, alpha = NULL, plot = FALSE) {
    if (ordered) {
        pal  <- seq_len(n) - 1
    } else {
        pal  <- 0
        p    <- 0
        step <- min(4, n/2)
        for (i in seq_len(n-1)) {
            p <- floor(p + step) %% n
            while (p %in% pal) {
              p <- (p + 1) %% n
            }
            pal <- c(pal, p)
        }
    }
    h <- (pal * 360/n + offset) %% 360
    if (is.null(alpha)) {
        pal <- hcl(h=h, c=c, l=l)
    } else {
        pal <- hcl(h=h, c=c, l=l, alpha = alpha)
    }
    if (plot) {
        plot(0, 0, xlim=c(0,1), ylim=c(0, n), type='n', bty='n', xaxt='n', 
            yaxt='n', xlab='', ylab='', xaxs='i', yaxs='i')
        axis(2, at=seq_len(n) - 0.5, labels=1:n, las=2, lwd=0, lwd.ticks=1)
        rect(rep(0, n), seq(0, n-1), rep(1, n), seq(1, n), 
            col=pal, border=NA)
        text(0.5, 1:n-0.5, pal)
    }
    pal
}

    
