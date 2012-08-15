

#' Draw lines for groups
#' 
#' @param x x-coordinates of points to join with line
#' @param y y-coordinates of points to join with line
#' @param by vector (converted to factor) defining the groups for which the 
#'     lines should be drawn.
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
linesby <- function(x, y, by, ...) {
    by   <- as.factor(by)
    lvls <- levels(by)
    for (lvl in lvls) {
        xlvl <- x[by == lvl]
        ylvl <- y[by == lvl]
        o    <- order(xlvl)
        lines(xlvl[o], ylvl[o], ...)
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

    
