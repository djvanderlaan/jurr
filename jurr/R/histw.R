# Maakt een gewogen histogram
#
# Versie: 20 mei 2009

histw <- function(y, borders, w = rep(1, length(y)), centers=c(), count=0, as.list=TRUE) {
  if (count > 0) {
    borders = seq(min(y),max(y)+max((max(y)-min(y))/100/count,.Machine$double.eps),length.out=count)
  }
  if (length(centers) > 1) {
    nc <- length(centers)
    d0 <- centers[2]-centers[1]
    d1 <- centers[nc]-centers[nc-1]
    borders <- c(centers[1]-d0/2, (centers[2:nc] + centers[1:(nc-1)])/2, centers[nc]+d1/2)
  }
  n <- length(borders)-1;
  if (n == 0) stop("The size of borders should be > 1.")
  h <- .C("histw_c", h=double(n+2), 
    as.double(y),
    as.double(w),
    as.integer(length(y)),
    as.double(borders),
    as.integer(length(borders)))$h  
  underflow <- h[1]
  overflow  <- h[length(h)]
  h         <- h[2:(length(h)-1)]
  if (underflow != 0) warning("There are observations smaller than the lower border.");
  if (overflow != 0) warning("There are observations larger than the upper border.");
  if (as.list) {
    centers   <- (borders[1:n+1]+borders[1:n])/2
    result    <- list(h=h, underflow=underflow, overflow=overflow, centers=centers, borders=borders)
    class(result) <- "histw"
    return(result)
  } else {
    return(h)
  }
}

histw2 <- function(x, y, borders.x, borders.y, w = rep(1, length(y)), centers.x=c(), centers.y=c(), count.x=0, count.y=0, as.list=TRUE) {
  if (count.x > 0) {
    borders.x = seq(min(x),max(x)+max((max(x)-min(x))/100/count.x,.Machine$double.eps),length.out=count.x)
  }
  if (count.y > 0) {
    borders.y = seq(min(y),max(y)+max((max(y)-min(y))/100/count.y,.Machine$double.eps),length.out=count.y)
  }
  if (length(centers.x) > 1) {
    nc <- length(centers.x)
    d0 <- centers.x[2]-centers.x[1]
    d1 <- centers.x[nc]-centers.x[nc-1]
    borders.x <- c(centers.x[1]-d0/2, (centers.x[2:nc] + centers.x[1:(nc-1)])/2, centers.x[nc]+d1/2)
  }
  if (length(centers.y) > 1) {
    nc <- length(centers.y)
    d0 <- centers.y[2]-centers.y[1]
    d1 <- centers.y[nc]-centers.y[nc-1]
    borders.y <- c(centers.y[1]-d0/2, (centers.y[2:nc] + centers.y[1:(nc-1)])/2, centers.y[nc]+d1/2)
  }
  n.x <- length(borders.x)-1;
  n.y <- length(borders.y)-1;
  if (n.x == 0) stop("The size of x-borders should be > 1.")
  if (n.y == 0) stop("The size of y-borders should be > 1.")
  h <- .C("histw2_c", h=double((n.x+2)*(n.y+2)), 
    as.double(x),
    as.double(y),
    as.double(w),
    as.integer(length(x)),
    as.double(borders.x),
    as.integer(length(borders.x)),
    as.double(borders.y),
    as.integer(length(borders.y)))$h  
  h         <- t(array(h, dim=c(n.x+2, n.y+2)))  
  underflow <- list(x=h[,1], y=h[1,])
  overflow  <- list(x=h[,n.x+2], y=h[n.y+2,])
  h         <- h[2:(n.y+1), 2:(n.x+1)]
  if (any(underflow$x != 0) || any(underflow$y != 0)) warning("There are observations smaller than the lower borders.");
  if (any(overflow$x != 0) || any(overflow$y != 0)) warning("There are observations larger than the upper borders.");
  if (as.list) {
    centers.x   <- (borders.x[1:n.x+1]+borders.x[1:n.x])/2
    centers.y   <- (borders.y[1:n.y+1]+borders.y[1:n.y])/2
    result      <- list(h=h, underflow=underflow, overflow=overflow, centers.x=centers.x, 
	borders.x=borders.x, centers.y=centers.y, borders.y=borders.y)
    class(result) <- "histw2"
    return(result)
  } else {
    return(h)
  }
}

project <- function(hist, dimension) {
  if (!is(hist, "histw2")) stop("Expecting an object of type histw2 as first argument. Got something else.")
  if (dimension == 1 || dimension == 'x') {
    h         <- apply(hist$h, 1, sum)
    h         <- h + hist$underflow$x[2:(length(hist$underflow$x)-1)] + 
        hist$overflow$x[2:(length(hist$overflow$x)-1)]
    underflow <- sum(hist$underflow$y)
    overflow  <- sum(hist$overflow$y)
    centers <- hist$centers.y
    borders <- hist$borders.y  
  } else if (dimension == 2 || dimension == 'y') {
    h         <- apply(hist$h, 2, sum)
    h         <- h + hist$underflow$y[2:(length(hist$underflow$y)-1)] + 
        hist$overflow$y[2:(length(hist$overflow$y)-1)]
    underflow <- sum(hist$underflow$x)
    overflow  <- sum(hist$overflow$x)
    centers <- hist$centers.x
    borders <- hist$borders.x  
  } else {
    stop("Dimension should be 1 or 2, or 'x' or 'y'.")
  }
  result <- list(h=h, underflow=underflow, overflow=overflow, centers=centers, borders=borders)
  class(result) <- "histw"  
  return(result)
}

slice <- function(hist, dimension, bin) {
  if (!is(hist, "histw2")) stop("Expecting an object of type histw2 as first argument. Got something else.")
  if (dimension == 1 || dimension == 'x') {
    if (bin < 1 || bin > length(hist$centers.x)) stop("Invalid bin")
    h         <- hist$h[ ,bin]
    underflow <- hist$underflow$y[bin+1]
    overflow  <- hist$overflow$y[bin+1]  
    centers   <- hist$centers.y
    borders   <- hist$borders.y  
  } else if (dimension == 2 || dimension == 'y') {
    if (bin < 1 || bin > length(hist$centers.y)) stop("Invalid bin")
    h         <- hist$h[bin, ]
    underflow <- hist$underflow$x[bin+1]
    overflow  <- hist$overflow$x[bin+1]  
    centers   <- hist$centers.x
    borders   <- hist$borders.x  
  } else {
    stop("Dimension should be 1 or 2, or 'x' or 'y'.")
  }
  result <- list(h=h, underflow=underflow, overflow=overflow, centers=centers, borders=borders)
  class(result) <- "histw"  
  return(result)
}


plot.histw <- function(hist, ...) {
  histplot(hist$borders, hist$h, ...)
}

plot.histw2 <- function(hist, ...) {
  histplot2(hist$h, borders.x=h$borders.x, borders.y=h$borders.y, ...)
}

print.histw <- function(hist) {
  w       <- 15
  h       <- format(hist$h, width=w)
  borders <- format(hist$borders, width=w)
  cat("Frequency table:\n")
  cat(format("Bin", width=w, justify="right"), 
      format("From", width=w, justify="right"), 
      format("To", width=w, justify="right"), 
      format("Count", width=w, justify="right"), 
      "\n", sep="")
  for (i in 1:length(hist$h)) {
    cat(format(i, width=w), borders[i], borders[i+1], h[i], "\n", sep="")
  }
  if (hist$underflow != 0 || hist$overflow != 0) {  
    cat("\n")
    cat("Underflow:", hist$underflow, "\n")  
    cat("Overflow: ", hist$underflow, "\n")  
  }  
  cat("\n")
}

print.histw2 <- function(hist) {
  wy        <- 15
  wx        <- max(nchar(format(hist$h)), nchar(format(hist$borders.x)))  
  h         <- format(hist$h, width=wx)
  borders.x <- format(hist$borders.x, width=wx)  
  borders.y <- format(hist$borders.y, width=wy)  
  bins.x    <- format(1:ncol(h), width=wx)
  cat("Frequency table:\n")
  cat(format("Bin Y", width=wy, justify="right"), 
      format("From Y", width=wy, justify="right"), 
      format("Bin X/From X", width=floor((length(borders.x)*(wx+1)-1)/2), justify="right"), 
      "\n", sep=" ")
  cat(rep('-', (wx+1)*length(borders.x)+wy*2+1), "\n", sep='')
  cat(format("", justify="right", width=wy), 
      format("", justify="right", width=wy), 
      bins.x, "\n", sep=' ')
  cat(format("", justify="right", width=wy), 
      format("", justify="right", width=wy), 
      borders.x, "\n", sep=' ')
  cat(rep('-', (wx+1)*length(borders.x)+wy*2+1), "\n", sep='')
  for (i in 1:nrow(h)) {
    cat(format(i, width=wy), borders.y[i], h[i,], "\n", sep=' ')
  }  
  cat(format("", width=wy), borders.y[i+1], "\n", sep=' ')
  cat(rep('-', (wx+1)*length(borders.x)+wy*2+1), "\n", sep='')

  underoroverflow <- (sum(hist$underflow$x) + sum(hist$overflow$x) + sum(hist$underflow$y) + sum(hist$overflow$y)) > 0
  if (underoroverflow) {
    cat("\n")
    cat("Underflow X:", sum(hist$underflow$x), "\n")  
    cat("          Y:", sum(hist$underflow$y), "\n")  
    cat("Overflow  X:", sum(hist$overflow$x), "\n")  
    cat("          Y:", sum(hist$overflow$y), "\n")  
  }
  cat("\n")
}

