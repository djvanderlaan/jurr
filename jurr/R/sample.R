
#' Stochastic rounding
#' 
#' @param x the numeric vector that needs rounding
#'
#' @details
#' A value of for example 10.2 is rounded with a probability of 80% to 10 and 
#' 20% to 11. 
#'
#' @return
#' A vector with rounded values
#'
#' @seealso
#' For other rounding routines: \code{\link{round}}, \code{\link{cumround}}
#'
#' @examples
#' r <- round(rep(10.2, 100))
#' table(r)
#'
#' @export
stochround <- function(x) {
  result <- floor(x)
  remainder <- x - result
  up <- rbinom(length(x), 1, remainder) > 0
  result <- result + up
  result
}

#' Helper routine for cumulative rounding
#'
#' @param x the numeric vector that needs rounding. The values need to be 
#'     between 0 and 1.
#'
#' @details
#' Expects a vector with values between 0 and 1. Returns a vector with zeros 
#' and ones. The sum of this vector differs at most 1 from the sum of the 
#' original vector.
#'
#' @seealso
#' \code{\link{cumround}}
#'
#' @examples
#' x <- runif(100, 0,1)
#' y <- cumr(x)
#' sum(y) - sum(x)
#'
#' @export
cumr <- function(x) {
  r <- cumsum(x) + runif(1, min=0, max=1)
  r <- floor(r);
  diff(c(0, r));
}


#' Cumulative rounding
#'
#' @param x the numeric vector that needs rounding
#' 
#' @details
#' A form of stochastic rounding where it is ensured that the sum of the 
#' rounded vector matches the sum of the original vector (maximum difference
#' is <1). 
#'
#' @return
#' A rounded vector
#'
#' @seealso
#' For other rounding routines: \code{\link{round}}, \code{\link{stochround}}
#'
#' @examples
#' x <- rnorm(1000, 100, 5)
#' y <- cumround(x)
#' sum(x) - sum(y)
#'
#' @export
cumround <- function(x) {
  result <- floor(x)
  result + cumr(x - result)
}

#' Sample without replacement with unequal probabilities
#'
#' @param x vector from which should be sampled
#' @param n the number of items to sample (should be smaller or equal to the
#'     number of elements in \code{x})
#' @param prob vector with probabilities (should be of same length as \code{x})
#'
#' @details
#' Before drawing the probabilies are scaled such that the sum of the 
#' probabilities equals \code{n}. 
#'
#' Cumulative rounding is used to ensure that the number of elements is equal to
#' n. Elements with a probability larger or equal than one (after scaling) are 
#' always selected (when this are more than n elements, more then n elements 
#' are drawn). This proces of scaling and selecting elements with a probability
#' larger than one is repeated until all probabilites are smaller than one. 
#' Then a sample is drawn from the remaining elements using cumulative rounding
#' (see \code{\link{cumr}}).
#' 
#' @return
#' A list with the following elements:
#' \describe{
#'   \item{sel}{a logical vector with selected elements}
#'   \item{w}{the inclusion probabilites of the elements}
#' }
#'
#' @examples
#' x <- 1:1000
#' p <- x/1000
#' s <- sample(x, size=100, prob=p)
#'
#' @export
sampwor <- function(x, n, prob) {
  if (length(prob) != length(x)) stop('Length prob != length x.')
  if (n > length(x)) stop('Sample size n larger than population x.')
  # scale prob such that sum equals n
  prob   <- n*prob/sum(prob)
  index  <- 1:length(x)
  index1 <- c();
  index2 <- c();
  # check for objects with prob >= 1
  # remove these; these are always selected
  repeat {
    tmp    <- prob >= 1;
    index1 <- c(index1, index[tmp])
    index  <- index[!tmp]
    prob   <- prob[!tmp]
    n      <- n-sum(tmp)
    prob   <- n*prob/sum(prob)
    if (all(prob < 1)) break;
  }
  # when there are elements left; sample
  if (length(prob) > 0) {
    tmp    <- sample(1:length(index));
    prob   <- prob[tmp];
    index  <- index[tmp]
    index2 <- index[cumr(prob) >0]
  }
  # determine which elements are selected and their weights
  sel       <- rep(F, length(x))
  sel[c(index1, index2)] <- TRUE
  p         <- rep(NA, length(x))
  p[index1] <- 1
  p[index]  <- prob
  # return
  list(sel=sel, p=p)
}


#' Replicate elements of a vector according to a weights vector
#'
#' @param data elements to replicate
#' @param weights a numeric vector with weights
#' @param permute put elements in random order before 
#'
#' @details
#' When the weights are integer numbers \code{inflate} does the same as:
#' \code{\link{rep}(data, weights)}. Cumulative rounding (see 
#' \code{\link{cumround}} is used to round the weights. Therefore, the output 
#' dependent on random numbers and repeated calls will generate different 
#' results.
#'
#' @seealso
#' \code{\link{rep}}, \code{\link{cumround}}
#' 
#' @examples
#' inflate(1:4, weights=1:4+0.5)
#'
#' # In expectation inflate repeats each element weight times
#' mean(replicate(1000, length(inflate(1, 2.5))))
#' # rep does not
#' mean(replicate(1000, length(rep(1, 2.5))))
#'
#' @export
inflate <- function(data, weights, permute=FALSE) {
  n <- length(data);
  p <- 1:n
  if (permute) p <- sample(p)
  w <- cumround(weights[p]);
  r <- rep(p, w);
  data[r]
}

#' Weighted bootstrap
#'
#' @param data \code{data.frame} with data used for bootstrap
#' @param weights vector with weights (inverse inclusion probability) for each 
#'     of the rows in the data
#' @param nbstr number of bootstraps to perform
#' @param f the function that performs the estimate for each of the bootstraps
#' @param simplify should the results be simplified to a vector; see 
#'     \code{\link{sapply}}.
#' @param ... additional arguments are passed on to \code{f}
#'
#' @details
#' First inflates the sample given in \code{data} to the complete population.
#' Then repeatedly samples are drawn without replacement with probabilities 
#' equal to 1/\code{weights}. These samples are then passed on to the function 
#' \code{f}. 
#'
#' @return
#' A list with length \code{nbstr} with the results of the calls to \code{f}.
#'
#' @seealso
#' The package \code{boot} also contains routines for performing bootstraps. 
#' The \code{survey} package contains routines for obtaining variance estimates
#' for survey data. 
#'
#' @examples
#' # Create data set
#' samp <- data.frame(gender=rep(c('M', 'F'), c(1000, 500)),
#'     length=c(rnorm(1000, 1.85, 0.1), rnorm(500, 1.75, 0.1)),
#'     weights=c(10000/1000, 10000/500))
#' # Function to estimate average length
#' avlength <- function(d, w) {
#'     sum(d$length*w)/sum(w)
#' }
#' avlength(samp, samp$weights)
#' # Perform bootstrap
#' br <- wbstr(samp, samp$weights, 25, avlength)
#' sd(br)
#' 
#' @export
#
wbstr <- function(data, weights, nbstr, f, ..., simplify = TRUE) {
  bst <- function(i, N, n, D) {
    stp  <- sampwor(1:N, n, 1/weights[D]);
    d    <- D[stp$sel];
    d    <- data[d,];
    w    <- 1/stp$p[stp$sel];
    f(d, w,  ...)
  }
  n      <- nrow(data);
  D      <- inflate(1:n, weights);
  N      <- length(D);
  sapply(1:nbstr, bst, N, n, D, simplify=TRUE);
}

