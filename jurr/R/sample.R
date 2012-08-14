
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
#' @result
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

# SAMPWOR
# Trekt een steekproef zonder teruglegging met ongelijke insluitkansen. Gebruikt
# hiervoor cumulatief afronden
#
sampwor <- function(x, n, prob) {
  if (length(prob) != length(x)) stop('Length prob != length x.')
  if (n > length(x)) stop('Sample size n larger than population x.')
  # schaal prob zodat de som overeenkomt met n
  prob   <- n*prob/sum(prob)
  index  <- 1:length(x)
  index1 <- c();
  index2 <- c();
  # controleer of er objecten zijn met prob >= 1
  # verwijder deze; deze worden altijd geselecteerd
  repeat {
    tmp    <- prob >= 1;
    index1 <- c(index1, index[tmp])
    index  <- index[!tmp]
    prob   <- prob[!tmp]
    n      <- n-sum(tmp)
    prob   <- n*prob/sum(prob)
    if (all(prob < 1)) break;
  }
  # als er nog objecten over zijn: trek de steekproef
  if (length(prob) > 0) {
    tmp    <- sample(1:length(index));
    prob   <- prob[tmp];
    index  <- index[tmp]
    index2 <- index[cumr(prob) >0]
  }
  # bepaal welke objecten geselecteerd zijn en de gewichten
  sel       <- rep(F, length(x))
  sel[c(index1, index2)] <- TRUE
  p         <- rep(NA, length(x))
  p[index1] <- 1
  p[index]  <- prob
  # klaar
  list(sel=sel, p=p)
}

# INFLATE
# Blaas de vector data op naar de totale populatie gebruik makend van de gewichten gegeven 
# in weights. Als permute=TRUE dan wordt de data eerst in willekeurige volgorde gezet. 
#
inflate <- function(data, weights, permute=FALSE) {
  n <- length(data);
  p <- 1:n
  if (permute) p <- sample(p)
  w <- cumround(weights[p]);
  r <- rep(p, w);
  data[r]
}


# WBSTR
# Gewogen bootstrap. Blaast de data eerst op naar de populatie vervolgens wordt hieruit
# herhaaldelijk steekproeven getrokken met behulp van sampwor. Deze steekproef wordt
# doorgegeven aan de functie f. wbstr geeft een vector/lijst met de resultaten van de 
# functie f terug. 
#
wbstr <- function(data, weights, nbstr, f, ...) {
  bst <- function(i, N, n, D) {
    stp  <- sampwor(1:N, n, 1/weights[D]);
    d    <- D[stp$sel];
    d    <- data[d,];
    w    <- 1/stp$p[stp$sel];
    f(d, ...)
  }
  n      <- nrow(data);
  D      <- inflate(1:n, weights);
  N      <- length(D);
  lapply(1:nbstr, bst, N, n, D);
}

