## adeckstats - R package for Random Numbers Distribution Simulation
## Copyright (C) 2024 Tingwei Adeck

#' CI Simulations
#' @family cisimulations
#' @param n sample size
#' @param m number of iterations
#' @param mean mean
#' @param sd standard deviation
#' @param z z-value from z distribution
#' @param beta dependence modifier coefficient
#' @return count
#' @export
#' @rdname cisimulation
#' @examples
#' \dontrun{testUniCISim(100,200,1,1,1.96)
#' testMvCISim(n=100, m = 200, mean=1,sd=1,z=1.96)}
NULL

#' @rdname cisimulation
#' @return count
#' @export
testUniCISim <- function(n, m, mean, sd, z) {
  countV <- c()
  ciV <- c()
  sum <- 0
  for (i in 1:m) {
    x <- rnorm(n, mean = mean, sd=sd)
    cil <- mean(x) - (z*(sd/sqrt(n)))
    ciu <- mean(x) + (z*(sd/sqrt(n)))
    ciV <- c(cil, ciu)
    #cil >= mean || ciu <= mean
    if ( mean < cil || mean > ciu) {
      countV <- c(countV, TRUE)
      sum <- sum + 1
    }
    
  }
  print(sum)
  return(length(countV))
  
} 

#' @rdname cisimulation
#' @return count
#' @export
testMvCISim <- function(beta = 20, n, m, mean, sd, z) {
  x = 1:n
  countV <- c()
  ciV <- c()
  sum <- 0
  for (i in 1:m) {
    D <- outer(x, x, FUN = "-")
    S <- sd * exp(-abs(D) / beta)
    y <- mvrnorm(1, rep(mean,n), Sigma = S)
    cil <- mean(y) - (z*(sd/sqrt(n)))
    ciu <- mean(y) + (z*(sd/sqrt(n)))
    ciV <- c(cil, ciu)
    if (mean < cil || mean > ciu) {
      countV <- c(countV, TRUE)
      sum <- sum + 1
    }
  }
  print(sum)
  return(length(countV))
  
}

