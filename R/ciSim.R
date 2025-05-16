#' testUniCISim(100,200,1,1,1.96)
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
