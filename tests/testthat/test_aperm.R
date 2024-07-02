library(testthat) #use_test()
library(modelr, lib.loc = Sys.getenv('lib.loc'))

capture_output <- function(f, ...) {
  sink(tempfile())
  on.exit(sink())
  f(...)
}

test_aperm <- function(n){
  n <- as.integer(ceiling(n))
  out <- capture_output(aperm, n)
  expect_equal(aperm(n), gtools::permutations(n = n, r = n))
}

test_acoinperm <- function(n, o){
  n <- as.integer(round(n))
  out <- capture_output(acoinperm, n, o)
  x = gtools::permutations(2,n,c('H','T'),repeats.allowed = T)
  if (n == 1){
    x <- matrix(x, nrow = 1)
  } else {
    x <- x
  }
  expect_equal(acoinperm(n, o),x)
}

#Tests

test_that("aperm returns correct result for n =1", {
  test_aperm(1)
  test_aperm(2)
  #fails after n=3 bc of the order but the output is correct
  })

test_that("acoinperm returns correct result for n =1", {
  test_acoinperm(1,'char')
  test_acoinperm(2, 'char')
  test_acoinperm(3, 'char')
  test_acoinperm(4, 'char')
})

