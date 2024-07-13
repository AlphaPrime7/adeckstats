## adeckstats - R package for Random Numbers Distribution Simulation
## Copyright (C) 2024 Tingwei Adeck

#' Basic permutation
#' @family permutation
#' @param n an integer
#' @return a matrix or data frame
#' @export
#' @rdname permutation
adeck_perm <- function(n){

  n <- as.integer(ceiling(n))

  if (!is.integer(n) | n < 0) {
    rlang::warn(
      message = sprintf('%f must be an integer and must be > than 0', n),
      use_cli_format = T
    )
  }

  if (n == 1){
    matrix(n)
  }
  else {
    p <- 1
    for (i in 2:n) {
      x <- cbind(p,i)
      pseq <- seq(i)
      indx <- c(pseq, pseq[-i])
      nrowx <- nrow(x)
      prows = nrowx * i

      p <- matrix(0, nrow = prows, ncol = i)
      p[1:nrowx, ] <- x

      for (j in pseq[-i]) {
        p[j * nrowx + 1:nrowx, ] <- x[, indx[1:i + j]]
      }
    }
    p
  }
}

#' Coin Toss Permutations
#' @family permutation
#' @param number_of_coins int
#' @param out_format format type
#' @return out_format type
#' @export
#' @rdname permutation
adeck_coinperm <- function(number_of_coins, out_format = c('char', 'num', 'list')){

  n <- as.integer(round(number_of_coins))

  if (!is.integer(n) | n < 0) {
    rlang::warn(
      message = sprintf('%f must be an integer and must be > than 0', n),
      use_cli_format = T
    )
  }

  sides <- 'HT'
  c <- vector()

  for (i in 0:n - 1){
    if (i == 0)
      c <- c(c, NULL)
    else
      c <- c(c,sides)
  }

  if (length(c) == 0)
    warning('No coins tossed')

  if (length(c) == 1){
    n <- length(c)
    for (i in 1:n){
      x <- matrix(strsplit(c[i], "")[[1]], nrow = 1)
      if (is.null(out_format) | 'char' %in% out_format) {
        return(x)
      }
      else if ('num' %in% out_format){
        x[x == 'H'] <- 1
        x[x == 'T'] <- 0
        x = apply(x, 2, as.numeric)
        return(x)
      }
      else if ('list' %in% out_format){
        rlang::abort(
          message = 'Unable to return a single coin toss in list format',
          use_cli_format = T
        )
      }
    }
  }

  perm_mat <- data.frame()
  if (length(c) > 1){
    n <- length(c)
    bin_toss_outcomes = c('H', 'T')
    perm_mat <- gtools::permutations(length(bin_toss_outcomes), n, bin_toss_outcomes, repeats.allowed = T)
  }

  if (is.null(out_format) | 'char' %in% out_format) {
    perm_mat
  }
  else if ('num' %in% out_format){
    perm_mat[perm_mat == 'H'] <- 1
    perm_mat[perm_mat == 'T'] <- 0
    perm_mat = apply(perm_mat, 2, as.numeric)
    perm_mat
  }
  else if ('list' %in% out_format){
    pm <- coins_mat_to_list(perm_mat, n)
    pm
  }

}

