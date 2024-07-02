#' Basic permutation
#' @author Tingwei Adeck
#' @param n an integer
#' @return a matrix or data frame
#' @export
#' @examples
#' aperm(6)
aperm <- function(n){

  n <- as.integer(ceiling(n))

  if (!is.integer(n) | n < 0) {
    rlang::warn(
      message = sprintf('%f must be an integer and must be > than 0', n),
      use_cli_format = T
    )
  }

  if (n == 1){
    matrix(1)
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

coins_mat_to_list <- function(df, num_coins){
  init_ls = vector(mode = 'list', length = nrow(df))
  res_ls = vector(mode = 'list', length = nrow(df))

  for (j in 1:nrow(df)) {
    for(l in 1: length(res_ls)){
      segment = df[j,]
      segment = as.character(segment)
      init_ls[[l]] = append(init_ls[[l]], segment)
    }
  }
  init_ls = init_ls[[1]]

  val = num_coins
  sn = c(1:val)
  for (l in 1:length(res_ls)){
    segment = init_ls[sn]
    incby = num_coins
    sn = sn + val
    res_ls[[l]] = append(res_ls[[l]], segment)
  }

  res_ls
}

acoinperm <- function(number_of_coins, out_format = c('char', 'num', 'list')){

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
