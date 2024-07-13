## adeckstats - R package for Random Numbers Distribution Simulation
## Copyright (C) 2024 Tingwei Adeck

#' Normal Distribution Simulation
#' @name normalsim
#' @param .n n
#' @param .mu mean
#' @param .sd sd
#' @param .num_sims sims
#' @param rep repeat
#' @return list of tibbles
#' @seealso
#' \code{\link[adeckstats]{adeck_normsim_by_mu_sigma}}
#' @examples
#' \dontrun{
#' adeck_normsim(10,1,2)
#' }
adeck_normsim <- function(.n=NULL, .mu=NULL, .sd=NULL, .num_sims = NULL, rep = FALSE){
  UseMethod("adeck_normsim")
}

#' @rdname normalsim
#' @return list of tibbles
#' @export
adeck_normsim.default <- function(.n=NULL, .mu=NULL, .sd=NULL, .num_sims = NULL, rep = FALSE){
  if (is.null(.num_sims)) {
    .num_sims = 1
  }

  n <- as.integer(.n)
  num_sims <- as.integer(.num_sims)
  mu = as.numeric(.mu)
  sigma = as.numeric(.sd)

  if (!is.integer(n)) {
    rlang::abort(
      message = sprintf('%d must be an integer and must be greater than 0', n),
      use_cli_format = TRUE
    )
  }

  if (!is.integer(num_sims)) {
    rlang::abort(
      message = sprintf('%d must be an integer', num_sims),
      use_cli_format = TRUE
    )
  }

  if (rep){
    if (!is.null(.n) & !is.null(.mu) & !is.null(.sd)){
      df <- data.table::CJ(
        sim_number = factor(1:num_sims),
        x = 1:n)

      set.seed(1234)
      df <- df[, y := rep(stats::rnorm(n = n, mean = mu, sd = sigma), num_sims)]

      df <- df[, c('dx', 'dy') := stats::density(y, n = n)[c('x', 'y')],
               by = sim_number]

      df[, p := stats::pnorm(y, mean = mu, sd = sigma)]

      df[, q := stats::qnorm(p, mean = mu, sd = sigma)]

      df
    }
    else {
      num_sims = num_sims
      n = 100
      mu = 0
      sigma = 1
      df <- data.table::CJ(
        sim_number = factor(1:num_sims),
        x = 1:n)

      df <- df[, y := rep(stats::rnorm(n = n, mean = mu, sd = sigma), num_sims)]

      df <- df[, c('dx', 'dy') := stats::density(y, n = n)[c('x', 'y')],
               by = sim_number]

      df[, p := stats::pnorm(y, mean = mu, sd = sigma)]

      df[, q := stats::qnorm(p, mean = mu, sd = sigma)]

      df

    }

  }
  else {
    if (!is.null(.n) & !is.null(.mu) & !is.null(.sd)){
      df <- data.table::CJ(
        sim_number = factor(1:num_sims),
        x = 1:n)

      df <- df[, y := stats::rnorm(n = .N, mean = mu, sd = sigma)]

      df <- df[, c('dx', 'dy') := stats::density(y, n = n)[c('x', 'y')],
               by = sim_number]

      df[, p := stats::pnorm(y, mean = mu, sd = sigma)]

      df[, q := stats::qnorm(p, mean = mu, sd = sigma)]

      df
    }
    else {
      num_sims = num_sims
      n = 100
      mu = 0
      sigma = 1
      df <- data.table::CJ(
        sim_number = factor(1:num_sims),
        x = 1:n)

      df <- df[, y := stats::rnorm(n = .N, mean = mu, sd = sigma)]

      df <- df[, c('dx', 'dy') := stats::density(y, n = n)[c('x', 'y')],
               by = sim_number]

      df[, p := stats::pnorm(y, mean = mu, sd = sigma)]

      df[, q := stats::qnorm(p, mean = mu, sd = sigma)]

      df
    }

  }

  anorm_list = extract_rows_by_simnum(df)

  names(anorm_list) <- paste0("sim_", seq_along(anorm_list))

  class(anorm_list) <- c('adeck_normsim', class(anorm_list))

  attr(anorm_list, "distribution_family_type") <- "continuous"
  attr(anorm_list, "mean_approx") <- mu
  attr(anorm_list, "sd_approx") <- sigma
  attr(anorm_list, "per_tibble_nrows") <- nrow(anorm_list[[1]])
  attr(anorm_list, "num_sims") <- length(anorm_list)
  attr(anorm_list, "tibble_types") <- "adeck_gaussian"

  anorm_list
}

#' @rdname normalsim
#' @return list of tibbles
#' @keywords internal
.adeck_normsim <- function(.n=NULL, .mu=NULL, .sd=NULL, .num_sims = NULL, rep = FALSE){
  if (is.null(.num_sims)) {
    .num_sims = .n
  }

  n <- as.integer(.n)
  num_sims <- as.integer(.num_sims)
  mu = as.numeric(.mu)
  sigma = as.numeric(.sd)

  if (!is.integer(n)) {
    rlang::abort(
      message = sprintf('%d must be an integer and must be greater than 0', n),
      use_cli_format = TRUE
    )
  }

  if (!is.integer(num_sims)) {
    rlang::abort(
      message = sprintf('%d must be an integer', num_sims),
      use_cli_format = TRUE
    )
  }

  if (rep){
    if (!is.null(.n) & !is.null(.mu) & !is.null(.sd)){
      df <- data.table::CJ(
        sim_number = factor(1:num_sims),
        x = 1:n)

      set.seed(1234)
      df <- df[, y := rep(stats::rnorm(n = n, mean = mu, sd = sigma), num_sims)]

      df <- df[, c('dx', 'dy') := stats::density(y, n = n)[c('x', 'y')],
               by = sim_number]

      df[, p := stats::pnorm(y, mean = mu, sd = sigma)]

      df[, q := stats::qnorm(p, mean = mu, sd = sigma)]

      df
    }
    else {
      num_sims = num_sims
      n = 100
      mu = 0
      sigma = 1
      df <- data.table::CJ(
        sim_number = factor(1:num_sims),
        x = 1:n)

      df <- df[, y := rep(stats::rnorm(n = n, mean = mu, sd = sigma), num_sims)]

      df <- df[, c('dx', 'dy') := stats::density(y, n = n)[c('x', 'y')],
               by = sim_number]

      df[, p := stats::pnorm(y, mean = mu, sd = sigma)]

      df[, q := stats::qnorm(p, mean = mu, sd = sigma)]

      df

    }

  }
  else {
    if (!is.null(.n) & !is.null(.mu) & !is.null(.sd)){
      df <- data.table::CJ(
        sim_number = factor(1:num_sims),
        x = 1:n)

      df <- df[, y := stats::rnorm(n = .N, mean = mu, sd = sigma)]

      df <- df[, c('dx', 'dy') := stats::density(y, n = n)[c('x', 'y')],
               by = sim_number]

      df[, p := stats::pnorm(y, mean = mu, sd = sigma)]

      df[, q := stats::qnorm(p, mean = mu, sd = sigma)]

      df
    }
    else {
      num_sims = num_sims
      n = 100
      mu = 0
      sigma = 1
      df <- data.table::CJ(
        sim_number = factor(1:num_sims),
        x = 1:n)

      df <- df[, y := stats::rnorm(n = .N, mean = mu, sd = sigma)]

      df <- df[, c('dx', 'dy') := stats::density(y, n = n)[c('x', 'y')],
               by = sim_number]

      df[, p := stats::pnorm(y, mean = mu, sd = sigma)]

      df[, q := stats::qnorm(p, mean = mu, sd = sigma)]

      df
    }


  }

  aggregated_df <- df %>%
    dplyr::group_by(sim_number) %>%
    dplyr::summarise(
      x = list(x),
      y = list(y),
      dx = list(dx),
      dy = list(dy)
    )

  anorm_list <- aggregated_df %>%
    purrr::pmap(list)

  names(anorm_list) <- paste0("sim_", seq_along(anorm_list))

  class(anorm_list) <- c('adeck_normsim', class(anorm_list))

  anorm_list
}

#' Normal Simulation by Mean SD Combination
#' @name normalsim_by_mu_sigma
#' @param .n n
#' @param .mu mean
#' @param .sd standard deviation
#' @param out_format output format (list preferable)
#' @return list of tibbles
#' @seealso
#' \code{\link[adeckstats]{adeck_normsim}}
#' @examples
#' \dontrun{
#' adeck_normsim_by_mu_sigma(10,c(0,1),c(1,2))
#' }
adeck_normsim_by_mu_sigma <- function(.n=NULL, .mu=NULL, .sd=NULL, out_format = c('list', 'tibble')){
  UseMethod("adeck_normsim_by_mu_sigma")
}

#' @rdname normalsim_by_mu_sigma
#' @return list of tibbles
#' @export
adeck_normsim_by_mu_sigma.default <- function(.n=NULL, .mu=NULL, .sd=NULL, out_format = c('list', 'dataframe')){

  n <- as.integer(.n)
  mu = as.numeric(.mu)
  sigma = as.numeric(.sd)

  if (!is.integer(n)) {
    rlang::abort(
      message = sprintf('%d must be an integer and must be greater than 0', n),
      use_cli_format = TRUE
    )
  }

  new_tbl_list <- list()
  id_vector <- c()

  if (!is.null(.n) & !is.null(.mu) & !is.null(.sd)){
    df <- data.table::CJ(
      x = 1:n,
      mu = mu,
      sigma = sigma)

    tbl_list <- extract_rows_by_mu_sigma(df)

    for (i in 1:length(tbl_list)) {
      df <- tbl_list[[i]]
      df <- data.table::as.data.table(df)
      set.seed(1234)
      mu <- df[['mu']][1]
      sigma <- df[['sigma']][1]
      id_vector <- c(id_vector, sprintf('mean_%f_sigma_%f', mu, sigma))
      df <- df[, y := stats::rnorm(n = .N, mean = mu, sd = sigma)]
      df <- df[, c('dx', 'dy') := stats::density(y, n = n )[c('x', 'y')],
               by = c('mu', 'sigma')]
      df[, p := stats::pnorm(y, mean = mu, sd = sigma)]
      df[, q := stats::qnorm(p, mean = mu, sd = sigma)]
      class(df) <- c('adeck_norm_tbl', class(df))
      new_tbl_list[[i]] <- df
    }

    new_tbl_list
  }
  else {
    n = 100
    mu = c(0,1)
    sigma = c(1,2)
    df <- data.table::CJ(
      x = 1:n,
      mu = mu,
      sigma = sigma)

    tbl_list <- extract_rows_by_mu_sigma(df)

    for (i in 1:length(tbl_list)) {
      df <- tbl_list[[i]]
      df <- data.table::as.data.table(df)
      set.seed(1234)
      mu <- df[['mu']][1]
      sigma <- df[['sigma']][1]
      id_vector <- c(id_vector, sprintf('mean_%f_sigma_%f', mu, sigma))
      df <- df[, y := stats::rnorm(n = .N, mean = mu, sd = sigma)]
      df <- df[, c('dx', 'dy') := stats::density(y, n = n )[c('x', 'y')],
               by = c('mu', 'sigma')]
      df[, p := stats::pnorm(y, mean = mu, sd = sigma)]
      df[, q := stats::qnorm(p, mean = mu, sd = sigma)]
      class(df) <- c('adeck_norm_tbl', class(df))
      new_tbl_list[[i]] <- df
    }

    new_tbl_list
  }

  if ('list' %in% out_format | is.null(out_format)){
    new_tbl_list
    class(new_tbl_list) <- c('adeck_normsim', class(new_tbl_list))
    names(new_tbl_list) <- paste0("sim_", id_vector[seq_along(id_vector)])
    new_tbl_list
  }
  else {
    fdf <- combine_tibbles(new_tbl_list)
    sim_number <- c()
    for (i in 1:length(new_tbl_list)){
      sim_number <- c(sim_number,rep(i, n))
    }
    sim_number <- as.data.frame(sim_number)
    fdf <- cbind(fdf, sim_number)
    fdf = fdf %>% dplyr::relocate(sim_number, .before=y)
    fdf$sim_number <- as.factor(fdf$sim_number)
    class(fdf) <- c('adeck_norm_combined_tbl', class(fdf))
    fdf
  }

}

#' @rdname normalsim_by_mu_sigma
#' @return list of tibbles
#' @keywords internal
.adeck_normsim_by_mu_sigma <- function(.n=NULL,
                                       .mu=NULL,
                                       .sd=NULL,
                                       out_format = c('list', 'dataframe')){

  n <- as.integer(.n)
  mu = as.numeric(.mu)
  sigma = as.numeric(.sd)

  if (!is.integer(n)) {
    rlang::abort(
      message = sprintf('%d must be an integer and must be greater than 0', n),
      use_cli_format = TRUE
    )
  }

  cp = as.data.frame(expand.grid(mu, sigma))
  colnames(cp) = c('mu', 'sigma')

  cp_list <- list_of_vectors(cp)
  new_tbl_list <- list()
  id_vector <- c()

  for (i in 1:nrow(cp)) {
    mu = cp[i,'mu']
    sigma = cp[i,'sigma']
    id_vector <- c(id_vector, sprintf('mean_%f_sigma_%f', mu, sigma))

    df <- data.table::data.table(x = 1:n, mean = mu, sd = sigma)
    df <- df[, y := stats::rnorm(n = .N, mean = mu, sd = sigma)]
    df <- df[, c('dx', 'dy') := stats::density(y, n = n)[c('x', 'y')],
             by = c('mean', 'sd')]
    df[, p := stats::pnorm(y, mean = mu, sd = sigma)]
    df[, q := stats::qnorm(p, mean = mu, sd = sigma)]
    class(df) <- c('adeck_norm_tbl', class(df))
    new_tbl_list[[i]] <- df
  }

  class(new_tbl_list) <- c('adeck_normsim', class(new_tbl_list))
  names(new_tbl_list) <- paste0("sim_", id_vector[seq_along(id_vector)])
  new_tbl_list

}


