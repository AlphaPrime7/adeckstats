## adeckstats - R package for Random Numbers Distribution Simulation
## Copyright (C) 2024 Tingwei Adeck

#' Extract tibbles
#' @family utils
#' @family extractors
#' @param df df
#' @return list
#' @export
#' @rdname extractors
extract_rows_by_simnum <- function(df) {
  df %>%
    dplyr::group_by(sim_number) %>%
    dplyr::group_split()
}

#' Extract tibbles
#' @family utils
#' @family extractors
#' @param df df
#' @return list
#' @export
#' @rdname extractors
extract_rows_by_mu_sigma <- function(df) {
  df %>%
    dplyr::group_by(mu, sigma) %>%
    dplyr::group_split()
}

#' List of Vectors
#' @family utils
#' @param cp Cartesian product
#' @return lov
#' @export
list_of_vectors <- function(cp){
  lov <- apply(cp, 2, function(row) as.vector(row))
  return(lov)
}

#' Combine Tibbles
#' @family utils
#' @family combine_tibbles
#' @param ls list
#' @return tibble
#' @export
#' @rdname combine_tibbles
combine_tibbles <- function(ls){
  atb = data.frame()
  sim_number <- c()
  n <- nrow(ls[[1]])
  for (i in 1:length(ls)){
    tb = ls[[i]]
    atb = dplyr::bind_rows(atb, tb)
  }

  if ( !('sim_number' %in% colnames(atb)) ) {
    for (i in 1:length(ls)){
      sim_number <- c(sim_number,rep(i, n))
    }
    sim_number <- as.data.frame(sim_number)
    atb <- cbind(atb, sim_number)
    atb = atb %>% dplyr::relocate(sim_number, .before=y)
    atb$sim_number <- as.factor(atb$sim_number)
    class(atb) <- c('adeck_norm_combined_tbl', class(atb))
    atb
  }
  else {
    class(atb) <- c('adeck_norm_combined_tbl', class(atb))
    atb
  }

}

#' Combine Tibbles
#' @family utils
#' @family combine_tibbles
#' @param ls list
#' @return tibble
#' @keywords internal
#' @rdname combine_tibbles
.combine_tibbles <- function(ls){
  atb = data.frame()
  for (i in 1:length(ls)){
    tb = ls[[i]]
    atb = rbind(atb, tb)
  }
  atb
}

#' Coins Matrix to List
#' @family utils
#' @family permutation_helpers
#' @param df df
#' @param num_coins number of coins
#' @return list
#' @export
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

#' Check Broken Packages
#' @family utils
#' @family dev_helpers
#' @return NULL
#' @export
check_broken_packages <- function(){
  .libPaths() %>%
    purrr::set_names() %>%
    purrr::map(function(lib) {
      .packages(all.available = TRUE, lib.loc = lib) %>%
        purrr::keep(function(pkg) {
          f <- system.file('Meta', 'package.rds', package = pkg, lib.loc = lib)
          tryCatch({readRDS(f); FALSE}, error = function(e) TRUE)
        })
    })
}

