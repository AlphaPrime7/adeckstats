## adeckstats - R package for Random Numbers Distribution Simulation
## Copyright (C) 2024 Tingwei Adeck

#' Title: Source files
#' @param path path
#' @return NULL
#' @export
#' @keywords internal
source_files <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  sapply(files, source)
}

