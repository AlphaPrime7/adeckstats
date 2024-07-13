## adeckstats - R package for Random Numbers Distribution Simulation
## Copyright (C) 2024 Tingwei Adeck

#' Binary Search
#' @family binarysearch
#' @param vect vector
#' @param key search value
#' @param fun boolean init
#' @return NULL
#' @name binarysearch
NULL

#' @rdname binarysearch
#' @return index
#' @note The original function I wrote while learning the binary search algorithm
#' @export
adeck_binarysearch <- function(vect,key = NULL, fun=NULL){
  vect <- sort(vect)
  indx1 <- length(vect) - (length(vect) - 1)
  indx_last <- length(vect)

  nofun <- is.null(fun)
  found = FALSE

  while (indx1 <= indx_last) {
    mid <- round((indx1 + indx_last) / 2)

    if (key < vect[[mid]]){
      indx_last <- mid - 1
    }
    else if (key > vect[[mid]]){
      indx1 <- mid + 1
    }
    else {
      message(sprintf('The searched key %d is found in index %d of the target vector', key, mid))
      found = TRUE
      return(mid)
    }

  }

  if(!found){
    print('not found')
    return(NULL)
  }
}

#' @rdname binarysearch
#' @return index
.adeck_binarysearch <- function(vect, key = NULL, fun = NULL) {
  vect <- sort(vect)

  indx1 <- 1
  indx_last <- length(vect)

  nofun <- is.null(fun)
  found <- FALSE

  while (indx1 <= indx_last) {
    mid <- floor((indx1 + indx_last) / 2)

    if (key < vect[mid]) {
      indx_last <- mid - 1
    } else if (key > vect[mid]) {
      indx1 <- mid + 1
    } else {
      found <- TRUE
      message(sprintf('The searched key %d is found in index %d of the sorted target vector', key, mid))
      return(mid)
    }
  }

  if (!found && nofun) {
    message('The searched key is not found in the vector')
    return(NULL)
  }

  return(NULL)
}

#' @rdname binarysearch
#' @return index
#' @export
adeck_binarysearch_mult <- function(vect, key = NULL, fun = NULL) {

  vect <- sort(vect)

  indx1 <- 1
  indx_last <- length(vect)

  nofun <- is.null(fun)
  found <- FALSE
  result_indices <- c()

  while (indx1 <= indx_last) {
    mid <- floor((indx1 + indx_last) / 2)

    if (key < vect[mid]) {
      indx_last <- mid - 1
    } else if (key > vect[mid]) {
      indx1 <- mid + 1
    } else {
      found <- TRUE
      left <- mid
      right <- mid

      while (left > 1 && vect[left - 1] == key) {
        left <- left - 1
      }

      while (right < length(vect) && vect[right + 1] == key) {
        right <- right + 1
      }

      result_indices <- c(left:right)
      message(sprintf('The searched key %d is found in indices %s of the sorted target vector', key, toString(result_indices)))
      return(result_indices)
    }
  }

  if (!found && nofun) {
    message('The searched key is not found in the vector')
    return(NULL)
  }

  return(NULL)
}

#' @rdname binarysearch
#' @return index
#' @export
adeck_binarysearch_recursive <- function(vect, key = NULL, fun = NULL) {
  vect <- sort(vect)

  binarysearch_helper <- function(vect, key, left, right) {
    if (left > right) {
      return(NULL)
    }

    mid <- floor((left + right) / 2)

    if (key < vect[mid]) {
      return(binarysearch_helper(vect, key, left, mid - 1))
    } else if (key > vect[mid]) {
      return(binarysearch_helper(vect, key, mid + 1, right))
    } else {
      result_indices <- mid
      left_search <- mid - 1
      right_search <- mid + 1

      while (left_search >= left && vect[left_search] == key) {
        result_indices <- c(left_search, result_indices)
        left_search <- left_search - 1
      }

      while (right_search <= right && vect[right_search] == key) {
        result_indices <- c(result_indices, right_search)
        right_search <- right_search + 1
      }

      return(result_indices)
    }
  }

  indx1 <- 1
  indx_last <- length(vect)

  result_indices <- binarysearch_helper(vect, key, indx1, indx_last)

  if (!is.null(result_indices)) {
    message(sprintf('The searched key %d is found in indices %s of the sorted target vector', key, toString(result_indices)))
    return(result_indices)
  } else {
    message('The searched key is not found in the vector')
    return(NULL)
  }
}

#' Index Search
#' @family indexsearch
#' @param vect vector
#' @param key search value
#' @param fun boolean init
#' @return NULL
#' @name indexsearch
NULL

#' @rdname indexsearch
#' @return NULL
#' @export
adeck_indexsearch <- function(vect, key = NULL, fun=NULL){

  nofun <- is.null(fun)
  found = FALSE

  while (found != nofun) {
    for (i in 1:length(vect)){
      if (vect[i] == key){
        found = TRUE
        message(sprintf('The searched key %d is found in index %d of the sorted target vector', key, i))
        return(i)
      }
    }
  }
}

#' @rdname indexsearch
#' @return index
#' @export
adeck_indexsearch_mult <- function(vect, key = NULL, fun = NULL) {
  nofun <- is.null(fun)
  found <- FALSE
  result_indices <- c()

  while (found != nofun) {
    for (i in 1:length(vect)) {
      if (vect[i] == key) {
        found <- TRUE
        result_indices <- c(result_indices, i)
      }
    }

    if (found) {
      message(sprintf('The searched key %d is found in indices %s of the target vector', key, toString(result_indices)))
      return(result_indices)
    } else {
      if (nofun) {
        message('The searched key is not found in the vector')
        return(NULL)
      } else {
        break
      }
    }
  }
}
