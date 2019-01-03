#' @name nn
#' @title Nearest neighbors and distances
#' @description Obtain index of nearest neighbors and distances from a disto
#'   handle. k nearest or fixed radius neighbors are supported
#' @param distoObject Object of class 'disto'
#' @param k Number of nearest neighbors
#' @param r Radius for nearest neighbors
#' @param ... Additional arguments for \code{\link{dapply}}
#' @details Exactly one among k or r has to be provided
#' @return A list with size of 'distObject' or length of the subset argument
#'   provided in dotdotdot. Each element of the list is a list with two
#'   elements: One is indexes (integer vector), other is distances (numeric
#'   vectors)
#' @examples
#' do  <- dist(iris[, 1:4])
#' dio <- disto(objectname = "do")
#' nn(dio, k = 5)
#' nn(dio, r = 3)
#' @export
nn <- function(distoObject
               , k
               , r
               , ...
               ){

  # assertions ----
  assertthat::assert_that(inherits(distoObject, "disto"))
  mk <- missing(k)
  mr <- missing(r)
  assertthat::assert_that(mk || mr
                          , msg = "One among k or r should be missing"
                          )
  assertthat::assert_that(!(mk && mr)
                          , msg = "One among k or r should be provided"
                          )
  if(mk){
    assertthat::assert_that(assertthat::is.number(r) && (r > 0))
  } else {
    assertthat::assert_that(assertthat::is.count(k))
  }


  # decide the method ----
  method <- ifelse(missing(k)
                   , function(vec, ind) nn_r(vec, ind, r = r)
                   , function(vec, ind) nn_k(vec, ind, k = k)
                   )

  # dapply over points ----
  res <- dapply(x        = distoObject
                , margin = 1
                , fun    = method
                , ...
                )

  return(res)

}

#' @name nn_k
#' @title k Nearest neighbors
#' @description k Nearest neighbors from a vector of distances
#' @param vec Vector of distances
#' @param index dummy to facilitate dapply
#' @param k Number of nearest neighbors
nn_k <- function(vec, index, k){

  notMoreThanK        <- (data.table::frankv(vec, ties.method = "dense") <= k)
  notMoreThanK[index] <- FALSE

  list(indexes = which(notMoreThanK), distances = vec[notMoreThanK])

}

#' @name nn_r
#' @title Fixed radius Nearest neighbors
#' @description Fixed radius Nearest neighbors from a vector of distances
#' @param vec Vector of distances
#' @param index dummy to facilitate dapply
#' @param r Radius for nearest neighbors
nn_r <- function(vec, index, r){

  withinR        <- (vec <= r)
  withinR[index] <- FALSE

  list(indexes = which(withinR), distances = vec[withinR])

}
