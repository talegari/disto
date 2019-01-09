#' @name nn
#' @title Nearest neighbors and distances
#' @description Obtain nearest neighbors and distances from a matrix or disto
#'   handle. k nearest or fixed radius neighbors are supported
#' @param x Object of class 'disto' or a numeric matrix
#' @param k Number of nearest neighbors
#' @param r Radius for nearest neighbors
#' @param ... Additional arguments for \code{\link{dapply}} when x is 'disto'
#'   object. Else additional arguments are sent to
#'   \code{\link[pbmcapply]{pbmclapply}}
#' @details Exactly one among k or r has to be provided
#' @return A list with these elements: \itemize{
#'
#'   \item \emph{triplet}: Matrix with three columns: row, col and distance. For
#'   a fixed observation(value in 'row'), all corresponding values in 'col' are
#'   the indexes of the nearest neighbors. All corresponding values in
#'   'distance' are the distances to those nearest neighbors
#'
#'   \item \emph{size}: Size of the distance matrix or number of rows of the
#'   matrix
#'
#'   \item \emph{k} or \emph{r} : Depending on the input
#'
#'   }
#' @examples
#' \dontrun{
#' # create a matrix
#' set.seed(100)
#' mat <- cbind(rnorm(3e3), rpois(3e3, 1))
#'
#' # compute a distance matrix and get a disto handle
#' do <- stats::dist(mat)
#' dio <- disto(objectname = "do")
#'
#' # nearest neighbors: k nearest and fixed radius
#' nn(dio, k = 1)
#' nn(mat, k = 1) # distance method defaults to 'euclidean'
#' str(nn(mat, k = 1)) # observe the structure of the output
#'
#' nn(dio, r = 0.1)
#' nn(mat, r = 0.1)
#'
#' # nearest neighbors parallelized: k nearest and fixed radius
#' # fast computation, higher memory usage
#' nn(dio, k = 1, nproc = 2)
#' nn(mat, k = 1, mc.cores = 2)
#'
#' nn(dio, r = 0.1, nproc = 2)
#' nn(mat, r = 0.1, mc.cores = 2)
#'
#' # different distance method
#' do <- stats::dist(mat, method = "manhattan")
#'
#' nn(dio, k = 1, nproc = 2)
#' nn(mat, k = 1, method = "manhattan", mc.cores = 2)
#'
#' nn(dio, r = 0.1, nproc = 2)
#' nn(mat, r = 0.1, method = "manhattan", mc.cores = 2)
#' }
#' @export
nn <- function(x, k, r, ...){
  UseMethod("nn", x)
}

#' @export
nn.disto <- function(x
                     , k
                     , r
                     , ...
                     ){

  # assertions ----
  assertthat::assert_that(inherits(x, "disto"))
  sz <- size(x)
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


  # decide the type ----
  type <- ifelse(missing(k)
                 , function(vec, ind) nn_r(vec, ind, r = r)
                 , function(vec, ind) nn_k(vec, ind, k = k)
                 )

  # dapply over points ----
  tSparse <- do.call(rbind
                     , dapply(x        = x
                              , margin = 1
                              , fun    = type
                              , ...
                              )
                     )
  colnames(tSparse) <- c("row", "col", "distance")

  # return ----
  res <- list(triplet = tSparse, size = sz)

  if(mk){
    res[["r"]] <- r
  } else {
    res[["k"]] <- k
  }

  class(res) <- "nn"
  return(res)
}

#' @export
nn.matrix <- function(x
                      , k
                      , r
                      , method = "euclidean"
                      , ...
                      ){

  # assertions ----
  assertthat::assert_that(inherits(x, "matrix"))
  assertthat::assert_that(typeof(x) %in% c("integer", "double"))
  sz <- nrow(x)
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
  assertthat::assert_that(assertthat::is.string(method) || is.function(method))

  # decide the type ----
  type <- ifelse(missing(k)
                 , function(vec, ind) nn_r(vec, ind, r = r)
                 , function(vec, ind) nn_k(vec, ind, k = k)
                 )

  # function to get nn per obs ----
  nnPerObs <- function(i){
    proxy::dist(x, matrix(x[i, ], nrow = 1), method = method) %>%
      type(i)
  }

  # loop over obs ----
  tSparse <- do.call(rbind, pbmcapply::pbmclapply(1:sz, nnPerObs, ...))
  colnames(tSparse) <- c("row", "col", "distance")

  #return ----
  res <- list(triplet = tSparse, size = sz)

  if(mk){
    res[["r"]] <- r
  } else {
    res[["k"]] <- k
  }

  class(res) <- "nn"
  return(res)
}

#' @name nn_k
#' @title k Nearest neighbors
#' @description k Nearest neighbors from a vector of distances
#' @param vec Vector of distances
#' @param index dummy to facilitate dapply
#' @param k Number of nearest neighbors
nn_k <- function(vec, index, k){

  notMoreThanK <- (data.table::frankv(vec, ties.method = "dense") <= k + 1)
  notMoreThanK[index] <- FALSE

  cbind(index, which(notMoreThanK), vec[notMoreThanK])
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
  nearestIndexes <- which(withinR)

  if(length(nearestIndexes) != 0){
    cbind(index, nearestIndexes , vec[withinR])
  } else {
    matrix(nrow = 0, ncol = 3)
  }
}

#' @name print.nn
#' @title Print method for class 'nn'
#' @description Print method for class 'nn'
#' @param x Object of class 'nn'
#' @param ... stub
#' @return Returns the input invisibly besides printing on the screen
#' @export
print.nn <- function(x, ...){

  if("k" %in% names(x)){
    message("k Nearest neighbors of class 'nn'")
  } else {
    message("Fixed radius nearest neighbors of class 'nn'")
  }

  message("Dimension of sparse adjacency matrix: ", x[["size"]] , " X ", x[["size"]])

  return(invisible(x))
}
