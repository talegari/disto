#' @name dapply
#' @title lapply like function for disto object
#' @description Apply function for data underlying disto object
#' @param x disto object
#' @param margin (one among 1 or 2) dimension to apply function along
#' @param fun Function to apply over the margin
#' @param subset (integer vector) Row/Column numbers along the margin. If this
#'   is missing, all rows or columns are considered
#' @param nproc Number of parallel processes (unix only)
#' @param progress (flag) Whether to show the progress bar or not. If missing
#'   and if the length of the subset is at least 1e4, progress is considered to
#'   be TRUE.
#' @details fun should accept two arguments(in the same order): First, a vector
#'   of distances (row or column of a disto depending on the margin). Second,
#'   the row or the column number. See the example below
#' @return A list
#' @examples
#' temp <- dist(iris[,1:4])
#' dio  <- disto(objectname = "temp")
#' # function to pick unsorted indexes of 5 nearest neighbors (excepting itself)
#' udf_nn <- function(distances, index){
#'
#'   nnPlusOne <- which(
#'                  data.table::frankv(distances, ties.method = "dense") <= 6)
#'   setdiff(nnPlusOne, index)
#' }
#' hi <- dapply(dio, 1, udf_nn)
#' head(hi)
#' max(sapply(hi, length))
#' @export
dapply <- function(x
                   , margin = 1
                   , fun
                   , subset
                   , nproc = 1
                   , progress
){

  assertthat::assert_that(inherits(x, "disto"))
  assertthat::assert_that(assertthat::is.scalar(margin) && margin %in% 1:2)
  assertthat::assert_that(is.function(fun))
  assertthat::assert_that(assertthat::is.count(nproc))
  assertthat::assert_that(missing(progress) || assertthat::is.flag(progress))

  size <- size(x)
  if(missing(subset)){
    subset <- 1:size
  } else {
    assertthat::assert_that(all(subset %in% 1:size))
  }
  if(missing(progress)){
    progress <- ifelse(size > 999, TRUE, FALSE)
  }

  if(progress){
    res <- pbmcapply::pbmclapply(subset
                                 , function(s) fun(as.numeric(x[s, ]), s)
                                 , mc.cores = nproc
    )
  } else {
    res <- parallel::mclapply(subset
                              , function(s) fun(as.numeric(x[s, ]), s)
                              , mc.cores = nproc
    )
  }
  return(res)
}

