#' @name disto
#' @title Constructor for class 'disto'
#' @description Create mapping to data sources storing distances(symmetric),
#'   dissimilarities(non-symmetric), similarities and so on
#' @details This is a wrapper to create a 'disto' handle over different backends
#'   storing distances, dissimilarities, similarities etc with minimal data
#'   overhead like a database connection. The following named arguments are
#'   required to set-up the backend:
#'
#'   \itemize{
#'
#'   \item \strong{dist}:
#'
#'   \itemize{
#'
#'   \item objectname: Object of the class 'dist' or the name of the object as a
#'   'string'.
#'
#'   \item env: Environment where the object exists. When this is missing, its
#'   assumed to be parent environment.#'
#'
#'   }
#'
#'   \item \strong{bigdist}:
#'
#'   \itemize{
#'
#'   \item object: Object of the class 'bigdist'
#'
#'   }
#'
#'   }
#' @param ... Arguments for a backend. See details
#' @param backend (string) Specify a backend. Currently supported: 'dist', 'bigdist'
#' @return Object of class 'disto' which is a thin wrapper on a list
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio  <- disto(objectname = "temp")
#' dio
#' unclass(dio)
#' @export
disto <- function(...
                  , backend = "dist"
                  ){

  assertthat::assert_that(assertthat::is.string(backend))

  backends  <- c("dist", "bigdist")
  assertthat::assert_that(backend %in% backends)

  arguments <- list(...)

  res <- switch(backend
         , dist    = disto_dist(arguments)
         , bigdist = disto_bigdist(arguments)
         )
  res$backend <- backend
  return(res)

}

#' @name disto_dist
#' @title Constructior of disto with dist backend
#' @description  Constructior of disto with dist backend
#' @param arguments to construct disto object
#' @return returns a list
#' @details to be used by disto constructor function
disto_dist <- function(arguments){

  if(is.null(arguments$env)){
    arguments$env <- .GlobalEnv
  } else {
    assertthat::assert_that(is.environment(arguments$env))
  }
  assertthat::assert_that(assertthat::is.string(arguments$objectname))

  assertthat::assert_that(
    exists(arguments$objectname, envir = arguments$env)
    , msg = "Unable to find the object in the specified environment."
    )
  assertthat::assert_that(
    inherits(get(arguments$objectname, envir = arguments$env)
             , "dist"
             )
    )

  dlist        <- vector("list")
  dlist$name   <- arguments$objectname
  dlist$env    <- arguments$env
  class(dlist) <- "disto"

  return(dlist)
}

#' @name disto_bigdist
#' @title Constructior of disto with bigdist backend
#' @description  Constructior of disto with bigdist backend
#' @param arguments to construct disto object
#' @return returns a list
#' @details to be used by disto constructor function
disto_bigdist <- function(arguments){

  assertthat::assert_that(!is.null(arguments$object))
  assertthat::assert_that(inherits(arguments$object, "bigdist"))

  dlist        <- vector("list")
  dlist$obj   <- arguments$object
  class(dlist) <- "disto"

  return(dlist)
}

#' @name size
#' @title Obtain size of the disto object
#' @description Obtain size of the disto object
#' @param x object of class disto
#' @param ... currently not in use
#' @return Integer vector of length 1
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio   <- disto(objectname = "temp")
#' size(dio)
#' @export
size <- function(x, ...){

  res <- switch(x$backend
    , dist    = attr(get(x$name, envir = x$env), "Size")
    , bigdist = bigdist::bigdist_size(x$obj)
    )

  return( res )
}

#' @name names.disto
#' @title Get names/labels
#' @description Get names/labels of the underlying distance storing backend
#' @param x disto object
#' @return A character vector
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio <- disto(objectname = "temp")
#' dio
#' names(dio) <- paste0("a", 1:150)
#' @export
names.disto <- function(x){

  res <- switch(x$backend
                , dist = attr(get(x$name, envir = x$env), "Labels")
                , bigdist = NULL
                )
  return(res)
}

#' @name `names<-.disto``
#' @title Set names/labels
#' @description Set names/labels of the underlying distance storing backend
#' @param x disto object
#' @param value A character vector
#' @return invisible disto object
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio <- disto(objectname = "temp")
#' dio
#' names(dio) <- paste0("a", 1:150)
#' @export
`names<-.disto` <- function(x, value){

  assertthat::assert_that(is.character(value))
  assertthat::assert_that(length(value) == size(x))

  res <- switch (x$backend,
    dist = {
      val <- eval(`attr<-`(get(x$name, envir = x$env)
                      , "Labels"
                      , value
                    )
             , envir = x$env
             )

      assign(x$name, val, envir = x$env)

    }
    , bigdist  = message("Not implemented for bigdist")
  )

  return(invisible(x))
}

#' @name print.disto
#' @title Print method for dist class
#' @description Print method for dist class
#' @param x object of class disto
#' @param ... currently not in use
#' @return invisible NULL. Function writes backend type and size to terminal as
#'   a message.
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio   <- disto(objectname = "temp")
#' print(dio)
#' @export
print.disto <- function(x, ...){
  message("disto with backend: ", x$backend)
  message("size: ", size(x))

  return(invisible(x))
}

#' @name summary.disto
#' @title Summary method for dist class
#' @description Summary method for dist class
#' @param object object of class disto
#' @param ... currently not in use
#' @return Result of summary function
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio   <- disto(objectname = "temp")
#' dio
#' summary(dio)
#' @export
summary.disto <- function(object, ...){

  res <- switch(object$backend
    , dist = {
        summary.default(get(object$name, envir = object$env))
        print(object)
    }
    , bigdist = message("Not implemented for bigdist")
    )

  return(invisible(res))
}

#' @name `[.disto`
#' @title Extract from a disto object in matrix style extraction
#' @description Extract a disto object in matrix style extraction and via direct
#'   indexing. 'product' specification allows both outer (matrix output, default
#'   option) and inner (vector) product type extraction. For dist backend see:
#'   \code{\link{dist_extract}}.
#' @param x object of class 'disto'
#' @param i (integer vector) row indexes
#' @param j (integer vector) column indexes
#' @param k (integer vector) direct indexes
#' @param product (string) One among: "inner", "outer"
#' @return When product is 'outer', returns a matrix. Else, a vector.
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio <- disto(objectname = "temp")
#' dio
#' names(dio) <- paste0("a", 1:150)
#'
#' dio[1, 2]
#' dio[2, 1]
#' dio[c("a1", "a10"), c("a5", "a72")]
#' dio[c("a1", "a10"), c("a5", "a72"), product = "inner"]
#' dio[k = c(1,3,5)]
#' @export
`[.disto` <- function(x, i, j, k, product = "outer"){

  res <- switch(x$backend
    , dist = dist_extract(get(x$name, envir = x$env)
                          , i
                          , j
                          , k
                          , product = product
                          )
    , bigdist = bigdist::bigdist_extract(
                          x$obj
                          , i
                          , j
                          , k
                          , product = product
                          )
                )

  return(res)

}

#' @name `[[.disto`
#' @title Extract a single value from disto object
#' @description Extract a single value from disto object in matrix style
#'   extraction and via direct indexing. This does not support using names. This
#'   is faster than \code{link{extract}}. For dist backend see:
#'   \code{\link{dist_extract}}.
#' @param x object of class 'disto'
#' @param i (integer vector) row index
#' @param j (integer vector) column index
#' @param k (integer vector) direct index
#' @return (A real number)  Distance value
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio  <- disto(objectname = "temp")
#' dio
#'
#' dio[[1, 2]]
#' dio[[2, 1]]
#' dio[[k = 3]]
#' @export
`[[.disto` <- function(x, i, j, k){

  size <- size(x)

  if(!missing(k)){
    assertthat::assert_that(missing(i) && missing(j))
    assertthat::assert_that(assertthat::is.scalar(k) &&
                            assertthat::is.count(k) &&
                            k <= size * (size - 1)/2
                            )
  } else {
    assertthat::assert_that(assertthat::is.scalar(i) &&
                              assertthat::is.count(i) &&
                              i <= size
                            )
    assertthat::assert_that(assertthat::is.scalar(j) &&
                              assertthat::is.count(j) &&
                              j <= size
                            )
  }

  res <- switch(x$backend
    ,
    dist = {
      if(missing(k)){

        `[`(get(x$name, envir = x$env), dist_ij_k(i, j, size))

      } else {

        `[`(get(x$name, envir = x$env), k)

      }
    }
    ,
    bigdist = {

      if(missing(k)){

        x$obj$fbm[i, j]

      } else {

        ijVec <- as.integer(dist_k_ij[k])
        x$obj$fbm[ijVec[1], ijVec[2]]

      }

    }
    )

  return(res)
}

#' @name `[<-.disto`
#' @title In-place replacement of values
#' @description For dist backend see: \code{\link{dist_replace}}.
#' @param x object of class 'disto'
#' @param i (integer vector) row index
#' @param j (integer vector) column index
#' @param k (integer vector) direct index
#' @param value (integer/numeric vector) Values to replace
#' @return Invisible disto object. Note that this function is called for its
#'   side effect.
#' @examples
#' temp       <- stats::dist(iris[,1:4])
#' dio        <- disto(objectname = "temp")
#' names(dio) <- paste0("a", 1:150)
#' dio
#'
#' dio[1, 2] <- 10
#' dio[1,2]
#'
#' dio[1:10, 2:11] <- 100
#' dio[1:10, 2:11, product = "inner"]
#'
#' dio[paste0("a", 1:5), paste0("a", 6:10)] <- 101
#' dio[paste0("a", 1:5), paste0("a", 6:10), product = "inner"]
#' @export
`[<-.disto` <- function(x, i, j, k, value){

  res <- switch(x$backend
    , dist = {
        if(missing(k)){
          val <- dist_replace(object = get(x$name, envir = x$env)
                        , i = i
                        , j = j
                        , value = value
                        )
        } else {
          val <- dist_replace(object = get(x$name, envir = x$env)
                        , k = k
                        , value = value
                        )
        }

        assign(x$name, val, envir = x$env)

    }
    , bigdist = {

      if(missing(k)){
        val <- bigdist::bigdist_replace(x$obj
                                        , i = i
                                        , j = j
                                        , value = value
                                        )
      } else {
        val <- bigdist::bigdist_replace(x$obj
                                        , k = k
                                        , value = value
                                        )
      }

    }
    )

  return(invisible(x))
}

#' @name plot.disto
#' @title Plot a disto object
#' @description Density plot a disto object
#' @param x object of class disto
#' @param ... Currently unused
#' @return Plot output as side effect
#' @examples
#' temp <- stats::dist(iris[,1:4])
#' dio  <- disto(objectname = "temp")
#' plot(dio)
#' @export
plot.disto <- function(x, ...){
  if(x$backend == "dist"){
    data.frame(values = unclass(get(x$name, envir = x$env))) %>%
      ggplot2::ggplot(ggplot2::aes_string("values")) +
      ggplot2::geom_density() +
      ggplot2::xlab("Distances") +
      ggplot2::ggtitle("Density plot of distances")
  }

  if(x$backend == "bigdist"){
    message("Not implemented for bigdist")
  }
}
