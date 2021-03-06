% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disto_methods.R
\name{`[.disto`}
\alias{`[.disto`}
\alias{[.disto}
\title{Extract from a disto object in matrix style extraction}
\usage{
\method{[}{disto}(x, i, j, k, product = "outer")
}
\arguments{
\item{x}{object of class 'disto'}

\item{i}{(integer vector) row indexes}

\item{j}{(integer vector) column indexes}

\item{k}{(integer vector) direct indexes}

\item{product}{(string) One among: "inner", "outer"}
}
\value{
When product is 'outer', returns a matrix. Else, a vector.
}
\description{
Extract a disto object in matrix style extraction and via direct
  indexing. 'product' specification allows both outer (matrix output, default
  option) and inner (vector) product type extraction. For dist backend see:
  \code{\link{dist_extract}}.
}
\examples{
temp <- stats::dist(iris[,1:4])
dio <- disto(objectname = "temp")
dio
names(dio) <- paste0("a", 1:150)

dio[1, 2]
dio[2, 1]
dio[c("a1", "a10"), c("a5", "a72")]
dio[c("a1", "a10"), c("a5", "a72"), product = "inner"]
dio[k = c(1,3,5)]
}
