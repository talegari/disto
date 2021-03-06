% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nn_utils.R
\name{nn2}
\alias{nn2}
\title{Extension of nn method for two matrices}
\usage{
nn2(x, y, k, r, method = "euclidean", ...)
}
\arguments{
\item{x}{Numeric matrix}

\item{y}{Numneric matrix}

\item{k}{Number of nearest neighbors}

\item{r}{Radius for nearest neighbors}

\item{method}{(string or function) Distance metric passed to `proxy::dist`}

\item{...}{Additional arguments are sent to
\code{\link[pbmcapply]{pbmclapply}}}
}
\value{
Object of class 'nn'. A list with these elements: \itemize{

  \item \emph{triplet}: Matrix with three columns: row, col and distance. For
  a fixed observation of matrix y (value in 'row'), all corresponding values in 'col' are
  the indexes of the nearest neighbors in  matrix x. All corresponding values in
  'distance' are the distances to those nearest neighbors

  \item \emph{size}: Number of rows of matrix y

  \item \emph{sizeX}: Number of rows of matrix x

  \item \emph{k} or \emph{r} : Depending on the input

  }
}
\description{
Find k or fixed radius nearest neighbors of each observation(row) matrix y in matrix x
}
\details{
Exactly one among k or r has to be provided
}
\examples{
temp <- nn2(x = matrix(rnorm(1e4), ncol = 10)
            , y = matrix(runif(1e3), ncol = 10)
            , r = 2
            )
temp
}
