% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netfacs_extract.R
\name{netfacs_extract}
\alias{netfacs_extract}
\alias{netfacs.extract}
\title{Extract results from a netfacs object}
\usage{
netfacs_extract(
  netfacs.data,
  combination.size = NULL,
  significance = 1,
  min.count = 0,
  min.prob = 0
)

netfacs.extract(
  netfacs.data,
  combination.size = NULL,
  significance = 1,
  min.count = 0,
  min.prob = 0
)
}
\arguments{
\item{netfacs.data}{An object of class \code{\link{netfacs}}.}

\item{combination.size}{Numeric, denoting the combination size(s) that should
be extracted. If \code{NULL} (default), all combination sizes are returned.}

\item{significance}{Numeric value between 0 and 1, determining the p-value
below which combinations are considered to be dissimilar enough from the
null distribution.}

\item{min.count}{Numeric, denoting the minimum number of times an element
combination occurred.}

\item{min.prob}{Numeric value between 0 and 1, denoting the minimum
probability an element combination occurred to be displayed.}
}
\value{
Function returns a \code{\link[tibble:tibble]{tibble}} data.frame
  that contains the results of the \code{\link{netfacs}} object. By default,
  returns all results for all observed combinations, but can optionally
  pre-filter results.
}
\description{
Extract results from a \code{\link{netfacs}} object.
}
\examples{
### how do angry facial expressions differ from non-angry ones?
data(emotions_set)
angry.face <- netfacs(
  data = emotions_set[[1]],
  condition = emotions_set[[2]]$emotion,
  test.condition = "anger",
  ran.trials = 10,
  combination.size = 2
)

netfacs_extract(angry.face)
}
