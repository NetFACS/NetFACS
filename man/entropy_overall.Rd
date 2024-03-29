% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entropy_overall.R
\name{entropy_overall}
\alias{entropy_overall}
\title{Calculate information content of the dataset}
\usage{
entropy_overall(x)
}
\arguments{
\item{x}{An object of class \code{\link{netfacs}} or simply a binary matrix
of 0s and 1s, with elements in columns and events in rows.}
}
\value{
Function returns a summary \code{\link[tibble:tibble]{tibble}}
  containing the observed entropy, expected entropy and entropy ratio
  (observed / expected) of the dataset. Observed entropy is calculated using
  Shannon's information entropy formula \eqn{- \sum_{i = 1}^n p_i \log
  (p_i)}. Expected entropy is based on randomization (shuffling the observed
  elements while maintaining the number of elements per row) and represents
  the maximum entropy that a dataset with the same properties as this one can
  reach. Ratios closer to 0 are more ordered; ratios closer to 1 are more
  random.
}
\description{
Compares the observed and expected information content of the dataset.
}
\examples{
### how do angry facial expressions differ from non-angry ones?
data(emotions_set)
angry.face <- netfacs(
  data = emotions_set[[1]],
  condition = emotions_set[[2]]$emotion,
  test.condition = "anger",
  ran.trials = 100,
  combination.size = 2
)

entropy_overall(angry.face)
}
\references{
Shannon, C. E. (1948). A Mathematical Theory of Communication.
  \emph{Bell System Technical Journal}.
  \code{https://doi.org/10.1002/j.1538-7305.1948.tb01338.x}
}
