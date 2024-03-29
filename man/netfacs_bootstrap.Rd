% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netfacs-helpers.R
\name{netfacs_bootstrap}
\alias{netfacs_bootstrap}
\title{Calculate expected probability from single bootstrap}
\usage{
netfacs_bootstrap(
  subject,
  subject.weight,
  null.subjects,
  null.elements,
  test.combinations,
  max.combination.size,
  max.event.size
)
}
\arguments{
\item{subject}{A character vector of unique subjects present in the data}

\item{subject.weight}{A numeric vector of weights to be used when sampling
subjects}

\item{null.subjects}{A denoting the subject of null.elements}

\item{null.elements}{A list of active elements in the null condition}

\item{test.combinations}{A vector denoting AU combinations that are present
in the test data}

\item{max.combination.size}{A positive integer indicating the maximum AU
combination size considered in the bootstrap}

\item{max.event.size}{A positive integer indicating the maximum event size to
be considered}
}
\value{
A list of bootstrapped probabilities for combinations and event sizes
}
\description{
Calculate expected probability from single bootstrap
}
