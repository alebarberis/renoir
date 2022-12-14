% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Sampler.R
\name{rsample}
\alias{rsample}
\title{Random samples}
\usage{
rsample(N, n, strata = NULL, balance = FALSE, replace = FALSE)
}
\arguments{
\item{N}{population size.}

\item{n}{sample size.}

\item{strata}{(optional) vector of stratification variables. If provided must be of length \code{N}}

\item{balance}{logical, whether to (try to) balance the strata frequency in the output sample.
Used only if \code{strata} is provided.}

\item{replace}{logical, whether to sample with replacement. Default is \code{FALSE}.
Set to \code{TRUE} for bootstrap method.}
}
\value{
A vector of length \code{n} containing the index of the computed random set of observations.
}
\description{
Takes a sample with or without replacement from the population.
}
\details{
If \code{strata} is provided, sampling is applied within each stratum, with the strategy
depending on \code{balance}. If \code{balance = FALSE}, the proportion of the strata
in the population is maintained in the samples (also called "proportionate allocation").
If \code{balance = TRUE}, the proportion of strata in the sample is attempted to be balanced.
}
\author{
Alessandro Barberis
}
\keyword{internal}
