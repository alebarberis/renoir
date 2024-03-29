% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Sampler.R
\name{get_foldid}
\alias{get_foldid}
\title{Get foldid}
\usage{
get_foldid(samples, N)
}
\arguments{
\item{samples}{list of samples as returned by resample}

\item{N}{population size}
}
\value{
a vector containing the fold ids
}
\description{
Helper function that, given a list of indices as returned by
resample, reshapes the data to obtain a vector containing the fold ids.
}
\keyword{internal}
