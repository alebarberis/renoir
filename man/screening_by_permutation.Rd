% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Screener.R
\name{screening_by_permutation}
\alias{screening_by_permutation}
\title{Compute the significance of the filtering test}
\usage{
screening_by_permutation(
  x,
  y,
  weights = NULL,
  resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
  observations = NULL,
  features,
  coef = NULL,
  return.type = c("pvalue", "object"),
  logger = Logger(verbose = F),
  multi = c("raw", "max", "average", "sum"),
  ...
)
}
\arguments{
\item{x}{the input matrix, where rows are observations and columns are variables.}

\item{y}{the response variable. Its number of rows must match the number of rows of \code{x}.}

\item{weights}{priors of the observations}

\item{resp.type}{the response type}

\item{observations}{(optional) indices of observations to keep}
}
\value{
The p-value associated with each feature
}
\description{
This function compute the significance of the screening test by using
\code{\link[renoir]{compute_sam}}
}
\keyword{internal}
