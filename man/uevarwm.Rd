% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confidence.R
\name{uevarwm}
\alias{uevarwm}
\title{Unbiased Variance of the Weighted Mean}
\usage{
uevarwm(x, weights, na.rm = T)
}
\arguments{
\item{x}{vector of measurements}

\item{weights}{vector of weights}

\item{na.rm}{logical, whether \code{NA} values in \code{x} should be stripped before the computation proceeds}
}
\value{
A length-one numeric vector.
}
\description{
Computes an unbiased estimate of the variance of the weighted sample mean.
}
\details{
Currently, this implementation is not considering eventual stratification
in the estimate of the variance. We also assume that the measurements
(i.e. \code{x}) are drawn from the same population with variance
\eqn{V(x_{i}) = \sigma_{x}^2}.

The variance of the sample mean is then computed as

\deqn{\hat{\sigma}_{W}^{2} = V(\frac{\sum_{i=1}^{n} w_{i}x_{i}}{\sum_{i=1}^{n} w_{i}})
= \frac{1}{(\sum_{i=1}^{n} w_{i})^2} V(\sum_{i=1}^{n} w_{i}x_{i})
= \frac{\sum_{i=1}^{n} w_{i}^2}{(\sum_{i=1}^{n} w_{i})^2} \sigma_{x}^2}

There are different ways to estimate the variance of the \code{x} (\eqn{\sigma_{x}^2}). Here we
use the common formula for unbiased estimator of unweighted data

\deqn{s_{x}^2 = \frac{\sum_{i=1}^{n} (x_{i} - \bar{x})^2}{n - 1}}
}
\author{
Alessandro Barberis
}
