% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performance.R
\name{area_under_roc_curve}
\alias{area_under_roc_curve}
\title{Area Under the ROC Curve}
\source{
This function is just a wrapper to \code{\link[survival]{concordance}}
}
\usage{
area_under_roc_curve(true, pred, weights = NULL, multi, ...)
}
\arguments{
\item{true}{a vector (or a matrix) of observed values. If a matrix is provided,
a multi-response is assumed}

\item{pred}{a vector (or a matrix) of predicted values}

\item{weights}{observation weights}

\item{multi}{currently not used}

\item{...}{further arguments (currently not used)}
}
\value{
A length-one numeric vector
}
\description{
This function computes the area under the ROC curve.
}
\details{
The Area Under the ROC Curve (AUC) measures the entire two-dimensional
area under the receiver operating characteristic (ROC) curve.
It provides an aggregate measure of performance across all possible classification
thresholds.
}
\author{
Alessandro Barberis
}
