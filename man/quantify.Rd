% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes_generics.R, R/class_Quantifier.R
\name{quantify}
\alias{quantify}
\alias{quantify,Quantifier-method}
\title{Quantify Uncertainty}
\usage{
quantify(quantifier, ...)

\S4method{quantify}{Quantifier}(quantifier, ...)
}
\arguments{
\item{quantifier}{a \linkS4class{Quantifier} object}

\item{...}{further arguments to quantifier function}

\item{x}{a vector (or a matrix) of values. If a matrix is provided,
a multi-response is assumed}

\item{weights}{observation weights}
}
\value{
the computed quantification of the uncertainty
}
\description{
This function quantitatively characterizes the uncertainties in the model performance.
}
\author{
Alessandro Barberis
}
