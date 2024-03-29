% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Evaluated.R
\name{generate_name}
\alias{generate_name}
\title{Generate name}
\usage{
generate_name(object, key = c("id", "config", "response", "sampling"))
}
\arguments{
\item{object}{object of class \linkS4class{Evaluated}}

\item{key}{character vector indicating the elements to use for the name generation.
Allowed strings are \code{"id"}, \code{"config"}, \code{"response"}, and \code{"sampling"}}
}
\value{
A character string.
}
\description{
This function generates a name from a key selection.
}
\author{
Alessandro Barberis
}
\keyword{internal}
