% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/renoir_nearest_shrunken_centroid.R
\name{record_nsc}
\alias{record_nsc}
\title{Features Recorder}
\usage{
record_nsc(object)
}
\arguments{
\item{obejct}{an object of class \code{pamrtrained}}
}
\value{
A list with one element per response, each element being
an integer vector of length equivalent to the number of features.
}
\description{
This function records the presence of the features
in an object of class \code{pamrtrained}.
}
\details{
The presence of the features in the \code{pamrtrained}
object in input is recorded and returned in a named vector
per output response.

If a feature is present in the model then the feature is returned with a \code{1}.
If a feature is missing in the model then the feature is returned with a \code{0}.
}
\author{
Alessandro Barberis
}
