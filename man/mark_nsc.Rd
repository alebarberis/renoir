% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/renoir_nearest_shrunken_centroid.R
\name{mark_nsc}
\alias{mark_nsc}
\title{Features Marker}
\usage{
mark_nsc(object, marking.system = c("presence"))
}
\arguments{
\item{marking.system}{a length-one character vector, indicating a strategy
for marking the features.}

\item{obejct}{an object of class \linkS4class{Trained}}
}
\value{
A list with one element per response, containing the marks for the features
}
\description{
This function marks the features of an object of class
\code{pamrtrained}.
}
\details{
The features in the \linkS4class{Trained} object in input
are marked, i.e. a numerical value is assigned to each feature
based on the choice of \code{marking.system}.

If \code{marking.system = "presence"}, a binary value of 1/0 is assigned
to the features by their presence in the model:
\code{1} is given to features that were used in the forest,
\code{0} otherwise.
}
\author{
Alessandro Barberis
}
