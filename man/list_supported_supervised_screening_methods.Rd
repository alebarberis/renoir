% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Screener.R
\name{list_supported_supervised_screening_methods}
\alias{list_supported_supervised_screening_methods}
\title{Supported Supervised Screening}
\usage{
list_supported_supervised_screening_methods(resp.type, y)
}
\arguments{
\item{resp.type}{(optional) the response type}

\item{y}{(optional) the response variable}
}
\value{
A \code{data.frame} with 3 columns:
\describe{
\item{\code{id}}{contains the id used in renoir for the supervised screening method (e.g. 'ebayes')}
\item{\code{name}}{contains a short description of the method (e.g. 'empirical Bayes moderated t-statistics test')}
}
}
\description{
This function returns a \code{data.frame} containing the currently
supported supervised screening methods.
}
\details{
The currently implemented supervised screening methods are returned in a \code{data.frame}.
If \code{resp.type} and \code{y} are provided, only the metrics supported for the given response
are returned.
}
\seealso{
\code{\link{ebayes_screener}},
\code{\link{permutation_screener}},
\code{\link{default_screener}}
}
\author{
Alessandro Barberis
}
