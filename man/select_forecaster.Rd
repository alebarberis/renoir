% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_ForecasterList.R
\name{select_forecaster,ForecasterList,character-method}
\alias{select_forecaster,ForecasterList,character-method}
\alias{select_forecaster,ForecasterList,Trained-method}
\title{Select forecaster}
\usage{
\S4method{select_forecaster}{ForecasterList,character}(object, by)

\S4method{select_forecaster}{ForecasterList,Trained}(object, by)
}
\arguments{
\item{object}{a S4 \linkS4class{ForecasterList} object}

\item{by}{the learning method to match. If missing, the \code{object} is returned
as it is. In case of no match \code{NULL} is returned.}
}
\value{
a \linkS4class{Forecaster} object
}
\description{
Select the first forecaster from the list matching the \code{learning.method}
}
\keyword{internal}
