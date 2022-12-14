% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recruit.R
\name{recruited}
\alias{recruited}
\title{Probability of recruitment}
\usage{
recruited(object, best.config = c("min", "1se"))
}
\arguments{
\item{object}{list of object of class \code{renoir.trained}}

\item{best.config}{which configuration to consider}
}
\value{
a \code{data.frame} containing the probability of recruitment for each coefficient
(number of times coefficient is nonzero / number of runs), or a list of \code{data.frame}
if response type is \code{"mgaussian"} or ungrouped \code{"multinomial"}.
}
\description{
Probability of recruitment
}
\author{
Alessandro Barberis
}
\keyword{internal}
