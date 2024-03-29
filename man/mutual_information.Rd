% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_tests.R
\name{mutual_information}
\alias{mutual_information}
\title{Compute score}
\usage{
mutual_information(
  x,
  g,
  discretization.method = "doane",
  out.type = c("mutual.information", "redundancy", "symmetric.uncertainty",
    "normalised.mutual.information"),
  verbose = F
)
}
\arguments{
\item{out.type}{the output type
\describe{
\item{mutual.information}{the mutual information; it is zero when the two variables are independent}
\item{redundancy}{a scaled and symmetric version of the mutual information;
it has a minimum value of zero when the two variables are independent; its maximum value is given
by min(H(X),H(Y))/(H(X)+H(Y))}
\item{symmetric.uncertainty}{another symmetric version of the mutual information;
it has a minimum value of zero when the two variables are independent}
\item{normalised.mutual.information}{it is derived from thinking of mutual information as
an analogue to covariance, and it is calculated akin to the Pearson correlation coefficient;
it ranges between \code{[0, 1]}, where 0 is related to two independent variables}
}}
}
\value{
a numerical value
}
\description{
Compute mutual information or a derived measure.
}
\keyword{internal}
