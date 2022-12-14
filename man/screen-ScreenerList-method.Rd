% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_ScreenerList.R
\name{screen,ScreenerList-method}
\alias{screen,ScreenerList-method}
\title{Features screening}
\usage{
\S4method{screen}{ScreenerList}(
  screener,
  x,
  y,
  weights = NULL,
  resp.type,
  observations = NULL,
  combine = c("max", "average", "sum"),
  order = c("increasing", "decreasing"),
  cutoff = NULL,
  maxvars = NULL,
  logger = Logger(verbose = F),
  ...
)
}
\arguments{
\item{screener}{a \linkS4class{ScreenerList} object}

\item{x}{the input matrix, where rows are observations and columns are variables.}

\item{y}{the response variable. Its number of rows must match the number of rows of \code{x}.}

\item{weights}{priors of the observations}

\item{resp.type}{the response type}

\item{observations}{(optional) indices of observations to keep}

\item{combine}{how to combine the results from multiple screeners
\describe{
  \item{\code{max}}{the max value of scores across multiple \linkS4class{Screened} objects is selected to get a single value for each observation}
  \item{\code{average}}{scores of multiple \linkS4class{Screened} objects are averaged to get a single value for each observation}
  \item{\code{sum}}{scores of multiple \linkS4class{Screened} objects are summed up to get a single value for each observation}
}}

\item{order}{whether to order the results by the score values
\describe{
  \item{\code{increasing}}{results are sorted in increasing order}
  \item{\code{decreasing}}{results are sorted in decreasing order}
  \item{\code{none}}{results are not sorted}
}}

\item{cutoff}{(optional) numeric, the value to use as a threshold for filtering not significant variables}

\item{maxvars}{integer, the maximum number of variables to keep. If the filtering is returning an higher number of variables,
only the most significant \code{maxvars} variables are kept.}
}
\value{
a \linkS4class{Screened} object
}
\description{
This function performs a feature screening by using the provided screeners. It combines
the results from the different screeners and return a \linkS4class{Screened} object
}
\author{
Alessandro Barberis
}
