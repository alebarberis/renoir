% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_ScorerList.R
\name{score,ScorerList,list,list-method}
\alias{score,ScorerList,list,list-method}
\title{Score}
\usage{
\S4method{score}{ScorerList,list,list}(
  scorer,
  true,
  pred,
  weights = NULL,
  multi = c("average", "sum"),
  grouped = TRUE,
  logger,
  ...
)
}
\arguments{
\item{scorer}{a \linkS4class{ScorerList} object}

\item{true}{a list of vectors (or matrices) of observed values. If list elements are matrices,
a multi-response is assumed}

\item{pred}{a list of vectors (or matrices) of predicted values}

\item{weights}{a list of vectors of observation weights}

\item{multi}{what to do when response has multiple output values
\describe{
  \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
  \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
}}

\item{grouped}{logical, whether to compute separate statistics when lists of values are provided}

\item{...}{further arguments to scorer function}
}
\value{
the computed accuracy measure for each \linkS4class{Scorer} in the \linkS4class{ScorerList}
}
\description{
Computes scores for different sets of data
}
