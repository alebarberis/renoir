% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Scorer.R
\name{summary_score,Scorer,list,list-method}
\alias{summary_score,Scorer,list,list-method}
\title{Score}
\usage{
\S4method{summary_score}{Scorer,list,list}(
  scorer,
  true,
  pred,
  weights = NULL,
  multi = c("average", "sum"),
  grouped = TRUE,
  min.obs = 3,
  logger,
  return.raw = FALSE,
  confidence = 0.95,
  distribution = "normal",
  ...
)
}
\arguments{
\item{scorer}{a \linkS4class{Scorer} object}

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

\item{min.obs}{integer, minimum number of observations per list. If the number of
observations per list is less than \code{min.obs}, an attempt to compute errors at the
observation level (for the unlisted observations)
and then summarise is made (equivalent to \code{grouped = FALSE}). It is working only for certain
accuracy measures. Default is \code{min.obs = 3}. Set \code{min.obs = 0} to suppress.}

\item{logger}{a \linkS4class{Logger}}

\item{return.raw}{logical, whether to return scores used to compute estimate}

\item{...}{further arguments to scorer function}
}
\value{
list of 2 elements containing a score estimate and its standard error
}
\description{
Computes a score estimate given different sets of data, and returns the estimate
and the relative standard error
}
