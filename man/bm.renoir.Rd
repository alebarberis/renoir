% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/best_model_selection.R
\name{bm.renoir}
\alias{bm.renoir}
\title{Best model selection}
\usage{
\method{bm}{renoir}(
  object,
  best.config = c("min", "1se"),
  type.measure = c("mse", "deviance", "class", "auc", "mae", "C"),
  tolerance = 0,
  set = c("test.set", "train.set", "full.set")
)
}
\arguments{
\item{object}{a \code{renoir} object}

\item{best.config}{which configuration to consider}

\item{type.measure}{the performance measure to consider for the selection}

\item{tolerance}{the new tolerance to use for the selection of the best model}

\item{set}{which set of assessment data to consider for the selection of the best model.
The available options are \code{'test.set'} (default), \code{'train.set'} and \code{'full.set'}
(which consists of \code{test.set + train.set})}
}
\value{
an object of class \code{renoir.best} containing different elements.
\describe{
  \item{\code{model}}{the best \code{renoir.trained} model}
  \item{\code{assess}}{a list of length 2
       \describe{
           \item{\code{ci}}{a list of confidence intervals for the mean value of each performance measure}
           \item{\code{measures}}{a list of performance measures, where each element is a vector of measures}
       }
  }
  \item{\code{precruit}}{a data frame containing the probability of recruitment for each feature of the model}
  \item{\code{importance}}{the features importance}
  \item{\code{resampling.method}}{the resampling method}
  \item{\code{itrain}}{a vector containing the observations indices for the resampling iteration}
  \item{\code{best.config}}{the considered configuration}
  \item{\code{measure}}{the performance measure considered for the selection}
  \item{\code{resp.type}}{the response type}
  \item{\code{set}}{the set of data used for the computation of the accuracy measures
  considered for the selection of the best training set size and then the best model}
  \item{\code{tolerance}}{the tolerance used in the selection of the best model}
  \item{\code{confidence}}{the confidence used in the computation of the confidence intervals for the mean
  resampling measure of accuracy. The confidence limits are used in the selection
  of the best training set size}
  \item{\code{call}}{(optional) the arguments of the call that produced this object}
}
}
\description{
This function extracts the best model from a \code{renoir} object
given the configuration and the type measure to consider.
}
\keyword{internal}
