% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/best_model_selection.R
\name{bm.renoir.tat}
\alias{bm.renoir.tat}
\title{Best model selection}
\usage{
\method{bm}{renoir.tat}(
  object,
  best.config = c("min", "1se"),
  type.measure = c("mse", "deviance", "class", "auc", "mae", "C"),
  set = c("test.set", "train.set", "full.set")
)
}
\arguments{
\item{object}{a \code{renoir.tat} object}

\item{best.config}{which configuration to consider}

\item{type.measure}{the performance measure to consider for the selection}

\item{set}{which set of assessment data to consider for the selection of the best model
(\code{'test.set'} by default)}
}
\value{
a list of length 2, containing
\describe{
  \item{\code{trained}}{the best \code{renoir.trained} model}
  \item{\code{itrain}}{a vector containing the observations indices for the resampling iteration}
}
}
\description{
This function extracts the best model from a \code{renoir.tat} object
given the configuration and the type measure to consider.
}
\keyword{internal}
