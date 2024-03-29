% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Trainer.R
\name{renoir_gknn}
\alias{renoir_gknn}
\title{Generalized k-Nearest Neighbors Classification or Regression}
\usage{
renoir_gknn(
  x,
  y = NULL,
  weights = NULL,
  offset = NULL,
  resp.type,
  observations = NULL,
  features = NULL,
  clean = FALSE,
  keep.call = TRUE,
  ...,
  k = 1
)
}
\arguments{
\item{x}{the input matrix, where rows are observations and columns are variables}

\item{y}{the response variable. Its number of rows must match the number of rows of \code{x}}

\item{weights}{priors of the observations}

\item{offset}{used only for GLM methods, it is an a priori known component to be included in the linear predictor during fitting}

\item{resp.type}{the response type}

\item{observations}{indices of observations to keep}

\item{features}{indices of predictors to keep}

\item{...}{further arguments to \code{\link[e1071]{gknn}}}

\item{k}{number of neighbours considered}
}
\value{
An object of class \linkS4class{Trained}
}
\description{
Generalized k-Nearest Neighbors Classification or Regression
}
\author{
Alessandro Barberis
}
