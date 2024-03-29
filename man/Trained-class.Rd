% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Trained.R
\docType{class}
\name{Trained-class}
\alias{Trained-class}
\alias{Trained}
\title{An S4 class to represent a renoir trained model}
\usage{
Trained(
  fit = NULL,
  config = list(),
  learning.method = character(),
  nfeatures = integer()
)
}
\description{
The studies object consists of eight slots

Constructor for the S4 \linkS4class{Trained} object.
}
\section{Slots}{

\describe{
\item{\code{fit}}{The trained object (can be of different class depending on the chosen learning method)}

\item{\code{learning.method}}{The used learning method}

\item{\code{config}}{A list containing the tested hyper-parameter configurations}

\item{\code{np}}{a length-one integer vector}
}}

