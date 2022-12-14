% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Renoir.R
\docType{class}
\name{Renoir-class}
\alias{Renoir-class}
\title{Renoir Class}
\description{
An S4 class representing the evaluation of a learning method by repeated sampling.
}
\section{Slots}{

\describe{
\item{\code{id}}{a character string}

\item{\code{config}}{a character string}

\item{\code{response}}{a character string}

\item{\code{nout}}{a one-length integer vector}

\item{\code{grid}}{a integer vector}

\item{\code{k}}{a one-length integer vector}

\item{\code{sampling}}{a character string}

\item{\code{learning}}{a character string}

\item{\code{screening}}{a character string}

\item{\code{scoring}}{a character string}

\item{\code{filter}}{a \linkS4class{Filtered} object}

\item{\code{evaluation}}{a \linkS4class{EvaluatedList} object}

\item{\code{stability}}{a list object}

\item{\code{marks}}{a \linkS4class{MarkedList} object}

\item{\code{nbest}}{a data.frame object}

\item{\code{call}}{list}
}}

\seealso{
\code{\link{Renoir}}
}
\author{
Alessandro Barberis
}
