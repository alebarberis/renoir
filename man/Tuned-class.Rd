% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Tuned.R
\docType{class}
\name{Tuned-class}
\alias{Tuned-class}
\title{Tuned Class}
\description{
An S4 class providing a container for the results of a tuning procedure.
}
\section{Slots}{

\describe{
\item{\code{model}}{a \linkS4class{Trained} object}

\item{\code{config}}{a character string}

\item{\code{mscore}}{a length-one numeric vector}

\item{\code{sem}}{a length-one numeric vector}

\item{\code{tuning}}{a character string}

\item{\code{sampling}}{a character string}

\item{\code{measure}}{a character string}

\item{\code{screened}}{a \linkS4class{Screened} object}

\item{\code{xvars}}{a length-one integer vector}

\item{\code{xobs}}{a length-one integer vector}

\item{\code{index}}{a length-one integer vector}

\item{\code{trained}}{a list}

\item{\code{validated}}{a list}

\item{\code{stability}}{a list}
}}

\author{
Alessandro Barberis
}
