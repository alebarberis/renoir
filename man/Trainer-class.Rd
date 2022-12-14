% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Trainer.R
\docType{class}
\name{Trainer-class}
\alias{Trainer-class}
\title{Trainer Class
An S4 class representing a learning method.}
\description{
The object consists of 4 slots
}
\section{Slots}{

\describe{
\item{\code{id}}{the learning method name}

\item{\code{trainer}}{trainer function.
It must have the following formal arguments:
\describe{
   \item{x}{the input matrix, where rows are observations and columns are variables.}
   \item{y}{the response variable. Its number of rows must match the number of rows of \code{x}.}
   \item{weights}{priors of the observations}
   \item{offset}{used only for GLM methods, it is an a priori known component to be included in the linear predictor during fitting}
   \item{resp.type}{the response type}
   \item{observations}{indices of observations to keep}
   \item{features}{indices of predictors to keep}
   \item{...}{additional arguments}
}}

\item{\code{parameters}}{list containing the parameters for the chosen learning method}

\item{\code{logger}}{a \linkS4class{Logger}}
}}

\author{
Alessandro Barberis
}
