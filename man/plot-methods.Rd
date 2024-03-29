% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Renoir.R
\name{plot,Renoir,missing-method}
\alias{plot,Renoir,missing-method}
\alias{plot}
\title{Plot method for \code{Renoir} object}
\usage{
\S4method{plot}{Renoir,missing}(x, y, ..., key, measure, set, interactive = FALSE)
}
\arguments{
\item{x}{an object of class \linkS4class{Renoir}}

\item{y}{unused parameter}

\item{...}{optional graphical parameters to \code{\link{plot_evaluation}} (if
\code{interactive = FALSE}) or \code{\link{plotly_evaluation}} (if
\code{interactive = TRUE})}

\item{key}{a character vector indicating how to group elements}

\item{measure}{the performance metric to plot}

\item{set}{the data set to consider (\code{train}, \code{test} or \code{full})}

\item{interactive}{logical, whether to draw an interactive plot}
}
\value{
An object of class \code{ggplot} (if \code{interactive = FALSE})
or \code{plotly} (if \code{interactive = TRUE})
}
\description{
This function produces a performance evaluation plot
for a \linkS4class{Renoir} object.
}
\details{
A plot showing the mean performance and the related
95\\% confidence interval of a learning method
across different training-set sizes is produced.
}
\seealso{
\code{\link{plot_evaluation}},
\code{\link{plot_single_evaluation}},
\code{\link{plot_multi_evaluation}},
\code{\link{plotly_evaluation}},
\code{\link{plotly_single_evaluation}},
\code{\link{plotly_multi_evaluation}}
}
\author{
Alessandro Barberis
}
