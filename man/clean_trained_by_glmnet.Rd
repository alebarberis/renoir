% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_Trained.R
\name{clean_trained_by_glmnet}
\alias{clean_trained_by_glmnet}
\title{Clean glmnet objects}
\usage{
clean_trained_by_glmnet(object, rm.call = F)
}
\arguments{
\item{object}{a \linkS4class{Trained} object}
}
\value{
the cleaned \linkS4class{Trained} object
}
\description{
This function remove the \code{x} from the \code{call}
slots of the trained object. It is needed to avoid that the final
object's size raises too much when the initial set of features and observations is big.
}
\keyword{internal}
