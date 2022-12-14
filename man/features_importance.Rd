% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recruit.R
\name{features_importance}
\alias{features_importance}
\title{Features Importance}
\usage{
features_importance(
  object,
  best.config = c("min", "1se"),
  recruitment.term = c("coefficient", "presence"),
  set = c("test.set", "full.set", "train.set")
)
}
\arguments{
\item{object}{a list of \code{renoir.tat} objects}

\item{best.config}{which configuration to consider}

\item{recruitment.term}{whether to use a binary variable (\code{presence = 0} or \code{presence = 1})
multiplied by the sign of the coefficient, or to use directly the coefficient of a feature
in the computation of the significance score.}

\item{set}{the set of data used for the computation of the accuracy measures. For example,
if \code{set = "test.set"} the accuracy measures to be used in the computation of the
features importance are the ones calculated on the test set.}
}
\value{
For single response, it is a list of vectors, where each vector is associate to an accuracy measure, and contains
measures of significance for each feature. For multi-response, it is a list of lists.
}
\description{
This function computes a significance score for each feature.
}
\section{Details}{

A feature significance score is computed as the sum of the normalised
coefficient of the feature in the model multiplied by a weight for such model.
The formula for the i-th feature is:

\deqn{score_{i} = \frac{1}{s}\sum_{j=1}^{n} weight_{j}*coefficient_{j} ,}

where \eqn{n} is the total number of models computed across the different
training set sizes, \eqn{coefficient_{j}} is the normalised coefficient
(i.e. the coefficient of the feature divided by the maximum absolute value
of the coefficients of the features for that model so as to have a
value ranging \eqn{[-1,1]}), and \eqn{s} is the sum of weights.
The \eqn{weight} for a model is obtained by dividing
the multiplicative inverse of the squared prediction error of the model by
the max value across all models, so that models with better prediction
accuracy have higher weight:

\deqn{weight_{j} = \frac{w_{j}}{w_{best}},}

where \eqn{w_{j}} is the multiplicative inverse of the squared prediction error of
the model:

\deqn{w_{j} = \frac{1}{err_{j}^2},}

and

\deqn{w_{best} = \max{w_{1},...,w_{j},...,w_{n}}.}

Note that for the \code{auc} accuracy measure, we consider the prediction error
as \eqn{err_{j} = 1 - auc_{j}}.

By selecting \code{use = "presence"}, the \eqn{coefficient_{j}}
term in the computation of the significance score is exchanged
for \eqn{presence_{j}*sign_{j}}, where \eqn{presence_{j}} stands for the
presence of the feature in the j-th model and \eqn{sign_{j}}
is the sign of the coefficient of the i-th feature in the j-th model.
}

