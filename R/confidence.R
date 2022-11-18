#'Confidence Interval
#Confidence interval of estimate
#'@description This function computes the confidence interval of
#'a point estimate.
#'
#'@details
#'The confidence interval provides additional information about the
#'variability of a point estimate and it is generally defined as
#'
#'\deqn{CI = estimate \pm \textit{margin of error} = estimate \pm \textit{critical value} \times \textit{standard error}}
#'
#'where \eqn{\textit{estimate}} is the sample statistic estimating the population parameter of interest;
#'\eqn{\textit{critical value}} is a value based on the sampling distribution of the estimate
#'and the desired confidence level; \eqn{\textit{standard error}} is the standard deviation of the
#'point estimate.
#'
#'Since estimates from different samples from the same population can be different, the estimate
#'can be seen as a random variable with its own distribution.
#'Given that we are not interested in the estimate for a specific sample but we rather
#'want to draw conclusions about the population, the confidence interval can be used to
#'obtain information about the population parameter.
#'
#'Beware about the interpretation. The CI is not telling us if the population parameter is inside its range.
#'In fact, as generally we don't know the population parameter, we don't know if the interval
#'contains it. However, we know that a certain number of intervals (our confidence level, e.g. 95\%)
#'formed in this way will contain the parameter.
#'
#'@param estimate sample statistic from population having distribution \code{distribution}
#'@param se standard error (standard deviation of \code{estimate})
#'@param confidence the desired confidence level
#'@param distribution sampling distribution of the estimate.
#'Use \code{normal} if the population has unknown mean and known variance (or if \code{n} is large),
#'\code{t} if population has unknown mean and variance
#'@param n sample size, used to compute the degrees of freedom if \code{distribution = "t"}
#'
#'@return A named numeric vector with 2 elements, \code{low} and \code{up},
#'the lower and upper bounds of the confidence interval
#'for the point estimate.
#'
#'@references \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5723800/}
#@family quantifying uncertainty
#'@author Alessandro Barberis
ci <- function(estimate, se, confidence = 0.95, distribution = c("normal", "t"), n){

  #alpha
  alpha = (1 - confidence) / 2;

  #distribution
  distribution = match.arg(distribution)

  #critical value
  if(identical(distribution, "normal")){
    Q <- stats::qnorm(p = 1 - alpha, lower.tail = TRUE);
  } else if(identical(distribution, "t")){
    Q <- stats::qt(p = 1 - alpha, df = n - 1, lower.tail = TRUE);
  }

  #Compute the interval
  interval <- c(low = estimate - Q*se, up = estimate + Q*se )

  #return
  return(interval);
}


#'Confidence Interval for the Weighted Mean
#'@description This function computes the confidence interval (CI) for the
#'weighted sample mean.
#'
#'@details
#'The confidence interval of the weighted sample mean provides additional information about the
#'variability of the population parameter estimate. It is defined as
#'
#'\deqn{CI = mean \pm \textit{critical value} \times \textit{standard error of the mean (SEM)}}
#'
#'where \eqn{mean = \frac{\sum_{i=1}^{n} w_{i} x_{i}}{\sum_{i=1}^{n} w_{i}}} is the weighted mean.
#'
#'
#'@param x vector of measurements
#'@param weights vector of weights
#'@param ... further arguments to \link{sewm}
#'@inheritParams ci
#'
#'@return \code{list} containing three elements
#'\describe{
#' \item{m}{sample mean}
#' \item{sem}{standard error of the mean}
#' \item{ci}{confidence interval for the sample mean}
#'}
#'
#@family quantifying uncertainty
#'
#'@seealso \code{\link{ci()}} for more information on confidence intervals, \code{\link{sewm()}} for the standard error
#'of the weighted sample mean.
#'
#'@references \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5723800/}
#'@author Alessandro Barberis
ciwm <- function(x, weights, ..., confidence = 0.95, distribution = "normal", n){

  #--------------------------------------------------------------------------------------------#
  #get sample size
  if(missing(n)){n = length(x)}

  #--------------------------------------------------------------------------------------------#
  #check weights
  if(missing(weights) || is.null(weights)){weights = rep(1, n)}

  #--------------------------------------------------------------------------------------------#
  #compute the weighted sample mean
  m = stats::weighted.mean(x = x, w = weights, na.rm = TRUE)

  #--------------------------------------------------------------------------------------------#
  #compute the standard error of the mean (weighted standard deviation)
  sem = sewm(x = x, weights = weights, ...)

  #--------------------------------------------------------------------------------------------#
  #compute confidence interval
  cim = ci(estimate = m, se = sem, confidence = confidence, distribution = distribution, n = n)

  #--------------------------------------------------------------------------------------------#
  #create output
  out = list(m = m, sem = sem, ci = cim)

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}

#'Standard Error of Weighted Mean
#'@description Computes the Standard Error of the weighted sample mean.
#'
#'@details
#'The standard error of the sample mean is its standard deviation, i.e.
#'the square root of the variance of the estimate
#'
#'\deqn{SEM = \hat{\sigma}_{W}}
#'
#'Be aware that while stratification during sampling is not affecting the
#'estimate of the mean, it may affect its standard error.
#'
#'Different options for the calculation of the variance are here provided.
#'
#'@inheritParams ciwm
#'@param weights vector of weights
#'@param var.est a character string, the estimator for the variance of the weighted mean
#'@param ... further arguments to \code{var.est}
#'
#'@return A length-one numeric vector.
#'
#'@seealso \code{\link{uevarwm()}} for more information on the default estimate
#'of the variance.
#'
#@references
#'
#@family quantifying uncertainty
#'@author Alessandro Barberis
sewm <- function(x, weights, var.est = "default", ...){
# sewm <- function(x, w, var.est = c("biased", "unbiased")){

  #compute variance
  out = switch(
    var.est,
    'default' = uevarwm(x = x, weights = weights, ...)
  )

  #compute standard deviation
  out = sqrt(x = out)

  #return
  return(out)
}

#'Unbiased Variance of the Weighted Mean
#'@description Computes an unbiased estimate of the variance of the weighted sample mean.
#'
#'@details
#'Currently, this implementation is not considering eventual stratification
#'in the estimate of the variance. We also assume that the measurements
#'(i.e. \code{x}) are drawn from the same population with variance
#'\eqn{V(x_{i}) = \sigma_{x}^2}.
#'
#'The variance of the sample mean is then computed as
#'
#'\deqn{\hat{\sigma}_{W}^{2} = V(\frac{\sum_{i=1}^{n} w_{i}x_{i}}{\sum_{i=1}^{n} w_{i}})
#'= \frac{1}{(\sum_{i=1}^{n} w_{i})^2} V(\sum_{i=1}^{n} w_{i}x_{i})
#'= \frac{\sum_{i=1}^{n} w_{i}^2}{(\sum_{i=1}^{n} w_{i})^2} \sigma_{x}^2}
#'
#'There are different ways to estimate the variance of the \code{x} (\eqn{\sigma_{x}^2}). Here we
#'use the common formula for unbiased estimator of unweighted data
#'
#'\deqn{s_{x}^2 = \frac{\sum_{i=1}^{n} (x_{i} - \bar{x})^2}{n - 1}}
#'
#'@inheritParams sewm
#'@param na.rm logical, whether \code{NA} values in \code{x} should be stripped before the computation proceeds

#'@return A length-one numeric vector.
#'
#'@author Alessandro Barberis
uevarwm <- function(x, weights, na.rm = T){

  #Number of samples
  n = length(x);

  #check weights
  if(missing(weights) || is.null(weights)){weights = rep(1, n)}

  if (length(weights) != length(x)){
    stop("'x' and 'weights' must have the same length")
  }

  if (na.rm) {
    keep <- !is.na(x)
    weights <- weights[keep]
    x <- x[keep]

    #update size
    n = length(x)
  }

  #compute mean
  m = mean(x = x, na.rm = na.rm)

  #compute variance
  s = sum((x - m)^2)/(n - 1)

  #effective sample size
  Neff = (sum(weights))^2/(sum(weights^2))

  #unbiased variance for the weighted mean
  out = s/Neff

  #return
  return(out)
}
