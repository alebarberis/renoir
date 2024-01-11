
#'Multi-response error
#'
#'@description If error is from multi-response data, this function allows to adopt different strategies.
#'
#'@param error vector or matrix containing accuracy measures
#'@param weights response weights
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#'   \item{\code{raw}}{returns a vector containing one error for each output}
#'}
#'
#'@return the error after applied the selected strategy
#'@author Alessandro Barberis
multiresponse_error <- function(error, weights = NULL, multi = c("average", "sum", "raw")){

  #get N
  N = ncol(error)

  #check if multi-response
  is.multi = !is.null(N)

  if(is.multi){

    if(N > 1){
      #multi
      multi = match.arg(multi)

      #check weights
      if(missing(weights) || is.null(weights)) {weights = rep(1, N)}

      if(identical(multi, "raw")){
        #Apply the weights
        error = sweep(x = error, MARGIN = 2, STATS = weights, FUN = `*`)
      }else{
        if(identical(multi, "sum")){
          #Apply the weights
          error = sweep(x = error, MARGIN = 2, STATS = weights, FUN = `*`)
          #sum
          error = rowSums(x = error, na.rm = T)
        } else {
          #average
          # acc.raw = rowMeans(x = acc.raw, na.rm = T)
          error = apply(X = error, MARGIN = 1, FUN = stats::weighted.mean, w = weights, na.rm = TRUE)
        }
      }
    } else {
      #make sure output is vector
      error = stats::setNames(object = as.vector(error), nm = rownames(error))
    }
  }

  return(error)
}


#'Absolute Error
#'
#'@description
#'This function computes the absolute error.
#'
#'@details
#'The absolute error (AE) is based on absolute values and it is defined as
#'
#'\deqn{AE(y_{i},\hat{y}_{i}) = \lvert y_{i} - \hat{y}_{i} \rvert}
#'
#'where \eqn{y_{i}} is the true value of the \eqn{i}-th sample and \eqn{\hat{y}_{i}}
#'is the predicted value.
#'
#'If the observed data has multiple output values and weights are provided, then the error is weighted.
#'
#'\deqn{wAE(w,y,\hat{y}) = \frac{1}{\sum_{j=1}^{n} w_{j}} w * \lvert y_{i} - \hat{y}_{i} \rvert}
#'
#'where \eqn{w_{j}} is the weighting factor assigned to the \eqn{j}-th response.
#'The best possible score is zero.
#'
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'@param weights observation weights. This argument is for consistency, but not currently used.
#'@param rweights response weights
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#'   \item{\code{raw}}{returns a vector containing one error for each output}
#'}
#'
#'@return the absolute error, a vector of positive \code{double} values or a matrix,
#'having one column for each response, if response has multiple output values and \code{multi = "raw"}
#'
#'@author Alessandro Barberis
absolute_error <- function(true, pred, weights = NULL, rweights = NULL, multi = c("average", "sum", "raw")){

  #Raw accuracy
  acc = abs(true - pred)

  #multi
  multi = match.arg(multi)

  #correct if multi
  acc = multiresponse_error(error = acc, weights = rweights, multi = multi)

  #return
  return(acc)
}


#'Mean Absolute Error
#'
#'@description
#'This function computes the mean absolute error.
#'
#'@details The mean absolute error (MAE) is a measure of errors based on absolute values.
#'It treats small and large errors equally so it is less sensitive to outliers compared
#'to other measures (e.g. MSE).
#'The MAE estimated over n observations is defined as
#'
#'\deqn{MAE(y,\hat{y}) = \frac{1}{n}\sum_{i=1}^{n} \lvert y_{i} - \hat{y}_{i} \rvert}
#'
#'where \eqn{y_{i}} is the true value of the \eqn{i}-th sample and \eqn{\hat{y}_{i}}
#'is the predicted value.
#'If observation weights are provided, then the weighted mean absolute error is computed as
#'
#'\deqn{wMAE(w,y,\hat{y}) = \frac{1}{\sum_{i=1}^{n} w_{i}}\sum_{i=1}^{n} w_{i} * \lvert y_{i} - \hat{y}_{i} \rvert}
#'
#'where \eqn{w_{i}} is the weighting factor assigned to the \eqn{i}-th observation.
#'The best possible score is zero.
#'
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'@param weights observation weights
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#'   \item{\code{raw}}{returns a vector containing one error for each output}
#'}
#'@param rweights response weights
#'
#'@return the mean absolute error, a positive \code{double} or a vector of positive \code{double} values,
#'one for each response, if response has multiple output values and \code{multi = "raw"}
#'
#'@author Alessandro Barberis
mean_absolute_error <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw"), rweights = NULL){

  #multi
  multi = match.arg(multi)

  #get N
  N = if(!is.null(nrow(true))){nrow(true)} else {length(true)}

  #check weights
  if(missing(weights) || is.null(weights)) {weights = rep(1, N)}

  #Raw accuracy
  acc.raw = absolute_error(true = true, pred = pred, rweights = rweights, multi = multi)

  #Apply the weighted mean
  # acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)

  #check dimension
  if(is.null(dim(acc.raw))){acc.raw = stats::setNames(object = data.frame(acc.raw, check.names = F, fix.empty.names = F, stringsAsFactors = F), nm = NULL)}

  #Apply the weighted mean
  acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)#to use when multi-config

  #return
  return(acc)
}

#'Absolute Percentage Error
#'
#'@description
#'This function computes the absolute percentage error.
#'
#'@details
#'The absolute percentage error (APE) is based on absolute values.
#'The absolute percentage error is defined as
#'
#'\deqn{APE(y,\hat{y}) = \frac{\lvert y_{i} - \hat{y}_{i} \rvert}{\max{\lvert y_{i} \rvert, \epsilon}}}
#'
#'where \eqn{y_{i}} is the true value of the \eqn{i}-th sample, \eqn{\hat{y}_{i}}
#'is the predicted value, and \eqn{\epsilon} is an arbitrary positive small number used
#'to avoid a division by zero.
#'
#'If response has multiple output values and weights are provided, then the error is weighted.
#'
#'\deqn{wAPE(w,y,\hat{y}) = \frac{w * \lvert y_{i} - \hat{y}_{i} \rvert}{\max{\lvert y_{i} \rvert, \epsilon}}}
#'
#'where \eqn{w} is a vector of \eqn{n} elements, \eqn{w_{j}} is the weighting factor assigned to the \eqn{j}-th response.
#'The best possible score is zero.
#'
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'@param rweights response weights
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#'   \item{\code{raw}}{returns a vector containing one error for each output}
#'}
#'
#'@return the absolute percentage error, a vector of positive \code{double} values or a matrix,
#'having one column for each response, if response has multiple output values and \code{multi = "raw"}
#'
#'@author Alessandro Barberis
absolute_percentage_error <- function(true, pred, weights = NULL, rweights = NULL, multi = c("average", "sum", "raw"), eps = 1e-10){

  #Raw accuracy
  acc = abs(true - pred) / pmax(abs(true), eps)

  #multi
  multi = match.arg(multi)

  #correct if multi
  acc = multiresponse_error(error = acc, weights = rweights, multi = multi)

  #return
  return(acc)
}


#'Mean Absolute Percentage Error
#'
#'@description
#'This function computes the mean absolute percentage error.
#'
#'@details
#'The mean absolute percentage error (MAPE) is a measure of errors based on relative absolute values.
#'The MAPE estimated over *n* observations is defined as
#'
#'\deqn{MAPE(y,\hat{y}) = \frac{1}{n}\sum_{i=1}^{n} \frac{\lvert y_{i} - \hat{y}_{i} \rvert}{\max{\lvert y_{i} \rvert, \epsilon}}}
#'
#'where \eqn{y_{i}} is the true value of the \eqn{i}-th sample, \eqn{\hat{y}_{i}}
#'is the predicted value, and \eqn{\epsilon} is an arbitrary positive small number used
#'to avoid a division by zero.
#'
#'If observation weights are provided, then the weighted mean absolute percentage error is computed as
#'
#'\deqn{wMAPE(w,y,\hat{y}) = \frac{1}{\sum_{i=1}^{n} w_{i}}\sum_{i=1}^{n} w_{i} * \frac{\lvert y_{i} - \hat{y}_{i} \rvert}{\max{\lvert y_{i} \rvert, \epsilon}}}
#'
#'where \eqn{w_{i}} is the weighting factor assigned to the \eqn{i}-th observation.
#'The best possible score is zero.
#'
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'@param weights observation weights
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#'   \item{\code{raw}}{returns a vector containing one error for each output}
#'}
#'@param eps an arbitrary positive small number, used to correct the MAPE formula and avoid the division by zero
#'@param rweights response weights
#'@return the mean absolute percentage error, a positive \code{double} or a vector of positive \code{double} values,
#'one for each response, if response has multiple output values and \code{multi = "raw"}
#'@author Alessandro Barberis
mean_absolute_percentage_error <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw"), eps = 1e-10, rweights = NULL){

  #multi
  multi = match.arg(multi)

  #Raw accuracy
  acc.raw = absolute_percentage_error(true = true, pred = pred, rweights = rweights, multi = multi, eps = eps)

  #get N
  N = if(!is.null(nrow(true))){nrow(true)} else {length(true)}

  #check weights
  if(missing(weights) || is.null(weights)) {weights = rep(1, N)}

  #Apply the weighted mean
  # acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)

  #check dimension
  if(is.null(dim(acc.raw))){acc.raw = stats::setNames(object = data.frame(acc.raw, check.names = F, fix.empty.names = F, stringsAsFactors = F), nm = NULL)}

  #Apply the weighted mean
  acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)#to use when multi-config

  #return
  return(acc)
}

squared_error <- function(true, pred, weights = NULL, rweights = NULL, multi = c("average", "sum", "raw"), root = FALSE){

  #Raw accuracy
  acc = (true - pred)^2

  #multi
  multi = match.arg(multi)

  #correct if multi
  acc = multiresponse_error(error = acc, weights = rweights, multi = multi)

  #root
  if(root){
    acc = sqrt(acc)
  }

  #return
  return(acc)
}

#'Mean Squared Error
#'
#'@description
#'This function computes the mean squared error.
#'
#'@details
#'The mean squared error (MSE) is a measure of errors based on squared losses.
#'Small and large errors are not treated equally, as the large errors are more heavily
#'weighted than small ones thus skewing the measure.
#'It is more sensitive to outliers compared to other measures (e.g. MAE).
#'The MSE estimated over *n* observations is defined as
#'
#'\deqn{MSE(y,\hat{y}) = \frac{1}{n}\sum_{i=1}^{n} (y_{i} - \hat{y}_{i})^{2}}
#'
#'where \eqn{y_{i}} is the true value of the \eqn{i}-th sample and \eqn{\hat{y}_{i}}
#'is the predicted value.
#'If observation weights are provided, then the weighted mean absolute error is computed as
#'
#'\deqn{wMSE(w,y,\hat{y}) = \frac{1}{\sum_{i=1}^{n} w_{i}}\sum_{i=1}^{n} w_{i} * (y_{i} - \hat{y}_{i})^{2}}
#'
#'where \eqn{w_{i}} is the weighting factor assigned to the \eqn{i}-th observation.
#'
#'The best possible score is zero.
#'
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'@param weights observation weights
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#'   \item{\code{raw}}{returns a vector containing one error for each output}
#'}
#'@param rweights response weights
#'@param root logical, whether to return the MSE or the root mean squared error (RMSE)
#'
#'@return the mean squared error, a positive \code{double} or a vector of positive \code{double} values,
#'one for each response, if response has multiple output values and \code{multi = "raw"}
#'
#'@author Alessandro Barberis
mean_squared_error <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw"), root = FALSE, rweights = NULL){

  #multi
  multi = match.arg(multi)

  #get N
  N = if(!is.null(nrow(true))){nrow(true)} else {length(true)}

  #check weights
  if(missing(weights) || is.null(weights)){weights = rep(1, N)}

  #Raw accuracy
  acc.raw = squared_error(true = true, pred = pred, rweights = rweights, multi = multi, root = FALSE)

  #Apply the weighted mean
  # acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)

  #check dimension
  if(is.null(dim(acc.raw))){acc.raw = stats::setNames(object = data.frame(acc.raw, check.names = F, fix.empty.names = F, stringsAsFactors = F), nm = NULL)}

  #Apply the weighted mean
  acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)#to use when multi-config

  #root
  if(root){
    acc = sqrt(acc)
  }

  #return
  return(acc)
}


root_squared_error <- function(true, pred, weights, multi, ...){
  acc = squared_error(true = true, pred = pred, weights = weights, multi = multi, root = TRUE, ...)
  return(acc)
}


#'Root-Mean-Square Error
#'
#'@description
#'This function computes the root-mean-square error.
#'
#'@details
#'The root-mean-square error (RMSE) is a measure of errors based on squared losses.
#'It is defined as the squared root of the \link{mean_squared_error}:
#'
#'\deqn{RMSE(y,\hat{y}) = \sqrt{MSE(y,\hat{y})} = \sqrt{\frac{1}{n}\sum_{i=1}^{n} (y_{i} - \hat{y}_{i})^{2}}}
#'
#'If observation weights are provided, then the weighted root mean squared error is computed as
#'
#'\deqn{wRMSE(w,y,\hat{y}) = \sqrt{wMSE(w,y,\hat{y})} = \sqrt{\frac{1}{\sum_{i=1}^{n} w_{i}}\sum_{i=1}^{n} w_{i} * (y_{i} - \hat{y}_{i})^{2}}}
#'
#'This function is a shortcut for \code{mean_squared_error(root = TRUE, ...)}.
#'@param ... arguments to \link{mean_squared_error}
#'
#'@return the root-mean-square error, a positive \code{double} or a vector of positive \code{double} values,
#'one for each response, if response has multiple output values and \code{multi = "raw"}
#'
#'@author Alessandro Barberis
root_mean_squared_error <- function(true, pred, weights, multi, ...){
  acc = mean_squared_error(true = true, pred = pred, weights = weights, multi = multi, root = TRUE, ...)
  return(acc)
}

squared_log_error <- function(true, pred, weights = NULL, rweights = NULL, multi = c("average", "sum", "raw"), root = FALSE){

  #Raw accuracy
  acc = (log(1 + true) - log(1 + pred))^2

  #multi
  multi = match.arg(multi)

  #correct if multi
  acc = multiresponse_error(error = acc, weights = rweights, multi = multi)

  #root
  if(root){
    acc = sqrt(acc)
  }

  #return
  return(acc)
}

#'Mean Squared Logarithmic Error
#'
#'@description
#'This function computes the mean squared logarithmic error.
#'
#'@details
#'The mean squared logarithmic error (MSLE) is a measure of errors based on squared
#'logarithmic losses. This metric is commonly preferred (e.g. over the MSE) when
#'\itemize{
#' \item Under-predicted estimates are more penalised than over-predicted ones.
#' \item Huge differences in the predicted and true values aren't to be penalised (when both are huge numbers).
#'}
#'
#'The MSLE estimated over n observations is defined as
#'
#'\deqn{MSLE(y,\hat{y}) = \frac{1}{n}\sum_{i=1}^{n} (log(1 + y_{i}) - log(1 + \hat{y}_{i}))^{2}}
#'
#'where \eqn{log} is the natural logarithm, \eqn{y_{i}} is the true value of the \eqn{i}-th sample
#'and \eqn{\hat{y}_{i}} is the predicted value.
#'If observation weights are provided, then the weighted mean absolute error is computed as
#'
#'\deqn{wMSLE(w,y,\hat{y}) = \frac{1}{\sum_{i=1}^{n} w_{i}}\sum_{i=1}^{n} w_{i} * (log(1 + y_{i}) - log(1 + \hat{y}_{i}))^{2}}
#'
#'where \eqn{w_{i}} is the weighting factor assigned to the \eqn{i}-th observation.
#'
#'The best possible score is zero.
#'
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'@param weights observation weights
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#'   \item{\code{raw}}{returns a vector containing one error for each output}
#'}
#'@param root logical, whether to return the MSLE or the root mean squared logarithmic error (RMSLE)
#'@param rweights response weights
#'
#'@return the mean squared error, a positive \code{double} or a vector of positive \code{double} values,
#'one for each response, if response has multiple output values and \code{multi = "raw"}
#'
#'@author Alessandro Barberis
mean_squared_log_error <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw"), root = FALSE, rweights = NULL){

  #get N
  N = if(!is.null(nrow(true))){nrow(true)} else {length(true)}

  #check weights
  if(missing(weights) || is.null(weights)){weights = rep(1, N)}

  #Raw accuracy
  acc.raw = squared_log_error(true = true, pred = pred, rweights = rweights, multi = multi, root = FALSE)

  #Apply the weighted mean
  # acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)

  #check dimension
  if(is.null(dim(acc.raw))){acc.raw = stats::setNames(object = data.frame(acc.raw, check.names = F, fix.empty.names = F, stringsAsFactors = F), nm = NULL)}

  #Apply the weighted mean
  acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)#to use when multi-config

  #root
  if(root){
    acc = sqrt(acc)
  }

  #return
  return(acc)
}


#'R squared
#'
#'@description
#'This function computes the coefficient of determination (R squared).
#'
#'@details
#'The coefficient of determination (R squared) is a measure of the proportion
#'of variance in the dependent variable that is explained by the independent variables.
#'The R2 estimated over n observations is defined as
#'
#'\deqn{R^{2}(y,\hat{y}) = 1 - \frac{\sum_{i=1}^{n} (y_{i} - \hat{y}_{i})^{2}}{\sum_{i=1}^{n} (y_{i} - \bar{y})^{2}}}
#'
#'where \eqn{\bar{y}} is the mean of the observed data, \eqn{y_{i}} is the true value of
#'the \eqn{i}-th sample, and \eqn{\hat{y}_{i}} is the predicted value.
#'
#'If observation weights are provided, then the weighted R squared is computed as
#'
#'\deqn{wR^{2}(w,y,\hat{y}) = 1 - \frac{\sum_{i=1}^{n} w_{i}*(y_{i} - \hat{y}_{i})^{2}}{\sum_{i=1}^{n} w_{i}*(y_{i} - \bar{y})^{2}}}
#'
#'where \eqn{w_{i}} is the weighting factor assigned to the \eqn{i}-th observation.
#'
#'The best possible score is one.
#'
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'@param weights observation weights
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#'   \item{\code{raw}}{returns a vector containing one error for each output}
#'}
#'@return the R squared score, a \code{double} or a vector of \code{double} values,
#'one for each response, if response has multiple output values and \code{multi = "raw"}
#'
#'@source See \href{https://en.wikipedia.org/wiki/Coefficient_of_determination}{Wiki page} for further information.
#'
#'@author Alessandro Barberis
r2_score <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw")){

  #get N
  N = if(!is.null(nrow(true))){nrow(true)} else {length(true)}

  #check weights
  if(missing(weights) || is.null(weights)){weights = rep(1, N)}

  #Residuals
  # acc.raw = weights * (true - pred)^2
  acc.raw = (true - pred)^2

  #check if multi-response
  is.multi = !is.null(dim(acc.raw))

  #compute mean of observed data
  if(is.multi){
    m =  apply(true, 2, stats::weighted.mean, w = weights, na.rm = TRUE)
  } else {
    m = stats::weighted.mean(x = true, w = weights, na.rm = TRUE)
  }

  #squared differences between observations and their overall mean
  # s = weights * (true - m)^2
  s = (true - m)^2

  #If multi-response, sum the residues
  if(is.multi){
    if(identical(multi, "raw")){
      #Compute R2
      acc = 1 - colSums(weights * acc.raw)/colSums(weights * s)
    }else{
      if(identical(multi, "sum")){
        #sum
        acc.raw = rowSums(x = acc.raw, na.rm = T)
        s       = rowSums(x = s, na.rm = T)
      } else {
        #average
        acc.raw = rowMeans(x = acc.raw, na.rm = T)
        s       = rowMeans(x = s, na.rm = T)
      }
      #Compute R2
      acc = 1 - sum(weights * acc.raw)/sum(weights * s)
    }
  } else {
    #Compute R2
    acc = 1 - sum(weights * acc.raw)/sum(weights * s)
  }

  #return
  return(acc)
}
























#--------------------------------------------------------------------------------------------#
#Actual condition
P <- function(TP, TN, FP, FN){out = TP + FN;return(out)}
N <- function(TP, TN, FP, FN){out = FP + TN;return(out)}
#Predicted condition
PP <- function(TP, TN, FP, FN){out = TP + FP;return(out)}
PN <- function(TP, TN, FP, FN){out = FN + TN;return(out)}

#--------------------------------------------------------------------------------------------#
#'False positive rate, probability of false alarm, fall-out, (1 - TNR)
#'@description This function computes the false positive rate.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
FPR <- function(TP, TN, FP, FN){out = FP / (FP + TN);return(out)}
#'True negative rate, specificity (SPC), selectivity, (1 - FPR)
#'@description This function computes the true negative rate.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
TNR <- function(TP, TN, FP, FN){out = TN / (FP + TN);return(out)}
#'False negative rate, miss rate, (1 - TPR)
#'@description This function computes the false negative rate.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
FNR <- function(TP, TN, FP, FN){out = FN / (TP + FN);return(out)}
#'True Positive Rate
#'@description This function computes the true positive rate. It is also known as
#'recall, sensitivity, probability of detection, power, hit rate (1 - FNR).
#'
#'@details The true positive rate measures the ability of a classifier
#'of correctly predicting the presence of a condition.
#'It is defined as
#'
#'\deqn{sensitivity = recall = \textit{true positive rate} (TPR) = \frac{TP}{P} = \frac{TP}{TP + FN} = 1 - FNR}
#'
#'The optimal value is 1 and the worst value is 0.
#'
#'@param TP number of true positives
#'@param TN number of true negatives
#'@param FP number of false positives
#'@param FN number of false negatives
#'
#'@return A numeric value, the true positive rate.
#'
#'@author Alessandro Barberis
#'@export
TPR <- function(TP, TN, FP, FN){out = TP / (TP + FN);return(out)}
SEN <- TPR

#'False discovery rate (1 - PPV)
#'@description This function computes the false discovery rate
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
FDR <- function(TP, TN, FP, FN){out = FP / (TP + FP);return(out)}

#'Positive predictive value, precision (1 - FDR)
#'@description This function computes the positive predictive value.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
PPV <- function(TP, TN, FP, FN){out = TP / (TP + FP);return(out)}

#'False omission rate (1 - NPV)
#'@description This function computes the false omission rate.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
FOR <- function(TP, TN, FP, FN){out = FN / (FN + TN);return(out)}
#'Negative predictive value (1 - FOR)
#'@description This function computes the negative predictive value.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
NPV <- function(TP, TN, FP, FN){out = TN / (FN + TN);return(out)}

#--------------------------------------------------------------------------------------------#
#Likelihood

#'Positive likelihood ratio (LR+)
#'@description This function computes the positive likelihood ratio.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
PLR <- function(TP, TN, FP, FN){out = TPR(TP, TN, FP, FN) / FPR(TP, TN, FP, FN);return(out)}
#'Negative likelihood ratio (LR-)
#'@description This function computes the negative likelihood ratio.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
NLR <- function(TP, TN, FP, FN){out = FNR(TP, TN, FP, FN) / TNR(TP, TN, FP, FN);return(out)}

#--------------------------------------------------------------------------------------------#
#'Markedness (MK),deltaP
#'@description This function computes the markedness.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
MKD <- function(TP, TN, FP, FN){out = PPV(TP, TN, FP, FN) + NPV(TP, TN, FP, FN) - 1;return(out)}
#'Diagnostic odds ratio
#'@description This function computes the diagnostic odds ratio
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
DOR <- function(TP, TN, FP, FN){out = PLR(TP, TN, FP, FN) / NLR(TP, TN, FP, FN);return(out)}

#--------------------------------------------------------------------------------------------#
#'Prevalence
#'@description This function computes the prevalence.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
PRV <- function(TP, TN, FP, FN){out = (TP + FN) / (TP + FN + FP + TN);return(out)}
#'Accuracy
#'@description This function computes the accuracy.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
ACC <- function(TP, TN, FP, FN){out = (TP + TN) / (TP + FN + FP + TN);return(out)}
#'Error rate
#'@description This function computes the error rate.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
ERR <- function(TP, TN, FP, FN){out = (FP + FN) / (TP + FN + FP + TN);return(out)}
#'Balanced accuracy score
#'@description This function computes the balanced accuracy score.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
BAS <- function(TP, TN, FP, FN){out = (TPR(TP, TN, FP, FN) + TNR(TP, TN, FP, FN)) / 2;return(out)}
#'F1 score
#'@description This function computes the F1 score.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
F1S <- function(TP, TN, FP, FN){out = 2*TP / (2*TP + FP + FN);return(out)}
#'Fbeta score
#'@description This function computes the F-beta score.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
FBS <- function(TP, TN, FP, FN, beta = 1){out = (1 + beta^2)*TP / ((1 + beta^2)*TP + FP + (beta^2)*FN);return(out)}
#'Fowlkes-Mallows index
#'@description This function computes the Fowlkes-Mallows index.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
FMI <- function(TP, TN, FP, FN){out = sqrt(x = (PPV(TP, TN, FP, FN)*TPR(TP, TN, FP, FN)));return(out)}
#'Threat score (TS), critical success index (CSI), Jaccard index
#'@description This function computes the Jaccard index.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
CSI <- function(TP, TN, FP, FN){out = TP / (TP + FN + FP);return(out)}
#'Matthews correlation coefficient
#'@description This function computes the Matthews correlation coefficient.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
MCC <- function(TP, TN, FP, FN){
  tpr = TPR(TP, TN, FP, FN)
  tnr = TNR(TP, TN, FP, FN)
  ppv = PPV(TP, TN, FP, FN)
  npv = NPV(TP, TN, FP, FN)

  fnr = FNR(TP, TN, FP, FN)
  fpr = FPR(TP, TN, FP, FN)
  For = FOR(TP, TN, FP, FN)
  fdr = FDR(TP, TN, FP, FN)

  out = sqrt(tpr*tnr*ppv*npv) - sqrt(x = fnr*fpr*For*fdr);
  return(out)
}


#'Informedness, bookmaker informedness (BM)
#'@description This function computes the bookmaker informedness.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
BMI <- function(TP, TN, FP, FN){out = TPR(TP, TN, FP, FN) + TNR(TP, TN, FP, FN) - 1;return(out)}
#'Prevalence threshold (PT)
#'@description This function computes the prevalence threshold.
#'@inheritParams TPR
#'@return A numeric value.
#'@author Alessandro Barberis
#'@export
PRT <- function(TP, TN, FP, FN){
  tpr = TPR(TP, TN, FP, FN)
  fpr = FPR(TP, TN, FP, FN)
  out = (sqrt(tpr*fpr) - fpr ) / (tpr - fpr);
  return(out)
}

true_pred  <- function(true, pred, weights = NULL, multi, ...){as.integer(true == pred)}
false_pred <- function(true, pred, weights = NULL, multi, ...){as.integer(true != pred)}

#'Confusion Matrix
#'@description
#'This function produces a confusion matrix given a set of observed and
#'predicted values.
#'
#'@details
#'The confusion matrix is a specific table that reports the
#'classification accuracy of an algorithm. In this implementation,
#'a confusion matrix C has rows reporting instances of the actual classes,
#'while columns report instances of the predicted classes.
#'
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'
#'@return the confusion matrix, a \code{table} reporting the number of samples
#'for different groups. The matrix element \code{(i, j)} represents the number of samples
#'known to be in group \code{i} and predicted in group \code{j}.
#'
#'@source See \href{https://en.wikipedia.org/wiki/Confusion_matrix}{Wiki page} for further information.
#'
#'@author Alessandro Barberis
confusion_matrix <- function(true, pred){

  #check if multi-dim
  is.multi = !is.null(dim(true))

  if (!is.multi) {
    #force to factor
    true = as.factor(true)
    pred = as.factor(pred)
    #nc
    nc = length(levels(true))

  } else {
    #n of classes
    nc = ncol(true)
    #
    yc = true %*% (1:nc)
    #class names
    cn = colnames(true)
    #set as factor
    if(!is.null(cn)){true = factor(yc, labels = cn)} else {true = factor(yc)}
    stop("Not supported yet\n")
  }

  #set pred levels equal to true levels
  #in order to have a table with also zero counts
  # levels(pred) = unique(c(levels(pred), levels(true)))
  all_levels = unique(c(levels(pred), levels(true)))
  levels(pred) = unique(c(levels(pred), all_levels))
  levels(true) = unique(c(levels(true), all_levels))

  # ctable = function(...) {
  #   tab = table(...)
  #   class(tab) = c("confusion.table", class(tab))
  #   tab
  # }
  #get confusion
  # out = ctable(Predicted = pred, True = true)
  # out = table(Predicted = pred, True = true)
  out = table(True = true, Predicted = pred)

  #order
  out = out[levels(true), levels(true), drop=F]

  #Set class
  # class(out) = c("confusion.table", class(out))
  class(out) = c("ConfusionMatrix", class(out))
  # class(out) = c("ConfusionMatrix")

  #return
  return(out)
}


#'Confusion Matrix outcomes
#'@description
#'This function returns the outcome of a confusion matrix in a list.
#'
#'@details
#'The outcomes of a \code{\link[confusion_matrix]{confusion matrix}} are returned as elements of a list.
#'The four possible outcomes are:
#'\describe{
#'   \item{TP}{true positives, when the actual presence of a condition is correctly predicted}
#'   \item{TN}{true negatives, when the actual absence of a condition is correctly predicted}
#'   \item{FP}{false positives, when the actual absence of a condition is wrongly predicted}
#'   \item{FN}{false negatives, when the actual presence of a condition is wrongly predicted}
#'}
#'
#'@inheritParams confusion_matrix
#'@param confusion a confusion matrix as returned by \code{\link{confusion_matrix}}. If \code{confusion} is not provided,
#'a confusion matrix is internally computed by using \code{true} and \code{pred}
#'@param positive integer or character indicating the target class.
#'If \code{positive = 0} (default), global values are computed.
#'
#'@return a list containing the number of false positives (\code{FP}), false negatives (\code{FN}),
#'true positives (\code{TP}), and true negatives (\code{TN}) of a confusion matrix.
#'
#'@author Alessandro Barberis
confusion_matrix_outcomes <- function(true, pred, confusion = NULL, positive = 0){
  #Check confusion matrix
  if(is.null(confusion)){
    if(isTRUE((missing(true) | missing(pred)))){
      stop("Missing input parameters: either 'true'/'pred' or 'confusion' must be provided.\n")
    } else {
      confusion = confusion_matrix(true = true, pred = pred)
    }
  }

  #--------------------------------------------------------------------------------------------#
  #classes
  classes = rownames(confusion)
  nc = length(classes)

  #--------------------------------------------------------------------------------------------#
  #Check positive
  if(isFALSE(is.numeric(positive))){
    positive = match(x = positive, table = classes)
    if(is.na(positive)){stop(paste("\nProvided argument 'positive' not valid. Please, check your input.\n", "\npositive:", positive))}
  }

  #--------------------------------------------------------------------------------------------#
  if((is.numeric(positive) & isTRUE(positive == 0))){
    #Global
    TP = sum(diag(x = confusion))
    diag(confusion) = 0
    FP = FN = sum(confusion)
    TN = 0
  } else if((is.numeric(positive) & isTRUE(positive > 0) & isTRUE(positive <= nc))){

    #set positive class
    pos_class = classes[positive]
    neg_class = classes[-positive]

    #--------------------------------------------------------------------------------------------#
    #Extract info
    #--------------------------------------------------------------------------------------------#
    #Class
    TP = confusion[pos_class, pos_class]
    # TN = sum(diag(x = confusion[neg_class, neg_class]))
    TN = sum(confusion[neg_class, neg_class])
    FN = sum(confusion[pos_class, neg_class])
    FP = sum(confusion[neg_class, pos_class])
  } else {
    logMsg = paste("\nProvided argument 'positive' not valid. Please, check your input.\n", "\npositive:", positive)
    stop(logMsg)
  }

  #--------------------------------------------------------------------------------------------#
  #create output list
  out = list(TP = TP, TN = TN, FP = FP, FN = FN)

  #--------------------------------------------------------------------------------------------#
  return(out)
}

#'Classification metric
#'
#'@description This function computes a classification metric.
#'
#'@details This function has a common interface to compute a classification metric.
#'
#'@inheritParams confusion_matrix_outcomes
#'@param metric character string indicating the score to compute
#'
#'@return A classification metric
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
classification_metric <- function(true, pred, confusion = NULL, positive = 1, metric){

  #get confusion matrix outcomes
  cmo = confusion_matrix_outcomes(true = true, pred = pred, confusion = confusion, positive = positive)

  #--------------------------------------------------------------------------------------------#
  #Compute metric
  out = if(isTRUE(length(metric)==1)){
    do.call(what = metric, args = cmo)
  } else {
    lapply(X = metric, FUN = do.call, args = cmo)
  }

  #--------------------------------------------------------------------------------------------#
  #set names
  names(out) = metric

  #--------------------------------------------------------------------------------------------#
  return(out)
}


#'Multi-response Classification Metric
#'
#'@description This function computes a multi-response classification metric.
#'
#'@details Common interface for computing a multi-response classification metric.
#'
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'@param weights observation weights (not implemented yet)
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{micro}}{the score is computed by using the global number of true positives, false negatives and false positives}
#'   \item{\code{macro}}{scores of different classes are averaged by unweighted mean to get a single value. Class imbalance is not taken into account}
#'   \item{\code{weighted}}{scores of different classes are averaged by weighted mean to get a single value. Weights are number of true instances per class. It takes into account label imbalance}
#'   \item{\code{raw}}{returns a vector containing one score for each class}
#'   \item{\code{binary}}{returns the score for the class specified by \code{positive}}
#'}
#'@inheritParams classification_metric
#'
#'@return A single score for the selected class if \code{multi = "binary"},
#'a vector containing one score for each class if \code{multi = "raw"},
#'a summary score computed by using the global metrics if \code{multi = "micro"} or
#'averaging the results from different classes if \code{multi = "macro"}. A summary score
#'produced by a weighted average of the results from different class is produced if
#'\code{multi = "weighted"}
#'
#'@keywords internal
#'
#'@author Alessandro Barberis
multiresponse_classification_metric <- function(
  true,
  pred,
  multi = c("weighted", "average", "micro", "macro", "raw","binary"),
  metric,
  positive = 1,
  weights,
  confusion = NULL,
  ...){

  #multi
  multi = match.arg(multi)

  #--------------------------------------------------------------------------------------------#
  #Check confusion matrix
  if(is.null(confusion)){
    if(isTRUE((missing(true) | missing(pred)))){
      stop("Missing input parameters: either 'true'/'pred' or 'confusion' must be provided.\n")
    } else {
      confusion = confusion_matrix(true = true, pred = pred)
    }
  }

  #--------------------------------------------------------------------------------------------#
  #classes
  classes = rownames(confusion)
  nc = length(classes)

  #--------------------------------------------------------------------------------------------#
  #compute score
  if(isTRUE(identical(multi, "raw"))){
    out = sapply(X = classes, FUN = classification_metric, true = true, pred = pred, confusion = confusion, metric = metric)
    names(out) = classes
  } else if(isTRUE(identical(multi, "binary"))){
    out = classification_metric(true = true, pred = pred, confusion = confusion, metric = metric, positive = positive)
  } else if(isTRUE(identical(multi, "micro"))){
    out = classification_metric(true = true, pred = pred, confusion = confusion, metric = metric, positive = 0)
  } else {
    out = sapply(X = classes, FUN = classification_metric, true = true, pred = pred, confusion = confusion, metric = metric)
    if(isTRUE(identical(multi, "average")) | isTRUE(identical(multi, "macro"))){
      out = mean(out, na.rm = T)
    } else if(isTRUE(identical(multi, "weighted"))){
      out = stats::weighted.mean(x = out, w = rowSums(confusion), na.rm = T)
    }
  }

  #--------------------------------------------------------------------------------------------#
  return(out)
}


#'Area Under the ROC Curve
#'@description This function computes the area under the ROC curve.
#'
#'@details The Area Under the ROC Curve (AUC) measures the entire two-dimensional
#'area under the receiver operating characteristic (ROC) curve.
#'It provides an aggregate measure of performance across all possible classification
#'thresholds.
#'
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'@param weights observation weights
#'@param multi currently not used
#'@param ... further arguments (currently not used)
#'
#'@return A length-one numeric vector
#'
#'@source This function is just a wrapper to \code{\link[survival]{concordance}}
#'
#'@author Alessandro Barberis
area_under_roc_curve <- function(true, pred, weights = NULL, multi, ...){

  # out = pROC::auc(response = true, predictor = pred)
  out = if (missing(weights)){
    survival::concordance(true ~ pred)$concordance
  } else {
    survival::concordance(true ~ pred, weights = weights)$concordance
  }

  return(out)
}



#'F1 Score
#'
#'@description This function computes the F1 score.
#'
#'@details The F1 score is a measure of accuracy and is computed as
#'the harmonic mean of the precision and recall:
#'
#'\deqn{F_{1} = \frac{2}{precision^{-1} + recall^{-1}} = 2\frac{precision \times recall}{precision + recall} = \frac{TP}{TP + \frac{1}{2}(FP + FN)}}
#'
#'The highest possible value is 1.0, indicating perfect precision and recall. The lowest possible value is 0.
#'
#'It is a specific case of the F-beta score, where \code{beta = 1}
#'
#'@inheritParams multiresponse_classification_metric
#'@param ... further arguments to \code{\link{multiresponse_classification_metric}}
#'
#'@inherit multiresponse_classification_metric return
#'
#'@seealso \code{\link{fbeta_score}}
#'
#'@references \url{https://en.wikipedia.org/wiki/F-score}
#'
#'@author Alessandro Barberis
f1_score <- function(true, pred, weights = NULL, multi, ...){
  #Compute score
  out = multiresponse_classification_metric(true = true, pred = pred, weights = weights, multi = multi, metric = "F1S", ...)
  #--------------------------------------------------------------------------------------------#
  return(out)
}


#'F-beta Score
#'
#'@description This function computes the F-beta score.
#'
#'@details The F-beta score is a measure of accuracy based on precision and recall. It is defined as
#'
#'\deqn{F_{\beta} = (1 + \beta^{2}) \frac{precision \times recall}{(\beta^{2} precision) + recall} = (1 + \beta^{2}) \frac{TP}{(1 + \beta^{2})TP + \beta^{2} FN + FP)}}
#'
#'The highest possible value of an F-score is 1.0, indicating perfect precision and recall. The lowest possible value is 0.
#'
#'@inheritParams multiresponse_classification_metric
#'@param beta a length-one numeric vector, the beta value to consider. Default is \code{beta = 1}
#'@param ... further arguments to \code{\link{multiresponse_classification_metric}}
#'
#'@inherit multiresponse_classification_metric return
#'
#'@seealso \code{\link{f1_score}} a special case of F-beta score where \code{beta = 1}
#'
#'@references \url{https://en.wikipedia.org/wiki/F-score}
#'
#'@author Alessandro Barberis
fbeta_score <- function(true, pred, beta = 1, weights = NULL, multi, ...){
  #Compute score
  out = multiresponse_classification_metric(true = true, pred = pred, weights = weights, multi = multi, metric = "FBS", beta = beta, ...)
  #--------------------------------------------------------------------------------------------#
  return(out)
}


#'Sensitivity
#'
#'@description This function computes the sensitivity.
#'
#'@details The sensitivity measures the ability of a classifier of correctly predicting the presence of a condition.
#'It is defined as
#'
#'\deqn{sensitivity = recall = \textit{true positive rate} (TPR) = \frac{TP}{P} = \frac{TP}{TP + FN} = 1 - FNR}
#'
#'The optimal value is 1 and the worst value is 0.
#'
#'@inheritParams multiresponse_classification_metric
#'@param ... further arguments to \code{\link{multiresponse_classification_metric}}
#'
#'@inherit multiresponse_classification_metric return
#'
#'@references \url{https://en.wikipedia.org/wiki/Sensitivity_and_specificity}
#'
#'@author Alessandro Barberis
sensitivity = recall = true_positive_rate <- function(
  true, pred, weights = NULL, multi, ...
){
  #Compute score
  out = multiresponse_classification_metric(true = true, pred = pred, weights = weights, multi = multi, metric = "TPR", ...)
  #return
  return(out)
}


#'Precision
#'
#'@description This function computes the precision.
#'
#'@details The precision is the fraction of correctly predicted elements with a condition.
#'It is defined as:
#'
#'\deqn{precision = \textit{positive predictive value} (PPV) = \frac{TP}{PP} = \frac{TP}{TP + FP} = 1 - FDR}
#'
#'The optimal value is 1 and the worst value is 0.
#'
#'@inheritParams multiresponse_classification_metric
#'@param ... further arguments to \code{\link{multiresponse_classification_metric}}
#'
#'@inherit multiresponse_classification_metric return
#'
#'@author Alessandro Barberis
precision = positive_predictive_value <- function(
  true, pred, weights = NULL, multi, ...
){
  #Compute score
  out = multiresponse_classification_metric(true = true, pred = pred, weights = weights, multi = multi, metric = "PPV", ...)
  #return
  return(out)
}

#'Jaccard Score
#'
#'@description This function computes the threat score
#'
#'@inheritParams multiresponse_classification_metric
#'@param ... further arguments to \code{\link{multiresponse_classification_metric}}
#'
#'@inherit multiresponse_classification_metric return
#'
#'@details The threat score is defined as:
#'
#'\deqn{threat score = Jaccard index = critical success index = \frac{TP}{TP + FN + FP}}
#'
#'@author Alessandro Barberis
threat_score = critical_success_index = jaccard_index <- function(
    true, pred, weights = NULL, multi, ...
){
  #Compute score
  out = multiresponse_classification_metric(true = true, pred = pred, weights = weights, multi = multi, metric = "CSI", ...)
  #return
  return(out)
}

#'Classification Error Rate
#'
#'@description This function computes the misclassification error.
#'
#'@details The classification error rate measures the fraction of all instances that are wrongly categorized.
#'It is defined as:
#'
#'\deqn{ERR = \frac{errors}{total} = \frac{FP + FN}{P + N} = \frac{FP + FN}{TP + FP + TN + FN} = 1 - ACC}
#'
#'The optimal value is 0 and the worst value is 1. The complementary statistic is the \code{\link[accuracy_score]{accuracy}}.
#'
#'@param true a vector of observed values
#'@param pred a vector of predicted values
#'@param weights vector of observation weights
#'@param multi what to do when response has multiple classes
#'\describe{
#'   \item{\code{average}}{errors of multiple classes are averaged to get a single value}
#'   \item{\code{raw}}{returns a vector containing one score for each class}
#'}
#'@param ... not currently used
#'
#'@return A numeric vector of length one if \code{multi = "average"} or \code{nc} if
#'\code{multi = "raw"}, where \code{nc} is the number of classes.
#'
#'@references \url{https://en.wikipedia.org/wiki/Evaluation_of_binary_classifiers#Single_metrics}
#'
#'@author Alessandro Barberis
classification_error_rate <- function(true, pred, weights = NULL, multi = c("average", "raw"), ...){
  # #Compute accuracy
  # out = accuracy_score(...)
  # #misclassification = 1 - accuracy
  # out = 1 - out

  #multi
  multi = match.arg(multi)

  #get N
  N = if(!is.null(nrow(true))){nrow(true)} else {length(true)}

  #check weights
  if(missing(weights) || is.null(weights)) {weights = rep(1, N)}

  #--------------------------------------------------------------------------------------------#
  #compute confusion matrix
  confusion = confusion_matrix(true = true, pred = pred)

  #classes
  classes = rownames(confusion)
  nc = length(classes)

  #--------------------------------------------------------------------------------------------#
  #check if multi-response
  is.multi = isTRUE(nc > 1)

  #compute accuracy
  if(is.multi & isTRUE(identical(multi, "average"))){

    #Raw accuracy
    acc.raw = false_pred(true = true, pred = pred)

    #Apply the weighted mean
    # out = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)#to use when multi-config
    out = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
  } else {
    out = sapply(X = classes, FUN = classification_metric, true = true, pred = pred, confusion = confusion, metric = "ERR")
    #name with the classes
    names(out) = classes

  }
  #return
  return(out)
}




#'Accuracy Classification Score
#'
#'@description This function computes the accuracy of a classification.
#'
#'@details The accuracy measures the fraction of all instances that are correctly categorized.
#'It is defined as:
#'
#'\deqn{ACC = \frac{correct}{total} = \frac{TP + TN}{P + N} = \frac{TP + TN}{TP + FP + TN + FN} = 1 - ERR}
#'
#'The optimal value is 1 and the worst value is 0.
#'The complementary statistic is the \code{\link[classification_error_rate]{classification error rate}}.
#'
#'@param true a vector of observed values
#'@param pred a vector of predicted values
#'@param weights vector of observation weights
#'@param multi what to do when response has multiple classes
#'\describe{
#'   \item{\code{average}}{errors of multiple classes are averaged to get a single value}
#'   \item{\code{raw}}{returns a vector containing one score for each class}
#'}
#'@param ... not currently used
#'
#'@return A numeric vector of length one if \code{multi = "average"} or \code{nc} if
#'\code{multi = "raw"}, where \code{nc} is the number of classes.
#'
#'@references \url{https://en.wikipedia.org/wiki/Evaluation_of_binary_classifiers#Single_metrics}
#'
#'@author Alessandro Barberis
accuracy_score <- function(true, pred, weights = NULL, multi = c("average", "raw"), ...){

  #multi
  multi = match.arg(multi)

  #get N
  N = if(!is.null(nrow(true))){nrow(true)} else {length(true)}

  #check weights
  if(missing(weights) || is.null(weights)) {weights = rep(1, N)}

  #--------------------------------------------------------------------------------------------#
  #compute confusion matrix
  confusion = confusion_matrix(true = true, pred = pred)

  #classes
  classes = rownames(confusion)
  nc = length(classes)

  #--------------------------------------------------------------------------------------------#
  #check if multi-response
  is.multi = isTRUE(nc > 1)

  #compute accuracy
  if(is.multi & isTRUE(identical(multi, "average"))){

    #Raw accuracy
    acc.raw = true_pred(true = true, pred = pred)

    #Apply the weighted mean
    # out = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)#to use when multi-config
    out = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
  } else {
    out = sapply(X = classes, FUN = classification_metric, true = true, pred = pred, confusion = confusion, metric = "ACC")
    #name with the classes
    names(out) = classes

  }

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}




supported_performance_metrics = function(resp.type = NULL){

  #Supported metrics
  out = c(
    #Regression
    "mae", "mape", "mse", "rmse", "msle", "r2",
    #Classification
    "class", "acc", "auc", "f1s", "fbeta", "precision", "sensitivity", "jaccard"
  )

  if(isTRUE(!missing(resp.type) & !is.null(resp.type))){
    #match measure with response type
    subclass = switch(
      resp.type,
      gaussian    = c(1, 2, 3, 4, 5, 6),
      binomial    = c(7, 8, 9, 10, 11, 12, 13, 14),
      # poisson     = c(2, 1, 5),
      # cox         = c(2, 6),
      multinomial = c(7, 8, 9, 10, 11, 12, 13, 14),
      mgaussian   = c(1, 2, 3, 4, 5, 6)
    )

    #subset
    out = out[subclass]
  }

  #get names
  measure.names = get_performance_metric_name(out)

  #set names
  names(out) = measure.names

  #return
  return(out)

}

get_performance_metric_name <- function(metric){
  typenames = c(
    #Regression
    mse            = "Mean-squared Error",
    mae            = "Mean Absolute Error",
    rmse           = "Root-mean-square Error",
    mape           = "Mean Absolute Percentage Error",
    r2             = "R2",
    msle           = "Mean-squared Logarithmic Error",
    ape            = "Absolute Percentage Error",
    sle            = "Squared Logarithmic Error",
    C              = "C-index",
    #Classification
    auc            = "AUC",
    class          = "Misclassification Error",
    acc            = "Accuracy",
    prevalence     = "Prevalence",
    f1s            = "F1 Score",
    fbeta          = "F-beta Score",
    precision      = "Precision",
    sensitivity    = "Sensitivity",
    jaccard        = "Jaccard Index"
  )

  out = typenames[metric]

  return(out)
}

get_performance_metric_ml_problem <- function(metric){
  typenames = c(
    #Regression
    mse           = "regression",
    mae           = "regression",
    rmse          = "regression",
    mape          = "regression",
    r2            = "regression",
    msle          = "regression",
    ape           = "regression",
    sle           = "regression",
    C             = "regression",
    #Classification
    auc            = "classification",
    class          = "classification",
    acc            = "classification",
    prevalence     = "classification",
    f1s            = "classification",
    fbeta          = "classification",
    precision      = "classification",
    sensitivity    = "classification",
    jaccard        = "classification"
  )

  out = typenames[metric]

  return(out)
}


#'Supported Performance Metrics
#'
#'@description This function returns a \code{data.frame} containing the currently
#'supported performance metrics.
#'
#'@details The currently implemented performance metrics are returned in a \code{data.frame}.
#'If \code{resp.type} is provided, only the metrics supported for the given response
#'are returned.
#'
#'@param resp.type (optional) the response type
#'
#'@return A \code{data.frame} with 3 columns:
#'\describe{
#'\item{\code{id}}{contains the id used in renoir for the performance metric (e.g. 'class')}
#'\item{\code{name}}{contains the name of the performance metric (e.g. 'Misclassification Error')}
#'\item{\code{problem}}{contains the type of problem for which is commonly used (e.g. 'classification')}
#'}
#'
#'@seealso
#'\code{\link{mean_absolute_error}},
#'\code{\link{mean_absolute_percentage_error}},
#'\code{\link{mean_squared_error}},
#'\code{\link{root_mean_squared_error}},
#'\code{\link{mean_squared_log_error}},
#'\code{\link{r2_score}},
#'\code{\link{classification_error_rate}},
#'\code{\link{accuracy_score}},
#'\code{\link{area_under_roc_curve}},
#'\code{\link{f1_score}},
#'\code{\link{fbeta_score}},
#'\code{\link{precision}},
#'\code{\link{sensitivity}},
#'\code{\link{jaccard_index}}
#'
#\code{\link{mae}},
#\code{\link{mape}},
#\code{\link{mse}},
#\code{\link{rmse}},
#\code{\link{msle}},
#\code{\link{r2}},
#\code{\link{class}},
#\code{\link{acc}},
#\code{\link{auc}},
#\code{\link{f1s}},
#\code{\link{fbeta}},
#\code{\link{precision}},
#\code{\link{sensitivity}},
#\code{\link{jaccard}}
#'
#'@export
#'
#'@author Alessandro Barberis
list_supported_performance_metrics <- function(resp.type = NULL){
  #metrics
  out = supported_performance_metrics(resp.type = resp.type)
  #create df
  out = data.frame(id = out, name = names(out), row.names = NULL, stringsAsFactors = F)
  #add ML problem
  out$problem = get_performance_metric_ml_problem(out$id)

  #r
  return(out)
}






#'Classification report
#'
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'@param confusion confusion matrix
#'
#'@return An object of class `ClassificationReport`.
#'
#'@keywords internal
#'@examples
#'\dontrun{
#'true = c(0, 1, 2, 2, 0)
#'pred = c(0, 0, 2, 1, 0)
#'
#'classification_report(true = true, pred = pred)
#'x = confusion_matrix(true = true, pred = pred)
#'cr = classification_report(confusion = x)
#'
#'true = c('dog', 'cat', 'dog', 'cat', 'dog', 'dog', 'cat', 'dog', 'cat', 'dog')
#'pred = c('dog', 'dog', 'dog', 'cat', 'dog', 'dog', 'cat', 'cat', 'cat', 'cat')
#'x = confusion_matrix(true = true, pred = pred)
#'print.ConfusionMatrix(x = x)
#'print.ConfusionMatrix(x = x, format = 'html')
#'
#'true = sample(x = 1:4, size = 10, replace = T)
#'pred = sample(x = 1:4, size = 10, replace = T)
#'x = confusion_matrix(true = true, pred = pred)
#'
#'print.ConfusionMatrix(x = x, format = 'html')
#'}
classification_report <- function(true, pred, confusion = NULL){
  #Check confusion matrix
  if(is.null(confusion)){
    if(isTRUE((missing(true) | missing(pred)))){
      stop("Missing input parameters: either 'true'/'pred' or 'confusion' must be provided.\n")
    } else {
      confusion = confusion_matrix(true = true, pred = pred)
    }
  }

  #--------------------------------------------------------------------------------------------#
  #classes
  classes = rownames(confusion)

  #Check number of classes
  nc = length(classes)

  #--------------------------------------------------------------------------------------------#
  #Get reports for every class
  reports = lapply(X = classes, FUN = classification_report_binomial, confusion = confusion, true = true, pred = pred)

  #--------------------------------------------------------------------------------------------#
  #Compute overall statistics
  ovr.stats = c("SEN", "PPV", "F1S")
  overall.mic = unlist(lapply(X = ovr.stats, FUN = multiresponse_classification_metric, confusion = confusion, true = true, pred = pred, multi = "micro"))
  overall.mac = sapply(X = ovr.stats, FUN = multiresponse_classification_metric, confusion = confusion, true = true, pred = pred, multi = "macro")
  overall.wei = sapply(X = ovr.stats, FUN = multiresponse_classification_metric, confusion = confusion, true = true, pred = pred, multi = "weighted")

  overall = list(Micro = overall.mic, Macro = overall.mac, Weighted = overall.wei)
  #--------------------------------------------------------------------------------------------#
  #create output
  out = list(confusion = confusion, classes = reports, overall = overall)

  class(out) = c("ClassificationReport", class(out))
  #--------------------------------------------------------------------------------------------#
  #return
  return(out)

}

#'Classification report for binomial data
#'
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'@param confusion confusion matrix
#'@param positive index of 'positive' class
#'
#'@return An object of class `BinaryClassificationReport`.
#'
#'@keywords internal
classification_report_binomial <- function(true, pred, confusion = NULL, positive = 1){

  metrics = c(
    # "TP", "TN", "FN", "FP",
    # "P" ,  "N", "PP", "PN",
    "TPR", "FPR", "FNR", "TNR",
    "FDR", "PPV", "FOR", "NPV",
    "PLR", "NLR", "MKD", "DOR",
    "PRV", "ACC", "BAS",
    "F1S", "FMI", "MCC", "CSI", "BMI", "PRT")


  #Check confusion matrix
  if(is.null(confusion)){
    if(isTRUE((missing(true) | missing(pred)))){
      stop("Missing input parameters: either 'true'/'pred' or 'confusion' must be provided.\n")
    } else {
      confusion = confusion_matrix(true = true, pred = pred)
    }
  }


  out = classification_metric(true = true, pred = pred, confusion = confusion, positive = positive, metric = metrics)

  #--------------------------------------------------------------------------------------------#
  #classes
  classes = rownames(confusion)
  nc = length(classes)

  #--------------------------------------------------------------------------------------------#
  #Check positive
  if(isFALSE(is.integer(positive) & isTRUE(positive > 0) & isTRUE(positive <= nc))){
    positive = match(x = positive, table = classes)
    if(is.na(positive)){stop("\nProvided argument 'positive' not valid. Please, check your input.\n")}
  }

  #--------------------------------------------------------------------------------------------#
  #set positive class
  pos_class = classes[positive]

  #--------------------------------------------------------------------------------------------#
  #Create output
  #--------------------------------------------------------------------------------------------#
  out = c(
    list(pos = pos_class),
    out
  )

  class(out) = c("BinaryClassificationReport", class(out))
  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}
