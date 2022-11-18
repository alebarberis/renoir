#' @include classes_generics.R class_TunedList.R class_TrainedList.R class_Logger.R
NULL

#' Forecaster Class
#' An S4 class providing the methods to test the trained models on provided data.
#'
#' The object consists of 3 slots
#' @slot id the object identifier, it must be equivalent to the learning method name (i.e. the trainer id)
#' @slot forecaster function to use to compute predictions.
#' It must have the following formal arguments:
#' \describe{
#'    \item{object}{a model for which prediction is desired.
#'    It is a \linkS4class{Trained} object as returned by the training process,
#'    where the fit is stored in the \code{fit} slot}
#    It is an object as returned by the training process and stored in the \code{fit} slot of a \linkS4class{Trained}}
#'    \item{newx}{the new values at which prediction are to be made}
#'    \item{type}{the type of prediction required}
#'    \item{newoffset}{if an offset was used in the fit then one must be provided}
#'    \item{...}{additional arguments affecting the predictions produced}
#' }
# @slot learning.method the associated learning method
#' @slot selector helper function to select the appropriate prediction type given
#' the response type and an accuracy measure
#' It must have the following formal arguments:
#' \describe{
#'    \item{learning.method}{the learning method, used to internally select the correct prediction type}
#'    \item{type.measure}{the accuracy type measure, used to internally select the correct prediction type}
#' }
# @slot logger a \linkS4class{Logger}
#' @author Alessandro Barberis
methods::setClass(
  Class = "Forecaster",
  slots = c(
    id              = "character",
    forecaster      = "function",
    # learning.method = "character",
    selector        = "function"
    # logger          = "Logger"
  )
)

#' Constructor for the S4 Forecaster object.
#'
#' Constructor for the S4 \linkS4class{Forecaster} object.
#'
# @param learning.method the learning method associated to this \linkS4class{Forecaster}.
# If learning method is one of the supported by renoir, the constructor will
# automatically select \code{prediction} and \code{selector} if they are missing.
#' @param id object identifier, must be equivalent to the id of the associated \linkS4class{Trainer}.
#' If learning method is one of the supported by renoir, the constructor will
#' automatically select \code{forecaster} and \code{selector} if they are missing.
#' @param forecaster (optional) function used to compute predictions.
#' Used if \code{id} is not one of the supported by renoir.
#' If \code{forecaster} is provided it must conform to the renoir common interface,
#' and must have the following formal arguments:
#' \describe{
#'    \item{object}{a model for which prediction is desired.
#'    It is a \linkS4class{Trained} object as returned by the training process,
#'    where the fit is stored in the \code{fit} slot}
#    It is an object as returned by the training process and stored in the \code{fit} slot of a \linkS4class{Trained}}
#'    \item{newx}{the new values at which prediction are to be made}
#'    \item{type}{the type of prediction required}
#'    \item{newoffset}{if an offset was used in the fit then one must be provided}
#'    \item{...}{additional arguments affecting the predictions produced}
#' }
#' @param selector (optional) helper function to select the appropriate prediction type given
#' the response type and an accuracy measure
#' If provided it must return a prediction type supported by \code{forecaster}
#' and must have the following formal arguments:
#' \describe{
#    \item{learning.method}{the learning method, used to internally select the correct prediction type}
#'    \item{resp.type}{the response type}
#'    \item{type.measure}{the accuracy type measure, used to internally select the correct prediction type}
#' }
#'
#'
#' @return An object of class \linkS4class{Forecaster}.
#'
#' @export
#'
#' @author Alessandro Barberis
Forecaster <- function(
  id,
  forecaster,
  # learning.method,
  selector
  # logger   = Logger()
){

  if(id %in% supported_learning_methods()){
    if(missing(forecaster)){
      forecaster = get_prediction_function(id)
    }

    if(missing(selector)){
      selector = get_prediction_type_selector_function(id)
    }
  }

  #Check provided prediction function
  check_provided_prediction_function(forecaster)

  #Check provided selector function
  check_provided_prediction_type_selector_function(selector)

  methods::new(
    Class = "Forecaster",
    id              = id,
    forecaster      = forecaster,
    # learning.method = learning.method,
    selector        = selector
  )
}

methods::setMethod(f = "get_forecaster",      signature = "Forecaster", definition = function(object){methods::slot(object = object, name = 'forecaster')})
methods::setMethod(f = "get_selector",        signature = "Forecaster", definition = function(object){methods::slot(object = object, name = 'selector')})
# methods::setMethod(f = "get_learning_method", signature = "Forecaster", definition = function(object){methods::slot(object = object, name = 'learning.method') })
methods::setMethod(f = "get_id", signature = "Forecaster", definition = function(object){methods::slot(object = object, name = 'id') })

#'Get prediction type
#'@description Select the appropriate prediction type given the accuracy measure
#'@param object a S4 \linkS4class{Forecaster} object
#'@param type.measure the accuracy measure
#'@return the prediction type required to compute the prediction values expected
#'in input by a \linkS4class{Scorer} to calculate the score
methods::setMethod(
  f = "get_prediction_type",
  signature = "Forecaster",
  definition = function(object, type.measure){

    #Extract selector
    pred_type_selector_fun = get_selector(object)

    #Get prediction type
    # out = do.call(what = pred_type_selector_fun, args = list(type.measure = type.measure))
    out = sapply(X = type.measure, FUN = pred_type_selector_fun)

    #Return
    return(out)
  }
)

check_provided_prediction_function <- function(forecaster){

  if(missing(forecaster)){
    stop("'forecaster' is missing with no default.\n")
  } else {
    #needed formals
    formals.def = c("object", "newx", "type", "newoffset")

    #get formals
    formals.fun = names(formals(fun = forecaster))

    #check
    # if(any(!(formals.fun %in% formals.def))){
    if(any(!(formals.def %in% formals.fun))){
      # stop("Provided prediction function without required formal arguments.Function interface must match renoir requirements. Try with:\n
      #      function(object, newx, type, newoffset, ...){ YOUR CODE }
      #      \n")
      stop("Provided prediction function without required formal arguments. Function interface must match renoir requirements. Try with:\n
         function(object, newx, type, newoffset, ...){
            #Extract fit from Trained object
            model = get_fit(object)

            #Extract configuration from Trained object
            config = get_config(object)

            #Use model and configuration to make prediction
            #out = YOUR CODE

            #Return prediction
            return(out)
         }
         \n")
    }
  }
}

check_provided_prediction_type_selector_function <- function(selector){
  if(missing(selector)){
    stop("'selector' is missing with no default.\n")
  } else {
    #needed formals
    formals.def = c("resp.type", "type.measure")

    #get formals
    formals.fun = names(formals(fun = selector))

    #check
    if(any(!(formals.fun %in% formals.def))){
      # stop("Provided prediction function without required formal arguments.Function interface must match renoir requirements. Try with:\n
      #      function(object, newx, type, newoffset, ...){ YOUR CODE }
      #      \n")
      stop("Provided selector function without required formal arguments. Function interface must match renoir requirements. Try with:\n
         function(resp.type, type.measure, ...){

            #Select the prediction type required for the desired type measure.
            #See help pages for more information about the supported accuracy measures
            #and the required input.
            #out = YOUR CODE

            #Return prediction type
            return(out)
         }
         \n")
    }
  }
}



#@describeIn forecast
#'@param models an object of class \code{\link{Trained}}
#'@param newx the new values at which prediction are to be made
#'@param newoffset the new offset, if provided it is added to the predicted value
#'@param type the type of prediction
methods::setMethod(
  f = "forecast",
  signature = methods::signature(forecaster = "Forecaster", models = "Trained"),
  definition = function(forecaster, models, newx, type, newoffset, check.newx = F, ...){

    #check learning method
    if(!identical(get_learning_method(models), y = get_id(forecaster))){
      stop("Learning method mismatch: please check the provided forecaster and trained model.\n")
    }

    #--------------------------------------------------------------------------------------------#
    #extract prediction function
    pred = get_forecaster(forecaster)

    #--------------------------------------------------------------------------------------------#
    #subset x to match features in models
    if(check.newx){
      newx = subset_features(object = newx, which = unique(unlist(features(object = models, type = "all"))))
    }

    #--------------------------------------------------------------------------------------------#
    #Use model to make prediction
    # out = do.call(
    #   what = pred,
    #   args = c(list(object = trained, newx = newx, type = type, newoffset = newoffset), list(...)))

    out = pred(object = models, newx = newx, type = type, newoffset = newoffset, ...)

    #--------------------------------------------------------------------------------------------#
    #Return prediction
    return(out)
  }
)

methods::setMethod(
  f = "forecast",
  signature = methods::signature(forecaster = "missing", models = "Trained"),
  definition = function(forecaster, models, newx, type, newoffset, ...){

    #Create forecaster
    forecaster = Forecaster(id = get_learning_method(models))

    #--------------------------------------------------------------------------------------------#
    #Compute predictions
    out = forecast(forecaster = forecaster, models = models, newx = newx, type = type, newoffset = newoffset, ...)

    #--------------------------------------------------------------------------------------------#
    #Return prediction
    return(out)
  }
)

methods::setMethod(
  f = "forecast",
  signature = methods::signature(forecaster = "Forecaster", models = "TrainedList"),
  definition = function(forecaster, models, newx, type, newoffset, ...){

    #Compute predictions
    out = lapply(X = models, FUN = forecast, forecaster = forecaster, newx = newx, type = type, newoffset = newoffset, ...)

    #--------------------------------------------------------------------------------------------#
    #Return prediction
    return(out)
  }
)

methods::setMethod(
  f = "forecast",
  signature = methods::signature(forecaster = "Forecaster", models = "Tuned"),
  definition = function(forecaster, models, newx, type, newoffset, ...){

    #check learning method
    if(!identical(get_learning_method(models), y = get_id(forecaster))){
      stop("Learning method mismatch: please check the provided forecaster and trained model.\n")
    }

    #--------------------------------------------------------------------------------------------#
    #extract prediction function
    # pred = get_forecaster(forecaster)

    #--------------------------------------------------------------------------------------------#
    #get trained
    trained = get_model(models)

    #--------------------------------------------------------------------------------------------#
    #Use model to make prediction
    # out = do.call(
    #   what = pred,
    #   args = c(list(object = trained, newx = newx, type = type, newoffset = newoffset), list(...)))

    # out = pred(object = trained, newx = newx, type = type, newoffset = newoffset, ...)
    out = forecast(forecaster = forecaster, models = trained, newx = newx, type = type, newoffset = newoffset, ...)
    #--------------------------------------------------------------------------------------------#
    #Return prediction
    return(out)
  }
)

methods::setMethod(
  f = "forecast",
  signature = methods::signature(forecaster = "missing", models = "Tuned"),
  definition = function(forecaster, models, newx, type, newoffset, ...){

    #Create forecaster
    forecaster = Forecaster(id = get_learning_method(models))


    #--------------------------------------------------------------------------------------------#
    #Compute predictions
    out = forecast(forecaster = forecaster, models = models, newx = newx, type = type, newoffset = newoffset, ...)

    #--------------------------------------------------------------------------------------------#
    #Return prediction
    return(out)
  }
)


methods::setMethod(
  f = "forecast",
  signature = methods::signature(forecaster = "Forecaster", models = "TunedList"),
  definition = function(forecaster, models, newx, type, newoffset, ...){

    #Compute predictions
    out = lapply(X = models, FUN = forecast, forecaster = forecaster, newx = newx, type = type, newoffset = newoffset, ...)

    #--------------------------------------------------------------------------------------------#
    #Return prediction
    return(out)
  }
)

# forecast <- function(forecaster, trained, newx, type, newoffset, ...){
#
#   #check learning method
#   if(!identical(get_learning_method(trained), y = get_learning_method(forecaster))){
#     stop("Learning method mismatch: please check the provided forecaster and trained model.\n")
#   }
#
#   #--------------------------------------------------------------------------------------------#
#   #extract prediction function
#   pred = get_forecaster(forecaster)
#
#   #--------------------------------------------------------------------------------------------#
#   #Use model to make prediction
#   out = do.call(
#     what = pred,
#     args = c(list(object = trained, newx = newx, type = type, newoffset = newoffset), list(...)))
#
#   #--------------------------------------------------------------------------------------------#
#   #Return prediction
#   return(out)
# }

#'
forecast_by_glmnet <- function(
    object,
    newx,
    type = c("link", "response", "coefficients", "class", "nonzero"),
    newoffset,
    s,
    lambda,
    gamma, ...){

  #--------------------------------------------------------------------------------------------#
  #Extract fit from Trained object
  fit = get_fit(object = object)

  #--------------------------------------------------------------------------------------------#
  #Extract configuration from Trained object
  config = get_config(object = object)

  #--------------------------------------------------------------------------------------------#
  #Get parameters
  args = list(...)

  #--------------------------------------------------------------------------------------------#
  #Check parameters: glmnet prediction function requires to select the lambda (and gamma, for relaxed fits) values

  #check lambda
  if(missing(lambda) && missing(s)){
    #set default
    s = NULL

    #check lambda
    lambda = config$lambda
    if(length(lambda)==1){
      s = lambda
    }
  } else if(missing(s)){
    s = lambda
  }

  #set s
  args[c('s')] = s

  #check if relaxed
  isrelaxed = !is.null(fit[['relaxed']])
  if(isrelaxed){
    if(missing(gamma)){
      #set default
      gamma = 1

      #check gamma
      gamma.tmp = config$gamma
      if(!is.null(gamma.tmp) && length(gamma.tmp)==1){
        gamma = gamma.tmp
      }
    }
    #set gamma
    args[c('gamma')] = gamma
  }
  #--------------------------------------------------------------------------------------------#
  #update
  if(!missing(newx)){args = c(args, list(newx = newx))}
  if(!missing(newoffset)){args = c(args, list(newoffset = newoffset))}
  args = c(list(object = fit, type = type), args)

  #--------------------------------------------------------------------------------------------#
  #Use model to make prediction
  if(isrelaxed){
    if(gamma == 1){
      #unrelaxed fit - equivalent to relax = FALSE
      #remove relaxed class to avoid errors
      index = match(x = class(args$object), table = "relaxed")
      index = index[!is.na(index)]
      if(length(index)>0){
        tmp = args$object
        #update
        class(tmp) = class(tmp)[-index]
        #store
        args$object = tmp
      }
      out = do.call(what = stats::predict, args = args)
    } else {

      # out = do.call(what = stats::predict, args = args)
      out = do.call(what = renoir_predict_relaxed, args = args)
    }

    # out = do.call(what = renoir_predict_relaxed, args = args)

  } else {
    out = do.call(what = stats::predict, args = args)
  }
  # out = do.call(what = stats::predict, args = args)

  #--------------------------------------------------------------------------------------------#
  #Check output of prediction function and standardise
  out = check_predicted_by_glmnet(object = out, type = type)

  #--------------------------------------------------------------------------------------------#
  #Return prediction
  return(out)
}

#'Forecaster for Random Forest Model Fits
#'
#'@description This function predicts values based on random forest
#'model objects.
#'
#'@param object an object of class \code{\link{Trained}}
#'@param newx the new values at which prediction are to be made
#'@param newoffset the new offset, if provided it is added to the predicted value
#'@param type the type of prediction. Three options are available
#'\describe{
#'\item{\code{"response"}}{predicted values are on the same scale as the outcome}
#'\item{\code{"prob"}}{matrix of class probabilities}
#'\item{\code{"votes"}}{matrix of vote counts}
#'}
#'@param target (optional) integer, index of the target class. If provided, only the
#'prediction for the selected class are returned (default to second level
#'of factor response). Set to \code{NULL} to return all classes
#'
#'@return A vector containing the predictions.
#'
#'@seealso
#'\code{\link[randomForest]{predict.randomForest}}
#'
#'@keywords internal
#'@author Alessandro Barberis
forecast_by_randomForest <- function(
  object,
  newx,
  type = c("response", "prob", "votes"),
  newoffset = NULL,
  target = 2L,
  ...){

  type = match.arg(type)

  #--------------------------------------------------------------------------------------------#
  #Extract fit from Trained object
  fit = get_fit(object = object)

  #--------------------------------------------------------------------------------------------#
  #Extract configuration from Trained object
  config = get_config(object = object)

  #--------------------------------------------------------------------------------------------#
  #Get parameters
  args = c(list(object = fit, type = type), list(...))
  #update
  if(!missing(newx)){args = c(args, list(newdata = newx))}

  #--------------------------------------------------------------------------------------------#
  #Use model to make prediction
  out = do.call(what = randomForest:::predict.randomForest, args = args)

  #--------------------------------------------------------------------------------------------#
  #add offset
  if(isTRUE(!missing(newoffset) & !is.null(newoffset))){
    out = out + newoffset
  }

  #--------------------------------------------------------------------------------------------#
  #check
  if(isTRUE(is.integer(target) & target > 0)){
    if(isTRUE(type %in% c("prob", "votes"))){
      if(isTRUE(target <= ncol(out)))
        out = out[,target]
    }
  }

  #--------------------------------------------------------------------------------------------#
  #Return prediction
  return(out)
}

#'Forecaster for eXtreme Gradient Boosting Model Fits
#'
#'@description This function predicts values based on extreme gradient
#'boosted model objects.
#'
#'@param object an object of class \code{\link{Trained}}
#'@param newx the new values at which prediction are to be made
#'@param newoffset the new offset, if provided it is added to the predicted value
#'@param type the type of prediction. Three options are available
#'\describe{
#'\item{\code{"response"}}{predicted values are on the same scale as the outcome,
#'e.g. for Bernoulli the returned values are probabilities, for Poisson the expected counts}
#'\item{\code{"class"}}{the class corresponding to the max probability}
#'}
#'@param pthr probability threshold, used when \code{type = "class"} and response
#'is binomial to assign label to observation
#'@param ... further arguments to \code{\link[xgboost]{predict.xgb.Booster}}
#'
#'@return A vector containing the predictions.
#'
#'@keywords internal
#'@author Alessandro Barberis
forecast_by_xgboost <- function(
  object, newx, type = c("response", "class"), newoffset,
  pthr = 0.5,
  ...){

  #--------------------------------------------------------------------------------------------#
  type = match.arg(type)

  #--------------------------------------------------------------------------------------------#
  #Extract fit from Trained object
  fit = get_fit(object = object)

  #--------------------------------------------------------------------------------------------#
  #Extract configuration from Trained object
  config = get_config(object = object)

  #--------------------------------------------------------------------------------------------#
  #Get parameters
  args = c(list(object = fit), list(...))
  #update
  if(!missing(newx)){args = c(args, list(newdata = newx))}

  #--------------------------------------------------------------------------------------------#
  #set args for common output format
  #output is array nclasses x nobservations if multiclass
  args$strict_shape = TRUE
  args$reshape = FALSE

  #--------------------------------------------------------------------------------------------#
  #Use model to make prediction
  out = do.call(what = xgboost:::predict.xgb.Booster, args = args)

  #--------------------------------------------------------------------------------------------#
  #set as list of vectors
  # out = apply(X = out, MARGIN = 1, FUN = as.vector, simplify = F)

  #--------------------------------------------------------------------------------------------#
  #add offset
  if(isTRUE(!missing(newoffset) & !is.null(newoffset))){
    out = out + newoffset
  }

  #--------------------------------------------------------------------------------------------#
  #check if class
  if(isTRUE(identical(type, "class"))){
    #get num of classes
    nc = dim(out)[1]

    #check nc
    if(nc==1){
      #binomial
      # out = out[[1]]
      out = as.numeric(out > pthr)#range 0,1
      # out = list(out)
    } else if(nc>1){
      #multinomial
      ##return class with max probability
      out = apply(X = out, MARGIN = 2, FUN = which.max, simplify = T)
      ##
      out = out - 1
    } else {
      stop("Unexpected dimension in returned array of prediction data.")
    }

    if(isFALSE(is.null(fit$classes))){
      #put number in range 1,2
      out = out + 1
      #label
      out = fit$classes[out]
    }

  }

  #--------------------------------------------------------------------------------------------#
  #Return prediction
  return(out)
}

#'Forecaster for GBM Model Fits
#'
#'@description This function predicts values based on generalized
#'boosted model objects.
#'
#'@param object an object of class \code{\link{gbm::gbm.object}}
#'@param newx the new values at which prediction are to be made
#'@param newoffset the new offset, if provided it is added to the predicted value
#'@param type the type of prediction. Three options are available
#'\describe{
#'\item{\code{"response"}}{predicted values are on the same scale as the outcome,
#'e.g. for Bernoulli the returned values are probabilities, for Poisson the expected counts}
#'\item{\code{"class"}}{the class corresponding to the max probability}
#'\item{\code{"link"}}{predictions are on the scale of f(x), e.g. for Bernoulli
#'the returned value is on the log odds scale}
#'}
#'@param pthr probability threshold, used when \code{type = "class"} and response
#'is binomial to assign label to observation
#'@param ... further arguments to \code{\link[gbm]{predict.gbm}}
#'
#'@return A vector containing the predictions.
#'
#'@keywords internal
#'@author Alessandro Barberis
forecast_by_gbm <- function(
  object, newx, type = c("response", "class", "link"), newoffset,
  pthr = 0.5,
  ...
){
  #--------------------------------------------------------------------------------------------#
  type = match.arg(type)

  #--------------------------------------------------------------------------------------------#
  #Extract fit from Trained object
  fit = get_fit(object = object)

  #--------------------------------------------------------------------------------------------#
  #Extract configuration from Trained object
  config = get_config(object = object)

  #--------------------------------------------------------------------------------------------#
  #Get parameters
  args = c(list(object = fit), list(...))
  #update
  if(!missing(newx)){args = c(args, list(newdata = newx))}

  #--------------------------------------------------------------------------------------------#
  #Use model to make prediction
  out = suppressMessages(expr = do.call(what = gbm:::predict.gbm, args = args))

  #--------------------------------------------------------------------------------------------#
  #add offset
  if(isTRUE(!missing(newoffset) & !is.null(newoffset))){
    out = out + newoffset
  }

  #--------------------------------------------------------------------------------------------#
  #check if class
  if(isTRUE(identical(type, "class"))){
    #get dim
    nc = dim(out)

    #check nc
    if(isTRUE(is.null(nc))){
      #binomial
      # out = out[[1]]
      out = as.numeric(out > pthr)

      # out = list(out)
    # } else if(nc>1){
    #   #multinomial
    #   ##return class with max probability
    #   out = apply(X = out, MARGIN = 2, FUN = which.max, simplify = T)
    #   ##
    } else {
      stop("Unexpected dimension in returned array of prediction data.")
    }

    if(isFALSE(is.null(fit$classes))){
      #put number in range 1,2
      out = out + 1
      #set labels
      out = fit$classes[out]
    }

  }

  #--------------------------------------------------------------------------------------------#
  #Return prediction
  return(out)

}



#'Forecaster for SVM Model Fits
#'
#'@description This function predicts values based on Support Vector Machines model objects.
#'
#'@details It seems there is an issue with the underlying function when asking for
#'probabilities
#'
#@param object an object of class \code{\link[e1071]{svm}}
#'@param object an object of class \linkS4class{Trained}
#'@param newx the new values at which prediction are to be made
#'@param newoffset currently not used
#'@param type character specifying the type of prediction. Three options are available
#'\describe{
#'\item{\code{"response"}}{predicted values are on the same scale as the outcome,
#'e.g. for binomial the returned values are classes, for gaussian the expected values}
#'\item{\code{"class"}}{the class corresponding to the max probability}
#'}
#'@param pthr probability threshold, used when \code{type = "class"} and response
#'is binomial to assign label to observation
#'@param ... further arguments to \code{\link[e1071]{predict.svm}}
#'
#'@return A vector containing the predictions.
#'
#'@seealso
#'\code{\link[e1071]{predict.svm}}
#'
#'@keywords internal
#'@author Alessandro Barberis
forecast_by_svm <- function(
  object, newx, type = c("response", "class"), newoffset,
  pthr = 0.5,
  ...
){
  #--------------------------------------------------------------------------------------------#
  type = match.arg(type)

  #--------------------------------------------------------------------------------------------#
  #Extract fit from Trained object
  fit = get_fit(object = object)

  #--------------------------------------------------------------------------------------------#
  #Get parameters
  args = c(list(object = fit), list(...))
  #update
  if(!missing(newx)){args = c(args, list(newdata = newx))}

  #--------------------------------------------------------------------------------------------#
  #check if was classification
  is.class = is.character(fit$levels)

  #--------------------------------------------------------------------------------------------#
  #check type
  prb = switch(
    type,
    'response' = TRUE,
    'class'    = FALSE,
    FALSE
  )

  #update
  args$probability = prb

  #--------------------------------------------------------------------------------------------#
  #Use model to make prediction
  out = do.call(what = e1071:::predict.svm, args = args)

  #--------------------------------------------------------------------------------------------#
  #add offset
  # if(isTRUE(!missing(newoffset) & !is.null(newoffset))){
  #   out = out + newoffset
  # }

  #--------------------------------------------------------------------------------------------#
  if(isTRUE(is.class && prb)){
    #get probabilities
    prob = attributes(out)$probabilities
    #out
    out = prob[,1]
  }

  #--------------------------------------------------------------------------------------------#
  #Return prediction
  return(out)
}

#'Forecaster for Generalized kNN Model Fits
#'
#'@description This function predicts values based on Generalized k-Nearest
#'Neighbours model objects.
#'
#'@param object an object of class \linkS4class{Trained}
#'@param newx the new values at which prediction are to be made
#'@param newoffset currently not used
#'@param type character specifying the type of prediction. Three options are available
#'\describe{
#'\item{\code{"response"}}{predicted values are on the same scale as the outcome,
#'e.g. for binomial the returned values are classes, for gaussian the expected values}
#'\item{\code{"class"}}{the class labels corresponding to the max probability}
#'\item{\code{"prob"}}{the class distribution}
#'\item{\code{"votes"}}{the raw counts for each class}
#'}
#'@param target (optional) integer, index of the target class. If provided, only the
#'prediction for the selected class are returned (default to second level
#'of factor response). Set to \code{NULL} to return all classes
#'@param ... further arguments to \code{\link[e1071]{predict.gknn}}
#'
#@inheritParams e1071:::predict.gknn
#'
#'@return A vector containing the predictions.
#'
#'@seealso
#'\code{\link[e1071]{predict.gknn}}
#'
#'@keywords internal
#'@author Alessandro Barberis
forecast_by_gknn <- function(
  object, newx, type = c("response", "class", "prob", "votes"), newoffset,
  ...,
  target = 2L
){
  #--------------------------------------------------------------------------------------------#
  #match
  type = match.arg(type)

  type = switch(
    type,
    response = "class",
    type
  )

  #--------------------------------------------------------------------------------------------#
  #Extract fit from Trained object
  fit = get_fit(object = object)

  #--------------------------------------------------------------------------------------------#
  #Get parameters
  args = c(list(object = fit), list(...))
  #update
  if(!missing(newx)){args = c(args, list(newdata = newx, type = type))}

  #--------------------------------------------------------------------------------------------#
  #Use model to make prediction
  out = do.call(what = e1071:::predict.gknn, args = args)

  #--------------------------------------------------------------------------------------------#
  #check
  if(isTRUE(is.integer(target) & target > 0)){
    if(isTRUE(type %in% c("prob", "votes"))){
      if(isTRUE(target <= ncol(out)))
      out = out[,target]
    }
  }

  #--------------------------------------------------------------------------------------------#
  #Return prediction
  return(out)
}


get_prediction_function <- function(learning.method){
  #set fun
  out = switch(
    learning.method,
    "lasso"              = forecast_by_glmnet,
    "ridge"              = forecast_by_glmnet,
    "elnet"              = forecast_by_glmnet,
    "elasticnet"         = forecast_by_glmnet,
    "relaxed_lasso"      = forecast_by_glmnet,
    "relaxed_ridge"      = forecast_by_glmnet,
    "relaxed_elnet"      = forecast_by_glmnet,
    "relaxed_elasticnet" = forecast_by_glmnet,
    "randomForest"       = forecast_by_randomForest,
    "xgbtree"            = forecast_by_xgboost,
    "xgblinear"          = forecast_by_xgboost,
    "gbm"                = forecast_by_gbm,
    "linear_SVM"         = forecast_by_svm,
    "polynomial_SVM"     = forecast_by_svm,
    "radial_SVM"         = forecast_by_svm,
    "sigmoid_SVM"        = forecast_by_svm,
    "linear_NuSVM"       = forecast_by_svm,
    "polynomial_NuSVM"   = forecast_by_svm,
    "radial_NuSVM"       = forecast_by_svm,
    "sigmoid_NuSVM"      = forecast_by_svm,
    "gknn"               = forecast_by_gknn,
    "nsc"                = forecast_by_nsc
  )

  return(out)
}

#'Check predict output
#'@description Function to check the output of predict function call and
#'standardise object. Expected objects: if output is multi-response,
#'a matrix nobs x nouts, if single response a vector of length nobs.
#'@return a standardised output prediction object
#'@keywords internal
check_predicted_by_glmnet <- function(object, type){
  #check dimension
  if(type %in% c("link", "response")){
    d = dim(object)
    #check if multi
    if(!is.null(d)){
      if(length(d)<3 && d[2]==1){
        object = object[,1,drop=T]
      } else if(length(d)==3 && d[3]==1){
        object = object[,,1,drop=T]
      }
    }

  } else if(type %in% c("class")){
    d = dim(object)
    #drop
    if(length(d)<3 && d[2]==1){
      object = object[,1,drop=T]
    }
  }
  return(object)
}



get_prediction_type_selector_function <- function(learning.method){
  out = switch(
    learning.method,
    "lasso"              = get_glmnet_prediction_type_for_performance_metric,
    "ridge"              = get_glmnet_prediction_type_for_performance_metric,
    "elnet"              = get_glmnet_prediction_type_for_performance_metric,
    "elasticnet"         = get_glmnet_prediction_type_for_performance_metric,
    "relaxed_lasso"      = get_glmnet_prediction_type_for_performance_metric,
    "relaxed_ridge"      = get_glmnet_prediction_type_for_performance_metric,
    "relaxed_elnet"      = get_glmnet_prediction_type_for_performance_metric,
    "relaxed_elasticnet" = get_glmnet_prediction_type_for_performance_metric,
    "randomForest"       = get_randomForest_prediction_type_for_performance_metric,
    "xgbtree"            = get_xgboost_prediction_type_for_performance_metric,
    "xgblinear"          = get_xgboost_prediction_type_for_performance_metric,
    "gbm"                = get_gbm_prediction_type_for_performance_metric,
    "linear_SVM"         = get_svm_prediction_type_for_performance_metric,
    "polynomial_SVM"     = get_svm_prediction_type_for_performance_metric,
    "radial_SVM"         = get_svm_prediction_type_for_performance_metric,
    "sigmoid_SVM"        = get_svm_prediction_type_for_performance_metric,
    "linear_NuSVM"       = get_svm_prediction_type_for_performance_metric,
    "polynomial_NuSVM"   = get_svm_prediction_type_for_performance_metric,
    "radial_NuSVM"       = get_svm_prediction_type_for_performance_metric,
    "sigmoid_NuSVM"      = get_svm_prediction_type_for_performance_metric,
    "gknn"               = get_gknn_prediction_type_for_performance_metric,
    "nsc"                = get_nsc_prediction_type_for_performance_metric,
    NA
  )

  return(out)
}


get_glmnet_prediction_type_for_performance_metric <- function(type.measure){

  out = switch(
    type.measure,
    'mse'                  = 'link',
    'msle'                 = 'link',
    'rmse'                 = 'link',
    'squared_error'        = 'link',
    'mae'                  = 'link',
    'mape'                 = 'link',
    'absolute_error'       = 'link',
    'ape'                  = 'link',
    'deviance'             = 'link',

    'class'                = 'class',
    'r2'                   = 'link',
    'binomial_deviance'    = 'link',
    'multinomial_deviance' = 'link',
    'poisson_deviance'     = 'link',
    'cox_deviance'         = 'coefficients',
    'C'                    = 'response',
    'auc'                  = 'response',
    'roc'                  = 'response',
    'true_pred'            = 'class',
    'false_pred'           = 'class',
    'acc'                  = 'class',
    'f1s'                  = 'class',
    'precision'            = 'class',
    'sensitivity'          = 'class',
    'jaccard'              = 'class',
    'class_report'         = 'class',
    NA
  )
  return(out)
}

get_randomForest_prediction_type_for_performance_metric <- function(type.measure){
  out = switch(
    type.measure,
    'mse'                  = 'response',
    'msle'                 = 'response',
    'rmse'                 = 'response',
    'squared_error'        = 'response',
    'mae'                  = 'response',
    'mape'                 = 'response',
    'absolute_error'       = 'response',
    'ape'                  = 'response',
    # 'deviance'             = 'link',

    'class'                = 'response',
    'acc'                  = 'response',
    'f1s'                  = 'response',
    'precision'            = 'response',
    'sensitivity'          = 'response',
    'jaccard'              = 'response',
    'class_report'         = 'response',
    # 'r2'                   = 'link',
    # 'binomial_deviance'    = 'link',
    # 'multinomial_deviance' = 'link',
    # 'poisson_deviance'     = 'link',
    # 'cox_deviance'         = 'coefficients',
    # 'C'                    = 'link',
    'auc'                  = 'prob',
    'roc'                  = 'prob',
    NA
  )
  return(out)
}


get_xgboost_prediction_type_for_performance_metric <- function(type.measure){
  out = switch(
    type.measure,
    'mse'                  = 'response',
    'msle'                 = 'response',
    'rmse'                 = 'response',
    'squared_error'        = 'response',
    'mae'                  = 'response',
    'mape'                 = 'response',
    'absolute_error'       = 'response',
    'ape'                  = 'response',
    # 'deviance'             = 'link',

    'class'                = 'class',
    'acc'                  = 'class',
    'f1s'                  = 'class',
    'precision'            = 'class',
    'sensitivity'          = 'class',
    'jaccard'              = 'class',
    'class_report'         = 'class',
    # 'r2'                   = 'link',
    # 'binomial_deviance'    = 'link',
    # 'multinomial_deviance' = 'link',
    # 'poisson_deviance'     = 'link',
    # 'cox_deviance'         = 'coefficients',
    # 'C'                    = 'link',
    'auc'                  = 'response',
    'roc'                  = 'response',
    NA
  )
  return(out)
}

get_gbm_prediction_type_for_performance_metric  <- function(type.measure){
  out = switch(
    type.measure,
    'mse'                  = 'response',
    'msle'                 = 'response',
    'rmse'                 = 'response',
    'squared_error'        = 'response',
    'mae'                  = 'response',
    'mape'                 = 'response',
    'absolute_error'       = 'response',
    'ape'                  = 'response',
    # 'deviance'             = 'link',

    'class'                = 'class',
    'acc'                  = 'class',
    'f1s'                  = 'class',
    'precision'            = 'class',
    'sensitivity'          = 'class',
    'jaccard'              = 'class',
    'class_report'         = 'class',
    # 'r2'                   = 'link',
    # 'binomial_deviance'    = 'link',
    # 'multinomial_deviance' = 'link',
    # 'poisson_deviance'     = 'link',
    # 'cox_deviance'         = 'coefficients',
    # 'C'                    = 'link',
    'auc'                  = 'response',
    'roc'                  = 'response',
    NA
  )
  return(out)
}

get_svm_prediction_type_for_performance_metric  <- function(type.measure){
  out = switch(
    type.measure,
    'mse'                  = 'response',
    'msle'                 = 'response',
    'rmse'                 = 'response',
    'squared_error'        = 'response',
    'mae'                  = 'response',
    'mape'                 = 'response',
    'absolute_error'       = 'response',
    'ape'                  = 'response',
    # 'deviance'             = 'link',

    'class'                = 'class',
    'acc'                  = 'class',
    'f1s'                  = 'class',
    'precision'            = 'class',
    'sensitivity'          = 'class',
    'jaccard'              = 'class',
    'class_report'         = 'class',
    # 'r2'                   = 'link',
    # 'binomial_deviance'    = 'link',
    # 'multinomial_deviance' = 'link',
    # 'poisson_deviance'     = 'link',
    # 'cox_deviance'         = 'coefficients',
    # 'C'                    = 'link',
    'auc'                  = 'response',
    'roc'                  = 'response',
    NA
  )
  return(out)
}

get_gknn_prediction_type_for_performance_metric  <- function(type.measure){
  out = switch(
    type.measure,
    'mse'                  = 'response',
    'msle'                 = 'response',
    'rmse'                 = 'response',
    'squared_error'        = 'response',
    'mae'                  = 'response',
    'mape'                 = 'response',
    'absolute_error'       = 'response',
    'ape'                  = 'response',
    # 'deviance'             = 'link',

    'class'                = 'class',
    'acc'                  = 'class',
    'f1s'                  = 'class',
    'precision'            = 'class',
    'sensitivity'          = 'class',
    'jaccard'              = 'class',
    'class_report'         = 'class',
    # 'r2'                   = 'link',
    # 'binomial_deviance'    = 'link',
    # 'multinomial_deviance' = 'link',
    # 'poisson_deviance'     = 'link',
    # 'cox_deviance'         = 'coefficients',
    # 'C'                    = 'link',
    'auc'                  = 'prob',
    'roc'                  = 'prob',
    NA
  )
  return(out)
}

#'@keywords internal
get_sample_predlist <- function(trainedlist, forecaster, newx, type, newoffset, ..., logger, indices, screened = NULL, iloop){

  #Set logger
  logger = open_con(logger)

  #--------------------------------------------------------------------------------------------#
  trained = trainedlist[[iloop]]

  #if Tuned get Trained
  if(!is.na(match("Tuned", table = class(trained)))){trained = get_model(trained)}

  #--------------------------------------------------------------------------------------------#
  #subset
  newx      = subset_observations(object = newx, which = indices[[iloop]])
  newoffset = subset_observations(object = newoffset, which = indices[[iloop]])

  #get screened features list
  screened = screened[[iloop]]
  if(!is.null(screened) && !is.na(match("Screened", table = class(screened))) && !S4Vectors::isEmpty(screened)){
    screened = get_index(screened)[1:get_screened_nvar_from_config(trained)]

    #update
    newx = subset_features_def(x = newx, which = screened)
  }

  #--------------------------------------------------------------------------------------------#
  #check prediction type
  if(!missing(type) && (length(type) == length(trainedlist))){
    type = type[[iloop]]
  }

  #--------------------------------------------------------------------------------------------#
  #Predict
  log_trace(object = logger, message = paste(iloop, "Predict..."), sep = "", add.level = TRUE, add.time = TRUE)
  # if(!is.na(type)){
  #   pred = predict(object = trained, newx = newx, newoffset = newoffset, type = type, ...)
  # } else {
  #   pred = predict(object = trained, newx = newx, newoffset = newoffset, ...)#leave to default value
  # }
  pred = forecast(forecaster = forecaster, models = trained, newx = newx, type = type, newoffset = newoffset, ...)
  log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------------------------------------#
  #close
  close_con(logger)

  #--------------------------------------------------------------------------------------------#
  return(pred)
  # return(list(pred = pred, true = get_y(trainer.i), weights = get_weights(trainer.i)))
}


#'@keywords internal
get_resample_predlist = function(trainedlist, forecaster, newx, type, newoffset, ..., looper, logger = Logger(verbose = F), indices, screened = NULL){

  # if(length(newx)!=length(indices)){stop("List of 'newx' and 'indices' must have same length.\n")}

  predlist = loop(
    looper = looper,
    n.iter = length(indices),
    # .inorder = TRUE,
    .inorder = TRUE,
    fun = get_sample_predlist,
    trainedlist = trainedlist, forecaster = forecaster, newx = newx, type = type, newoffset = newoffset,
    logger = logger, indices = indices, screened = screened, ...)

  return(predlist)
}
