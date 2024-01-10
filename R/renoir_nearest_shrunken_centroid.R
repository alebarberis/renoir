#' @include classes_generics.R utils.R class_Trainer.R class_Trained.R class_TrainedList.R
NULL

#'Nearest Shrunken Centroid
#'
#'@description This function trains a nearest shrunken centroid classifier.
#'
#'@details Renoir interface to \code{\link[pamr]{pamr.train}}.
#'
#'@inheritParams train,Trainer,ANY-method
#'@param ... further arguments to \code{\link[pamr]{pamr.train}}
#'@inheritParams pamr::pamr.train
#'
#'@inherit train,Trainer,ANY-method return
#'
#'@seealso
#'\code{\link[pamr]{pamr.train}}
#'
#'@author Alessandro Barberis
renoir_nearest_shrunken_centroid = function(
  x, y, weights = NULL, offset = NULL, clean, keep.call,
  resp.type = c("binomial", "multinomial"),
  observations = NULL,
  features = NULL,
  learning.method,
  ...,
  threshold
){

  #--------------------------------------------------------------------------------------------#
  #match
  resp.type = match.arg(resp.type)

  #--------------------------------------------------------------------------------------------#
  #Subset observations
  x       = subset_observations(object = x, which = observations)
  y       = subset_observations(object = y, which = observations)
  offset  = subset_observations(object = offset, which = observations)
  weights = subset_observations(object = weights, which = observations)

  #subset features
  x = subset_features(object = x, which = features)

  #--------------------------------------------------------------------------------------------#
  # args = c(as.list(environment()), list(...));
  args = list(...)

  #--------------------------------------------------------------------------------------------#
  #if only 1 lambda is provided, use a path
  if(!is.null(threshold) && length(threshold)==1){
    #set flag
    check.obj = F
    split.obj = F#if one single lambda is provided, not split as the single lambda will be in trained config

    #keep data as should be final fit
    if(missing(clean)){clean = FALSE}
    if(missing(keep.call)){keep.call = T}
  } else {
    check.obj = FALSE
    #if multiple lambdas are provided, split
    split.obj = T
    #remove data as should be tuning fit
    if(missing(clean)){clean = TRUE}
    if(missing(keep.call)){keep.call = F}
  }

  #--------------------------------------------------------------------------------------------#
  #set fun
  fun = pamr::pamr.train

  #get formals
  args.def = names(formals(fun = fun))

  #get provided
  args = c(
    list(
      data = list(x = t(x), y = y),
      # weights = weights,
      # offset = offset,
      threshold = threshold
  ), args);

  #check passed arguments
  if(any(!(names(args) %in% args.def))){
    # warning("Provided argument not found in formals\n")
    warning("Provided argument not found in formals:",
            paste(setdiff(x = names(args), y = args.def), collapse = ", "),
            "\n")
  }

  #--------------------------------------------------------------------------------------------#
  #train
  trained = do.call(what = fun, args = args)

  #--------------------------------------------------------------------------------------------#
  #remove call from object (if present) to reduce space before splitting
  if(!keep.call){
    trained$call = NULL
  }

  #clean object to reduce space before splitting
  if(clean){
    #not implemented
  }

  #--------------------------------------------------------------------------------------------#
  #Split
  if(split.obj){
    trained = renoir_split_nsc(object = trained)
  }

  #check if class is RenoirSplitList
  if(!identical(class(trained), "RenoirSplitList")){
    trained = list(el1 = trained)
  }

  #--------------------------------------------------------------------------------------------#
  out = list()

  for(itrained in trained){

    #set
    out = c(
      out,
      Trained(
        fit = itrained,
        learning.method = "nsc",
        config = list(threshold = itrained$threshold),
        nfeatures = itrained$nonzero)
      )
  }

  #--------------------------------------------------------------------------------------------#
  if(length(out)>1){
    out = TrainedList(out)
  } else {
    out = out[[1]]
  }
  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}

#'Split Trained Object
#'
#'@description This function splits a trained object containing multiple
#'configurations into several objects, one for each configuration.
#'
#'@details The following elements stored in the object and linked to
#'the hyperparameter \code{threshold} are split:
#'yhat, prob, threshold, nonzero, errors
#'
#'@param object an object of class \code{pamtrained}
#'
#'@return An object of class \code{RenoirSplitList}, which is a list
#'containing the objects resulting from the split.
#'
#'@keywords internal
#'
#'@author Alessandro Barberis
renoir_split_nsc <- function(object){

  #get lambda
  threshold = object$threshold

  #get number of config
  ncf = length(threshold)

  if(ncf > 1){

    #check if is multi response
    is.multi = is.list(object$beta)

    #list
    out = list()

    #loop and split
    for(i in seq(ncf)){

      #subset
      iobj = object

      #subset
      iobj$yhat      = iobj$yhat[,i,drop=T]
      iobj$prob      = iobj$prob[,,i]
      iobj$threshold = iobj$threshold[i]
      iobj$nonzero   = iobj$nonzero[i]
      iobj$errors    = iobj$errors[i]

      #store
      out[[i]] = iobj

      #clean
      rm(iobj)
    }

    #add a class to recognise
    class(out) = c("RenoirSplitList")

  } else {
    out = object
  }

  return(out)
}


#'Features Recorder
#'
#'@description This function records the presence of the features
#'in an object of class \code{pamrtrained}.
#'
#'@details The presence of the features in the \code{pamrtrained}
#'object in input is recorded and returned in a named vector
#'per output response.
#'
#'If a feature is present in the model then the feature is returned with a \code{1}.
#'If a feature is missing in the model then the feature is returned with a \code{0}.
#'
#'@param obejct an object of class \code{pamrtrained}
#'
#'@return A list with one element per response, each element being
#'an integer vector of length equivalent to the number of features.
#'
#'@author Alessandro Barberis
record_nsc <- function(object){

  #number of features
  nvars = length(object$centroid.overall)
  if(isTRUE(is.null(nvars))){nvars = 1}

  #get names
  vars = names(object$centroid.overall)
  if(isTRUE(is.null(vars))){
    #try here
    vars = rownames(object$centroids)
  }

  #check feature names
  vars = create_varnames(nm = vars, n = nvars)

  #out
  out = stats::setNames(object = rep(x = 0, times = nvars), nm = vars)

  #get nonzero
  used = pamr::pamr.predict(fit = object, newx = NULL, threshold = object$threshold[1], type = "nonzero")

  #update
  out[used] = 1

  #return as list for common format across learning methods
  if(!is.list(out)){out = list(out)}

  return(out)
}


#'Features Marker
#'
#'@description This function marks the features of an object of class
#'\code{pamrtrained}.
#'
#'@details The features in the \linkS4class{Trained} object in input
#'are marked, i.e. a numerical value is assigned to each feature
#'based on the choice of \code{marking.system}.
#'
#'If \code{marking.system = "presence"}, a binary value of 1/0 is assigned
#'to the features by their presence in the model:
#'\code{1} is given to features that were used in the forest,
#'\code{0} otherwise.
#'
#'@param obejct an object of class \linkS4class{Trained}
#'@param marking.system a length-one character vector, indicating a strategy
#'for marking the features.
#'
#'@return A list with one element per response, containing the marks for the features
#'
#'@author Alessandro Barberis
mark_nsc <- function(object, marking.system = c("presence")){
  #match
  marking.system = match.arg(marking.system)

  #get fit
  object = get_fit(object)

  #--------------------------------------------------------------------------------------------#
  #Get feature names
  #number of features
  nvars = length(object$centroid.overall)
  if(isTRUE(is.null(nvars))){nvars = nrow(object$centroids)}

  #get names
  vars = names(object$centroid.overall)
  if(isTRUE(is.null(vars))){
    #try here
    vars = rownames(object$centroids)
  }

  #check feature names
  vars = create_varnames(nm = vars, n = nvars)

  #out
  out = stats::setNames(object = rep(x = 0, times = nvars), nm = vars)

  #get nonzero
  used = pamr::pamr.predict(fit = object, newx = NULL, threshold = object$threshold[1], type = "nonzero")

  #update
  out[used] = 1

  #--------------------------------------------------------------------------------------------#
  #to return a uniform output across learning methods, return a list
  if(!is.list(out)){out = list(out)}

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}


#'Forecaster for Nearest Shrunken Centroid Model Fits
#'
#'@description This function predicts values based on Nearest Shrunken
#'Centroid model objects.
#'
#'@param object an object of class \linkS4class{Trained}
#'@param newx the new values at which prediction are to be made
#'@param newoffset currently not used
#'@param type character specifying the type of prediction. Three options are available
#'\describe{
#'\item{\code{"response"}}{predicted values are on the same scale as the outcome,
#'e.g. for binomial the returned values are classes}
#'\item{\code{"class"}}{the class labels corresponding to the max probability}
#'\item{\code{"posterior"}}{posterior probabilities}
#'\item{\code{"centroid"}}{(unshrunken) class centroids}
#'\item{\code{"nonzero"}}{index of genes surviving the threshold}
#'}
#'@param target (optional) integer, index of the target class. If provided, only the
#'prediction for the selected class are returned (default to second level
#'of factor response). Set to \code{NULL} to return all classes. Used when
#'\code{type = "posterior"} or \code{type = "centroid"}
#'@param ... further arguments to \code{\link[pamr]{pamr.predict}}
#'
#'@inheritParams pamr::pamr.predict
#'
#'@return A vector containing the predictions.
#'
#'@seealso
#'\code{\link[pamr]{pamr.predict}}
#'
#'@keywords internal
#'@author Alessandro Barberis
forecast_by_nsc <- function(
  object, newx, type = c("response", "class", "posterior", "centroid", "nonzero"), newoffset,
  ...,
  threshold,
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
  args = c(list(fit = fit), list(...))
  #update
  if(!missing(newx)){args = c(args, list(newx = t(newx), type = type))}
  if(missing(threshold)){args = c(args, list(threshold = fit$threshold[1]))}
  #--------------------------------------------------------------------------------------------#
  #Use model to make prediction
  out = do.call(what = pamr::pamr.predict, args = args)

  #--------------------------------------------------------------------------------------------#
  #check
  if(isTRUE(is.integer(target) & target > 0)){
    if(isTRUE(type %in% c("posterior", "centroid"))){
      if(isTRUE(target <= ncol(out)))
        out = out[,target]
    }
  }

  #--------------------------------------------------------------------------------------------#
  #Return prediction
  return(out)
}


get_nsc_prediction_type_for_performance_metric  <- function(type.measure){
  out = switch(
    type.measure,
    # 'mse'                  = 'response',
    # 'msle'                 = 'response',
    # 'rmse'                 = 'response',
    # 'squared_error'        = 'response',
    # 'mae'                  = 'response',
    # 'mape'                 = 'response',
    # 'absolute_error'       = 'response',
    # 'ape'                  = 'response',
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
    'auc'                  = 'posterior',
    'roc'                  = 'posterior',
    NA
  )
  return(out)
}

which_least_complex_model_nsc <- function(models){

  #extract from trained list
  models = lapply(X = models, FUN = get_fit)

  #get data
  nzr = sapply(X = models, FUN = '[[', 'nonzero')
  err = sapply(X = models, FUN = '[[', 'errors')

  #df
  df = data.frame(nzr = nzr, err = err, id = seq(length(nzr)))

  #select model with minor number of training errors
  df = df[
    with(df, order(err, nzr)),
  ]

  #match
  id = df[1,"id"]

  #return
  return(id)
}


renoir_tuner_default_hyperparameters_nsc <- function(name, learning.method){

  #n
  out = list(
    threshold = seq(0, 2, length.out = 30)
  )

  #if name is provided
  if(!missing(name) && !is.null(name)){
    #get names
    lnames = names(out)
    #match
    name = match(x = name, table = lnames)
    #check
    if(!is.na(name)){
      out = out[[name]]
    }
  }

  return(out)
}
