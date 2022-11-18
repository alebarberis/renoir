#' @include classes_generics.R class_Logger.R class_Trained.R class_TrainedList.R class_Tuned.R
NULL

#' Recorder Class
#' An S4 class to represent a recorder.
#'
#' The object consists of 3 slots
#'
#' @slot id a length-one character vector
#' @slot recorder a function able to record the presence of a feature in a trained model
#' @slot logger a \linkS4class{Logger} object
#'
#' @author Alessandro Barberis
methods::setClass(
  Class = "Recorder",
  slots = c(
    id       = "character",
    recorder = "function",
    logger   = "Logger"
  )
)

methods::setMethod(f = "get_id",         signature = "Recorder", definition = function(object){methods::slot(object = object, name = 'id')})
# methods::setMethod(f = "get_parameters", signature = "Recorder", definition = function(object){methods::slot(object = object, name = 'parameters')})
methods::setMethod(f = "get_recorder",   signature = "Recorder", definition = function(object){methods::slot(object = object, name = 'recorder')})
methods::setMethod(f = "get_function",   signature = "Recorder", definition = function(object){methods::slot(object = object, name = 'recorder')})
methods::setMethod(f = "get_logger",     signature = "Recorder", definition = function(object){methods::slot(object = object, name = 'logger')})


#' Recorder Class Constructor
#'
#' Constructor for the S4 \linkS4class{Recorder} object.
#'
#' @param id the learning method name associated to this \linkS4class{Recorder}.
#' If learning method is one of the supported by renoir, the constructor will
#' automatically select a \code{recorder}. See \code{supported_learning_methods()}
#' for the supported methods.
#' @param recorder (optional) function to record if a predictor is present in the trained model.
#' Used if \code{id} is not one of the supported by renoir.
#' If \code{recorder} is provided it must conform to the renoir common interface,
#' and must have the following formal arguments:
#' \describe{
#'    \item{object}{a \linkS4class{Trained} object}
#'    \item{...}{additional arguments}
#' }
#' @param logger a \linkS4class{Logger}
#'
#' @return An object of class \linkS4class{Recorder}.
#'
#' @export
#'
#' @author Alessandro Barberis
Recorder <- function(
  id,
  recorder,
  logger  = Logger()
){

  if(missing(recorder) && (id %in% supported_learning_methods())){
    recorder = get_recorder_function(id)
  } else if(!missing(recorder)){
    if(id %in% supported_learning_methods()){
      stop(paste("'id' not valid, please change name. It must be different from the built-in renoir method names:\n",paste(renoir_learning_methods(), collapse = ", "),"\n"))
    }
  } else {
    stop("Please provide a valid 'id' and/or 'recorder'.\n")
  }

  methods::new(
    Class = "Recorder",
    id       = id,
    recorder = recorder,
    logger   = logger
  )
}

get_recorder_function <-function(id) {
  out = switch(
    id,
    lasso              = record_glmnet,
    ridge              = record_glmnet,
    elnet              = record_glmnet,
    elasticnet         = record_glmnet,
    relaxed_lasso      = record_glmnet,
    relaxed_ridge      = record_glmnet,
    relaxed_elnet      = record_glmnet,
    relaxed_elasticnet = record_glmnet,
    randomForest       = record_randomForest,
    xgbtree            = record_xgboost,
    xgblinear          = record_xgboost,
    gbm                = record_gbm,
    linear_SVM         = record_svm,
    polynomial_SVM     = record_svm,
    radial_SVM         = record_svm,
    sigmoid_SVM        = record_svm,
    linear_NuSVM       = record_svm,
    polynomial_NuSVM   = record_svm,
    radial_NuSVM       = record_svm,
    sigmoid_NuSVM      = record_svm,
    gknn               = record_gknn,
    nsc                = record_nsc
  )
  return(out)
}


#'@param obejct a \linkS4class{Trained} object
record_trained_glmnet <- function(object){out = record_glmnet(object = get_fit(object)); return(out)}

#'Features Recorder
#'
#'@description This function records the presence of the features
#'in an object of class \code{\link{glmnet}}.
#'
#'@details The presence of the features in the \linkS4class{glmnet}
#'object in input is recorded and returned in a named vector
#'per output response.
#'
#'If a feature is present in the model (i.e. the feature's coefficient
#'is different from zero) then the feature is returned with a \code{1}.
#'If a feature is missing in the model (i.e. the feature's coefficient
#'is equal to zero) then the feature is returned with a \code{0}.
#'
#'@param obejct an object of class \linkS4class{glmnet}
#'
#'@return A list with one element per response, each element being
#'an integer vector of length equivalent to the number of features.
#'
#'@author Alessandro Barberis
record_glmnet <- function(object){
  #get beta
  beta = object$beta
  #check if multi
  is.multi = is.list(beta)
  if(!is.multi){beta = list(beta)}

  #Record predictors
  out = lapply(X = beta, FUN = function(b){
    out = stats::setNames(object = vector(mode = "integer", length = nrow(b)), nm = rownames(b))
    #make sure 'explicit zeroes' are removed
    b = Matrix::drop0(x = b)
    #non-zero entries
    i = b@i
    #check
    if(!S4Vectors::isEmpty(i)){out[i+1] = 1}
    return(out)
  })

  return(out)
}

#'Features Recorder
#'
#'@description This function records the presence of the features
#'in an object of class \code{\link{randomForest}}.
#'
#'@details The presence of the features in the \linkS4class{randomForest}
#'object in input is recorded and returned in a named vector
#'per output response.
#'
#'If a feature is present in the model (i.e. the feature is used in
#'the random forest) then the feature is returned with a \code{1}.
#'If a feature is missing in the model (i.e. the feature is not used in
#'the random forest) then the feature is returned with a \code{0}.
#'
#'@param obejct an object of class \linkS4class{randomForest}
#'
#'@return A list with one element per response, each element being
#'an integer vector of length equivalent to the number of features.
#'
#'@author Alessandro Barberis
record_randomForest <- function(object){
  # #get fit
  # object = get_fit(object)
  #get importance
  rf.imp = randomForest::importance(object)

  #number of features
  nvars = nrow(rf.imp)

  #check names
  vars = rownames(rf.imp)
  if(is.null(vars) || identical(vars, as.character(seq(nvars)))){
    vars = names(object[["forest"]][["xlevels"]])
  }

  #check feature names
  vars = create_varnames(nm = vars, n = nvars)

  # #number of features
  # nvars = length(vars)

  #get used variables
  used = randomForest::varUsed(x = object, by.tree = F, count = F)

  #out
  out = stats::setNames(object = rep(x = 0, times = nvars), nm = vars)

  #update
  out[used] = 1

  #return as list for common format across learning methods
  if(!is.list(out)){out = list(out)}

  return(out)
}


#'Features Recorder
#'
#'@description This function records the presence of the features
#'in an object of class \code{\link{xgboost::xgb.Booster}}.
#'
#'@details The presence of the features in the \code{xgb.Booster}
#'object in input is recorded and returned in a named vector
#'per output response.
#'
#'If a feature is present in the model then the feature is returned with a \code{1}.
#'If a feature is missing in the model then the feature is returned with a \code{0}.
#'
#'@param obejct an object of class \code{xgb.Booster}
#'
#'@return A list with one element per response, each element being
#'an integer vector of length equivalent to the number of features.
#'
#'@author Alessandro Barberis
record_xgboost <- function(object){
  # #get fit
  # object = get_fit(object)
  #get importance
  rf.imp = xgboost::xgb.importance(model = object)

  #number of features
  nvars = object$nfeatures

  #check names
  vars = object$feature_names

  #check feature names
  vars = create_varnames(nm = vars, n = nvars)

  # #number of features
  # nvars = length(vars)

  #get used variables
  used = rf.imp$Feature

  #out
  out = stats::setNames(object = rep(x = 0, times = nvars), nm = vars)

  #update
  out[used] = 1

  #return as list for common format across learning methods
  if(!is.list(out)){out = list(out)}

  return(out)
}

#'Features Recorder
#'
#'@description This function records the presence of the features
#'in an object of class \code{\link{gbm::gbm.object}}.
#'
#'@details The presence of the features in the \code{gbm.object}
#'object in input is recorded and returned in a named vector
#'per output response.
#'
#'If a feature is present in the model then the feature is returned with a \code{1}.
#'If a feature is missing in the model then the feature is returned with a \code{0}.
#'
#'@param obejct an object of class \code{gbm.object}
#'
#'@return A list with one element per response, each element being
#'an integer vector of length equivalent to the number of features.
#'
#'@author Alessandro Barberis
record_gbm <- function(object){
  # #get fit
  # object = get_fit(object)
  #get importance
  # rf.imp = gbm::summary.gbm(object = object, plotit = F)
  rf.imp = gbm::relative.influence(object = object, n.trees = object$n.trees, scale. = FALSE, sort. = FALSE)

  #number of features
  nvars = length(rf.imp)

  #get names
  vars = names(rf.imp)

  #check feature names
  vars = create_varnames(nm = vars, n = nvars)

  #get used variables
  used = !is.na(rf.imp) & rf.imp!=0

  #out
  out = stats::setNames(object = rep(x = 0, times = nvars), nm = vars)

  #update
  out[used] = 1

  #return as list for common format across learning methods
  if(!is.list(out)){out = list(out)}

  return(out)
}


#'Features Recorder
#'
#'@description This function records the presence of the features
#'in an object of class \code{\link[e1071]{svm}}.
#'
#'@details The presence of the features in the \code{svm}
#'object in input is recorded and returned in a named vector
#'per output response.
#'
#'If a feature is present in the model then the feature is returned with a \code{1}.
#'If a feature is missing in the model then the feature is returned with a \code{0}.
#'
#'@param obejct an object of class \code{svm}
#'
#'@return A list with one element per response, each element being
#'an integer vector of length equivalent to the number of features.
#'
#'@author Alessandro Barberis
record_svm <- function(object){

  #check coefs
  hasCoefs = !is.null(object$coefs)

  if(isTRUE(hasCoefs)){
    rf.imp = t(object$coefs) %*% object$SV # weight vectors
    rf.imp <- apply(rf.imp, 2, function(v){sqrt(sum(v^2))})  # weight
  } else {
    #get data
    rf.imp = rep(x = NA, times = ncol(object$SV))

    #set names
    names(rf.imp) = colnames(object$SV)
  }

  #number of features
  nvars = length(rf.imp)

  #get names
  vars = names(rf.imp)

  #check feature names
  vars = create_varnames(nm = vars, n = nvars)

  #get used variables
  used = !is.na(rf.imp) & rf.imp!=0

  #out
  out = stats::setNames(object = rep(x = 0, times = nvars), nm = vars)

  #update
  out[used] = 1

  #return as list for common format across learning methods
  if(!is.list(out)){out = list(out)}

  return(out)
}

#'Features Recorder
#'
#'@description This function records the presence of the features
#'in an object of class \code{\link[e1071]{gknn}}.
#'
#'@details The presence of the features in the \code{gknn}
#'object in input is recorded and returned in a named vector
#'per output response.
#'
#'If a feature is present in the model then the feature is returned with a \code{1}.
#'If a feature is missing in the model then the feature is returned with a \code{0}.
#'
#'@param obejct an object of class \code{gknn}
#'
#'@return A list with one element per response, each element being
#'an integer vector of length equivalent to the number of features.
#'
#'@author Alessandro Barberis
record_gknn <- function(object){

  #number of features
  nvars = ncol(object$x)
  if(isTRUE(is.null(nvars))){nvars = 1}

  #get names
  vars = colnames(object$x)

  #check feature names
  vars = create_varnames(nm = vars, n = nvars)

  #out
  out = stats::setNames(object = rep(x = 1, times = nvars), nm = vars)

  #return as list for common format across learning methods
  if(!is.list(out)){out = list(out)}

  return(out)
}

#'@param features (optional) vector containing all the predictor names. If provided,
#'the results from the recorder function are checked and the missing features are added
#'with NA record
methods::setMethod(
  f = "record",
  signature = methods::signature(object ="Trained", recorder = "Recorder"),
  definition = function(
    object, recorder, features, ...){

    #--------------------------------------------------------------------------------------------#
    #get function
    recorder_fun = get_function(recorder)

    #--------------------------------------------------------------------------------------------#
    #record
    # out = recorder_fun(object, ...)
    out = recorder_fun(get_fit(object), ...)

    #--------------------------------------------------------------------------------------------#
    if(!missing(features) && !S4Vectors::isEmpty(features) && !all(is.na(out))){
      out = lapply(X = out, FUN = function(o, features){
        #set diff
        miss = setdiff(x = features, y = names(o))
        #check
        if(length(miss)>0){
          o = c(o, stats::setNames(object = rep(x = 0, times = length(miss)), nm = miss))
        }
        #order
        o = o[features]
        #return
        return(o)

      }, features = features)
    }

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

methods::setMethod(
  f = "record",
  signature = methods::signature(object ="Trained", recorder = "missing"),
  definition = function(
    object, recorder, features, ...){

    #--------------------------------------------------------------------------------------------#
    #get recorder
    #--------------------------------------------------------------------------------------------#
    #get id
    id = get_learning_method(object)

    #create recorder
    recorder = Recorder(id = id)

    #--------------------------------------------------------------------------------------------#
    #record
    out = record(object = object, recorder = recorder,  ...)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

methods::setMethod(
  f = "record",
  signature = methods::signature(object ="TrainedList", recorder = "Recorder"),
  definition = function(
    object, recorder, ...){

    #--------------------------------------------------------------------------------------------#
    #record
    out = lapply(X = object, FUN = record, recorder = recorder, ...)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

methods::setMethod(
  f = "record",
  signature = methods::signature(object ="Tuned", recorder = "Recorder"),
  definition = function(
    object, recorder, ...){

    #--------------------------------------------------------------------------------------------#
    #record
    out = record(object = get_model(object), recorder = recorder, ...)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

#Get features in the model
methods::setMethod(
  f = "features",
  signature = methods::signature(object ="Trained", recorder = "Recorder"),
  definition = function(
    object, recorder, type = c("all", "nonzero"), ...){

    type = match.arg(type)

    #--------------------------------------------------------------------------------------------#
    #record
    out = record(object = object, recorder = recorder, ...)

    #--------------------------------------------------------------------------------------------#
    #Get features
    out = lapply(X = out, FUN = function(recorded, type){
      #remove NA
      recorded = recorded[!is.na(recorded)]
      #remove 0
      if(identical(type, "nonzero")){
        recorded = recorded[recorded!=0]
      }
      #out
      o = names(recorded)
      #r
      return(o)
    }, type = type)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

methods::setMethod(
  f = "features",
  signature = methods::signature(object ="Trained", recorder = "missing"),
  definition = function(
    object, recorder, ...){

    #--------------------------------------------------------------------------------------------#
    #get recorder
    #--------------------------------------------------------------------------------------------#
    #get id
    id = get_learning_method(object)

    #create recorder
    recorder = Recorder(id = id)

    #--------------------------------------------------------------------------------------------#
    #record
    out = features(object = object, recorder = recorder, ...)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

get_nrecords <- function(records){

  if(!is.list(records)){records = list(records)}

  #remove
  out = lapply(X = records, FUN = function(recorded){
    #remove NA
    recorded = recorded[!is.na(recorded)]
    #remove 0
    recorded = recorded[recorded!=0]
    #out
    o = names(recorded)
    #r
    return(o)
  })

  #--------------------------------------------------------------------------------------------#
  #Get features
  out = unique(unlist(out))

  #--------------------------------------------------------------------------------------------#
  #number of features
  if(isTRUE(all(!is.na(out)))){
    out = length(out)
  } else {
    out = NA_integer_
  }

  #--------------------------------------------------------------------------------------------#
  return(out)
}

#Get features in the model
methods::setMethod(
  f = "nfeatures",
  signature = methods::signature(object ="Trained", recorder = "Recorder"),
  definition = function(
    object, recorder, ...){

    #--------------------------------------------------------------------------------------------#
    #record
    f = features(object = object, recorder = recorder, ...)

    #--------------------------------------------------------------------------------------------#
    #Get features
    f = unique(unlist(f))

    #--------------------------------------------------------------------------------------------#
    #number of features
    if(isTRUE(all(!is.na(f)))){
      out = length(f)
    } else {
      out = NA_integer_
    }

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)


methods::setMethod(
  f = "features",
  signature = methods::signature(object ="Tuned", recorder = "Recorder"),
  # signature = methods::signature(object ="Tuned", recorder = "ANY"),
  definition = function(
    object, recorder, ...){

    #--------------------------------------------------------------------------------------------#
    #record
    out = features(object = get_model(object), recorder = recorder, ...)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

methods::setMethod(
  f = "features",
  signature = methods::signature(object ="Tuned", recorder = "missing"),
  definition = function(
    object, recorder, ...){

    #get id
    id = get_learning_method(object)

    #create recorder
    recorder = Recorder(id = id)

    #--------------------------------------------------------------------------------------------#
    #record
    out = features(object = get_model(object), recorder = recorder, ...)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

methods::setMethod(
  f = "nfeatures",
  signature = methods::signature(object ="Tuned", recorder = "Recorder"),
  definition = function(
    object, recorder, ...){

    #--------------------------------------------------------------------------------------------#
    #record
    out = nfeatures(object = get_model(object), recorder = recorder, ...)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

# trainedlist = evaluated[[1]][[1]]@models[[1]]@trained
