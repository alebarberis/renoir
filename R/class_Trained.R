#'@include classes_generics.R
NULL

#' An S4 class to represent a renoir trained model
#'
#' The studies object consists of eight slots
#' @slot fit The trained object (can be of different class depending on the chosen learning method)
#' @slot learning.method The used learning method
#' @slot config A list containing the tested hyper-parameter configurations
# @slot screened (optional) a vector containing the indices of the selected variables from original input data
# if a feature screening was performed
#' @slot np a length-one integer vector
methods::setClass(
  Class = "Trained",
  slots = c(
    fit             = "ANY",
    config          = "list",
    learning.method = "character"
    # screened        = "Screened"
    # ,nfeatures      = "integer"
    ,np             = "integer"
  )
)


#' Constructor for the S4 Trained object.
#'
#' Constructor for the S4 \linkS4class{Trained} object.
Trained <- function(
  fit             = NULL,
  config          = list(),
  learning.method = character()
  # screened        = Screened()
  ,nfeatures      = integer()
){
  methods::new(
    Class = "Trained",
    fit             = fit,
    config          = config,
    learning.method = learning.method
    # screened        = screened
    ,np             = nfeatures
    # ,nfeatures      = nfeatures
  )
}


methods::setMethod("get_fit",    "Trained", function(object){ methods::slot(object = object, name = 'fit')})
methods::setMethod("get_config", "Trained", function(object){ methods::slot(object = object, name = 'config')})
methods::setMethod("get_learning_method", "Trained", function(object){ methods::slot(object = object, name = 'learning.method') })
methods::setMethod("get_nfeatures", "Trained", function(object){ methods::slot(object = object, name = 'np') })

methods::setMethod(f = "set_config", signature = "Trained", definition = function(object, value){ methods::slot(object = object, name = 'config') = value; return(object)})
methods::setMethod(f = "set_fit",    signature = "Trained", definition = function(object, value){ methods::slot(object = object, name = 'fit') = value; return(object)})
methods::setMethod(f = "set_learning_method",    signature = "Trained", definition = function(object, value){ methods::slot(object = object, name = 'learning.method') = value; return(object)})


methods::setMethod("get_learning",    "Trained", function(object){"training"})
methods::setMethod("get_screening",    "Trained", function(object){""})

methods::setMethod(
  f = "add_config_element",
  signature = methods::signature(object = "Trained", value = "list"),
  definition = function(object, value){
    #get
    config = get_config(object = object)
    #add
    config = c(config, value)
    #store
    methods::slot(object = object, name = 'config') = config;
    #return
    return(object)
  }
)

methods::setMethod(
  f = "del_config_element",
  signature = methods::signature(object = "Trained", value = "character"),
  definition = function(object, value){
    #get
    config = get_config(object = object)
    #check if present
    index = match(x = value, table = names(config))
    #remove
    if(!is.na(index)){
      config = config[-index]
    }
    #store
    methods::slot(object = object, name = 'config') = config;
    #return
    return(object)
  }
)

methods::setMethod(
  f = "get_config_element",
  signature = methods::signature(object = "Trained", value = "character"),
  definition = function(object, value){
    #get
    config = get_config(object = object)
    #check if present
    index = match(x = value, table = names(config))
    #remove
    if(!is.na(index)){
      out = config[[index]]
    } else {
      out = NA
    }
    #return
    return(out)
  }
)

methods::setMethod(
  f = "add_screened_nvar_to_config",
  signature = methods::signature(object = "Trained"),
  definition = function(object, value){
    value = list(renoirscreenednvar = value)
    object = add_config_element(object = object, value = value)
    return(object)
  }
)

methods::setMethod(
  f = "del_screened_nvar_from_config",
  signature = methods::signature(object = "Trained"),
  definition = function(object){
    object = del_config_element(object = object, value = "renoirscreenednvar")
    return(object)
  }
)

methods::setMethod(
  f = "get_screened_nvar_from_config",
  signature = methods::signature(object = "Trained"),
  definition = function(object){
    object = get_config_element(object = object, value = "renoirscreenednvar")
    return(object)
  }
)


# add_screened_nvar_to_config <- function(object, value){
#   value = list(renoirscreenednvar = value)
#   object = add_config_element(object = object, value = value)
#   return(object)
# }
#
# del_screened_nvar_from_config <- function(object){
#   object = del_config_element(object = object, value = "renoirscreenednvar")
#   return(object)
# }
#
# get_screened_nvar_from_config <- function(object){
#   object = get_config_element(object = object, value = "renoirscreenednvar")
#   return(object)
# }
#
# #'
# #' @rdname  predict-methods
# #' @aliases predict
# #'
# #' @param  object     object of type \code{Trained}.
# #'
# #' @export
# methods::setMethod(
#   f = "predict",
#   signature = "Trained",
#   definition = function(object, newx, type, newoffset, ...){
#
#     #----------------------------------------------------------------------#
#     #method
#     learning.method = get_learning_method(object = object)
#
#     #----------------------------------------------------------------------#
#     #setup type
#     if(missing(type)){
#       type = switch(
#         learning.method,
#         glmnet       = "link",
#         lasso        = "link",
#         ridge        = "link",
#         elnet        = "link",
#         randomForest = "response"
#       )
#     }
#
#     #----------------------------------------------------------------------#
#     #extract fit
#     fit = get_fit(object = object)
#
#     #----------------------------------------------------------------------#
#     #predict
#     # out = stats::predict(object = fit, ...)
#     # out = switch(
#     #   learning.method,
#     #   glmnet       = stats::predict(object = fit, newx = newx, type = type, newoffset = newoffset, ...),
#     #   lasso        = stats::predict(object = fit, newx = newx, type = type, newoffset = newoffset, ...),
#     #   ridge        = stats::predict(object = fit, newx = newx, type = type, newoffset = newoffset, ...),
#     #   elnet        = stats::predict(object = fit, newx = newx, type = type, newoffset = newoffset, ...),
#     #   randomForest = stats::predict(object = fit, newdata = newx, type = type, ...)
#     # )
#
#     args = get_predict_param(object = object, ...)
#
#     #update
#     args = c(list(object = fit, type = type), args)
#
#     if(learning.method %in% glmnet_learning_method_names()){
#
#       #update
#       if(!missing(newx)){args = c(args, list(newx = newx))}
#       if(!missing(newoffset)){args = c(args, list(newoffset = newoffset))}
#
#       out = do.call(what = stats::predict, args = args)
#
#       #check predicted
#       out = check_predicted_by_glmnet(object = out, type = type)
#
#     } else if(identical(learning.method, "randomForest")){
#       # out = randomForest:::predict.randomForest(object = fit, newdata = newx, type = type, ...)
#
#       if(!missing(newx)){args = c(args, list(newdata = newx))}
#
#       out = do.call(what = stats::predict, args = args)
#     } else {
#       stop("Learning method not supported.\n")
#     }
#
#     #----------------------------------------------------------------------#
#     return(out)
# })


#' Assess performance using test data
#' @param object an object of class \code{renoir.trained.cv.glmnet} containing the fitted data
#'
methods::setMethod(
  f = "assess",
  signature = methods::signature(object = "Trained"),
  function(object, newx, newy, weights = NULL, ...){

    #----------------------------------------------------------------------#
    #method
    learning.method = get_learning_method(object = object)

    #----------------------------------------------------------------------#
    #fit
    fit = get_fit(object = object)

    #----------------------------------------------------------------------#
    # out = switch(
    #   learning.method,
    #   glmnet = glmnet::assess.glmnet(object = fit, newx = newx, newy = newy, weights = weights,...),
    #   lasso  = glmnet::assess.glmnet(object = fit, newx = newx, newy = newy, weights = weights,...),
    #   ridge  = glmnet::assess.glmnet(object = fit, newx = newx, newy = newy, weights = weights,...),
    #   elnet  = glmnet::assess.glmnet(object = fit, newx = newx, newy = newy, weights = weights,...)
    # )

    #----------------------------------------------------------------------#
    #create args
    args = get_predict_param(object = object, ...)

    #assess
    out = switch(
      learning.method,
      glmnet = do.call(what = glmnet::assess.glmnet, args = c(list(object = fit, newx = newx, newy = newy, weights = weights), args)),
      lasso  = do.call(what = glmnet::assess.glmnet, args = c(list(object = fit, newx = newx, newy = newy, weights = weights), args)),
      ridge  = do.call(what = glmnet::assess.glmnet, args = c(list(object = fit, newx = newx, newy = newy, weights = weights), args)),
      elnet  = do.call(what = glmnet::assess.glmnet, args = c(list(object = fit, newx = newx, newy = newy, weights = weights), args))
    )

    #----------------------------------------------------------------------#
    return(out)
  }
)

#'Get parameters for predict method from Trained object
#'@return a list of parameters to use for predict method. The list can be empty
#'if no parameter was found in input data
get_predict_param <- function(object, ...){

  #----------------------------------------------------------------------#
  #default
  out = list()

  #----------------------------------------------------------------------#
  #method
  learning.method = get_learning_method(object = object)

  #----------------------------------------------------------------------#
  #extract config
  config = get_config(object = object)

  #----------------------------------------------------------------------#
  #get args
  args = list(...)

  #----------------------------------------------------------------------#
  if(learning.method %in% glmnet_learning_method_names()){
    #set default
    s = NULL

    #check lambda
    lambda = config$lambda
    if(length(lambda)==1){
      s = lambda
      #force args to be null
      args[c('lambda', 's')] = NULL
    }
    # } else {
    #   #check if lambda was given as argument
    #   index = match(x = c('lambda', 's'), table = names(args))
    #   #force to select 1
    #   index = index[index!=0][1]
    #   #check if not NA (no match)
    #   if(!is.na(index)){
    #     lambda = args[[index]]
    #     # if(length(lambda)==1){
    #     #   s = lambda
    #     # }
    #     s = lambda
    #   }
    # }

    out = list(s = s)

    #check if relaxed
    if(!is.null(get_fit(object)[['relaxed']])){
      #check gamma
      gamma.tmp = config$gamma
      if(!is.null(gamma.tmp)){
        #set default
        gamma = 1
        if(length(gamma.tmp)==1){
          gamma = gamma.tmp
          #force args to be null
          args[c('gamma')] = NULL
        }
        # } else {
        #   #check if gamma was given as argument
        #   index = match(x = 'gamma', table = names(args))
        #   #get
        #   index = index[index!=0]
        #   #check if no match
        #   if(length(index)==1){
        #     gamma = args[[index]]
        #   }
        # }

        out = c(out, list(gamma = gamma))
      }
    }
  }

  #----------------------------------------------------------------------#
  #update
  out = c(out, args)

  #----------------------------------------------------------------------#
  return(out)
}


#'Clean Trained objects
#'@description This function tries to clean the trained object to avoid that the final
#'object's size raises too much when the initial set of features and observations is big.
#'@param object a \linkS4class{Trained} object
#'@return the cleaned \linkS4class{Trained} object
methods::setMethod(
  f = "clean",
  signature = methods::signature(object = "Trained"),
  definition = function(object, rm.call = F, ...){

    #get learning method
    lm = get_learning_method(object = object)

    #clean
    object = switch(
      lm,
      lasso                = clean_trained_by_glmnet(object, rm.call = rm.call),
      ridge                = clean_trained_by_glmnet(object, rm.call = rm.call),
      elasticnet           = clean_trained_by_glmnet(object, rm.call = rm.call),
      relaxed_lasso        = clean_trained_by_glmnet(object, rm.call = rm.call),
      relaxed_ridge        = clean_trained_by_glmnet(object, rm.call = rm.call),
      relaxed_elasticnet   = clean_trained_by_glmnet(object, rm.call = rm.call),
      object
    )
    #return
    return(object)
  }
)

#'Clean glmnet objects
#'@description This function remove the \code{x} from the \code{call}
#'slots of the trained object. It is needed to avoid that the final
#'object's size raises too much when the initial set of features and observations is big.
#'@param object a \linkS4class{Trained} object
#'@return the cleaned \linkS4class{Trained} object
#'@keywords internal
clean_trained_by_glmnet <- function(object, rm.call = F){
  #get fit
  fit = get_fit(object = object)
  #clean
  fit = clean_glmnet(fit, rm.call = rm.call)
  #update
  object = set_fit(object = object, value = fit)
  #return
  return(object)
}

is.Trained <- function(object){
  out = !is.na(match("Trained", table = class(object)))
  return(out)
}




#'Summary Table of a Trained Object
#'
#'@description
#'This function produces a summary table of an object of class \linkS4class{Trained}.
#'
#'@param object an object of class \linkS4class{Trained}
#@param show.config logical, whether to report information on the configuration
#'
#'@return A data frame with the following elements:
#'\describe{
#'\item{\code{learning}}{character string, the learning method}
#'\item{\code{nfeatures}}{integer, the number of features}
#'\item{\code{configuration}}{string, the configuration, i.e. the fixed hyperparameters of the model}
#'}
#'
#'@seealso
#'\code{\link{summary_table.Renoir}},
#'\code{\link{summary_table.Evaluated}},
#'\code{\link{summary_table.EvaluatedList}},
#'\code{\link{summary_table.Tested}}
#'\code{\link{summary_table.TestedList}},
#'\code{\link{summary_table.Tuned}}
#'
#'@author Alessandro Barberis
summary_table.Trained <- function(object){

  #--------------------------------------------------------------------------------------------#
  #get learning
  learning = get_learning_method(object = object)

  #--------------------------------------------------------------------------------------------#
  #get np
  np = get_nfeatures(object)

  #--------------------------------------------------------------------------------------------#
  #output
  out = data.frame(
    learning         = learning,
    nfeatures        = np,
    stringsAsFactors = F
  )

  #--------------------------------------------------------------------------------------------#
  #Configuration
  config = get_config(object)
  config.names = names(config);
  config = paste(sapply(X = config.names, FUN = function(name, config){paste0(name, ": ", config[[name]])}, config = config), collapse = "<br>");

  #--------------------------------------------------------------------------------------------#
  #update output
  out$configuration = config

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}

#'Summary Table of a Trained Object
#'
#@describeIn summary_table Summary of an object of class \linkS4class{Trained}
#'
#'@inheritParams summary_table.Trained
#'@inherit summary_table.Trained return
#'
#'@author Alessandro Barberis
#'@export
methods::setMethod(
  f = "summary_table",
  signature = methods::signature(object = "Trained"),
  definition = summary_table.Trained
)


