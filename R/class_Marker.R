#' @include classes_generics.R class_Logger.R class_Marked.R class_MarkedList.R class_Trained.R class_Tuned.R
NULL

#' Marker Class
#' An S4 class to represent a features marker.
#'
#' The object consists of 3 slots
#'
#' @slot id a length-one character vector
#' @slot marker a function able to assign a numerical value to the features in a trained model
#' @slot parameters a list
#' @slot logger a \linkS4class{Logger} object
#'
#' @author Alessandro Barberis
methods::setClass(
  Class = "Marker",
  slots = c(
    id     = "character",
    marker = "function",
    parameters = "list",
    logger = "Logger"
  )
)

methods::setMethod(f = "get_id",         signature = "Marker", definition = function(object){methods::slot(object = object, name = 'id')})
methods::setMethod(f = "get_parameters", signature = "Marker", definition = function(object){methods::slot(object = object, name = 'parameters')})
methods::setMethod(f = "get_marker",     signature = "Marker", definition = function(object){methods::slot(object = object, name = 'marker')})
methods::setMethod(f = "get_function",   signature = "Marker", definition = function(object){methods::slot(object = object, name = 'marker')})
methods::setMethod(f = "get_logger",     signature = "Marker", definition = function(object){methods::slot(object = object, name = 'logger')})


#' Marker Class Constructor
#'
#' @description
#' Constructor for the S4 \linkS4class{Marker} object.
#'
#' @param id the learning method
#' @param marker (optional) function able to assign a numerical value to
#' the features in a trained model. Used if \code{id} is not one of the supported by renoir.
#' If provided, it must return a list with the number of elements equivalent to the number
#' of responses in the outcome variable.
#' @param parameters list containing the parameters to fix for the marking
#' @param logger a \linkS4class{Logger}
#'
#' @return An object of class \linkS4class{Marker}.
#'
#' @export
#'
#' @author Alessandro Barberis
Marker <- function(
  id,
  marker,
  parameters,
  logger  = Logger()
){

  if(missing(marker) && (id %in% supported_learning_methods())){
    marker = get_marker_function(id)
  } else if(!missing(marker)){
    if(id %in% supported_learning_methods()){
      stop(paste("'id' not valid, please change name. It must be different from the built-in renoir method names:\n",paste(renoir_learning_methods(), collapse = ", "),"\n"))
    }
  } else {
    stop("Please provide a valid 'id' and/or 'marker'.\n")
  }

  #Check provided function
  check_provided_marker_function(marker)

  #Check parameters
  if(missing(parameters) || is.null(parameters) || length(parameters)==0){
    # parameters = list()
    parameters = renoir_marker_default_parameters(id)
  }


  methods::new(
    Class = "Marker",
    id     = id,
    marker = marker,
    parameters = parameters,
    logger = logger
  )
}


get_marker_function <-function(id) {
  out = switch(
    id,
    lasso              = mark_glmnet,
    ridge              = mark_glmnet,
    elnet              = mark_glmnet,
    elasticnet         = mark_glmnet,
    relaxed_lasso      = mark_glmnet,
    relaxed_ridge      = mark_glmnet,
    relaxed_elnet      = mark_glmnet,
    relaxed_elasticnet = mark_glmnet,
    randomForest       = mark_randomForest,
    xgbtree            = mark_xgbtree,
    xgblinear          = mark_xgblinear,
    gbm                = mark_gbm,
    linear_SVM         = mark_svm,
    polynomial_SVM     = mark_svm,
    radial_SVM         = mark_svm,
    sigmoid_SVM        = mark_svm,
    linear_NuSVM       = mark_svm,
    polynomial_NuSVM   = mark_svm,
    radial_NuSVM       = mark_svm,
    sigmoid_NuSVM      = mark_svm,
    gknn               = mark_gknn,
    nsc                = mark_nsc,
    mark_default
  )
  return(out)
}


mark_default <- function(object, ...){out = NA;return(out)}



#'Features Marker
#'
#'@description This function marks the features of an object of class
#'\code{\link[e1071]{gknn}}.
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
mark_gknn <- function(object, marking.system = c("presence")){
  #match
  marking.system = match.arg(marking.system)

  #get fit
  object = get_fit(object)

  #--------------------------------------------------------------------------------------------#
  #Get feature names
  #number of features
  nvars = ncol(object$x)

  #check names
  vars = colnames(object$x)

  #check feature names
  vars = renoir:::create_varnames(nm = vars, n = nvars)

  #--------------------------------------------------------------------------------------------#
  #out
  out = stats::setNames(object = rep(x = 1, times = nvars), nm = vars)

  #--------------------------------------------------------------------------------------------#
  #to return a uniform output across learning methods, return a list
  if(!is.list(out)){out = list(out)}

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}

#'Features Marker
#'
#'@description This function marks the features of an object of class
#'\code{\link[e1071]{svm}}.
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
#'If \code{marking.system = "weights"}, the feature weights are used
#'as marks.
#'
#'@param obejct an object of class \linkS4class{Trained}
#'@param marking.system a length-one character vector, indicating a strategy
#'for marking the features.
#'
#'@return A list with one element per response, containing the marks for the features
#'
#'@author Alessandro Barberis
mark_svm <- function(object, marking.system = c("presence", "weights")){
  #match
  marking.system = match.arg(marking.system)

  #get fit
  object = get_fit(object)

  #--------------------------------------------------------------------------------------------#
  #Get feature names
  #number of features
  nvars = ncol(object$SV)

  #check names
  vars = colnames(object$SV)

  #check feature names
  vars = renoir:::create_varnames(nm = vars, n = nvars)

  #--------------------------------------------------------------------------------------------#
  #out
  out = stats::setNames(object = rep(x = 0, times = nvars), nm = vars)

  #--------------------------------------------------------------------------------------------#
  #get weights
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

  #--------------------------------------------------------------------------------------------#
  #get used variables
  used = !is.na(rf.imp) & rf.imp!=0

  #get value
  o = switch(
    marking.system,
    'presence' = 1,
    'weights'  = rf.imp[used]
  )

  #update
  out[used] = o

  #--------------------------------------------------------------------------------------------#
  #to return a uniform output across learning methods, return a list
  if(!is.list(out)){out = list(out)}

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}


#'Features Marker
#'
#'@description This function marks the features of an object of class
#'\code{\link[gbm]{gbm.object}}.
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
#'If \code{marking.system = "influence"}, the relative influence of each
#'variable in reducing the loss function in the gbm object is used.
#'
#'If \code{marking.system = "normalized_influence"}, the relative influence
#'normalized to sum to 100 is used.
#'
#If \code{marking.system = "permutation"}, the reduction in predictive
#performance when randomly permuting each predictor variable at a time is used.
#As reported in the \code{gbm} documentation, this is similar to the variable
#importance measures Breiman uses for random forests, with the difference that
#gbm currently computes the measures using the entire training dataset
#(not the out-of-bag observations).
#'
#'@param obejct an object of class \linkS4class{Trained}
#'@param marking.system a length-one character vector, indicating a strategy
#'for marking the features. Available options are \code{"presence"} (default),
#'\code{"influence"}, and \code{"normalized_influence"}
#\code{"influence"}, and \code{"permutation"}
#'
#'@return A list with one element per response, containing the marks for the features
#@return a named vector containing the feature marks
#'
#'@seealso
#'\code{\link[gbm]{relative.influence}},
#'\code{\link[gbm]{permutation.test.gbm}}
#\code{\link[gbm]{gbm.loss}}
#'
#'@author Alessandro Barberis
# mark_gbm <- function(object, marking.system = c("presence", "influence", "permutation"), ...){
mark_gbm <- function(object, marking.system = c("presence", "influence", "normalized_influence")){
  #match
  marking.system = match.arg(marking.system)

  #get fit
  object = get_fit(object)

  #--------------------------------------------------------------------------------------------#
  #Get feature names

  #get importance
  rf.imp = switch(
    marking.system,
    'presence'             = gbm::relative.influence(object = object, n.trees = object$n.trees, scale. = FALSE, sort. = FALSE),
    'influence'            = gbm::relative.influence(object = object, n.trees = object$n.trees, scale. = FALSE, sort. = FALSE),
    'normalized_influence' = gbm::relative.influence(object = object, n.trees = object$n.trees, scale. = TRUE, sort. = FALSE)
  )

  #number of features
  nvars = length(rf.imp)

  #check names
  vars = names(rf.imp)

  #check feature names
  vars = renoir:::create_varnames(nm = vars, n = nvars)

  #--------------------------------------------------------------------------------------------#
  #out
  out = stats::setNames(object = rep(x = 0, times = nvars), nm = vars)

  #--------------------------------------------------------------------------------------------#
  #get used variables
  used = !is.na(rf.imp) & rf.imp!=0

  #get value
  o = switch(
    marking.system,
    'presence'             = 1,
    'influence'            = rf.imp[used],
    'normalized_influence' = rf.imp[used]
  )

  #update
  out[used] = o

  #--------------------------------------------------------------------------------------------#
  #to return a uniform output across learning methods, return a list
  if(!is.list(out)){out = list(out)}

  #--------------------------------------------------------------------------------------------#
  # out = NA
  return(out)
}


#'Features Marker
#'
#'@description This function marks the features of an object of class
#'\code{\link{xgb.Booster}}.
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
#'If \code{marking.system = "coefficient"}, the linear coefficients of the
#'features are used as marks.
#'
#'@param obejct an object of class \linkS4class{Trained}
#'@param marking.system a length-one character vector, indicating a strategy
#'for marking the features. Available options are \code{"presence"} (default) and
#'\code{"coefficient"}
#'
#'@return A list with one element per response, containing the marks for the features
#'
#'@author Alessandro Barberis
mark_xgblinear <- function(object, marking.system = c("presence", "coefficient")){
  #match
  marking.system = match.arg(marking.system)

  #get fit
  object = get_fit(object)

  #--------------------------------------------------------------------------------------------#
  #Get feature names

  #get importance
  rf.imp = xgboost::xgb.importance(model = object)

  #number of features
  nvars = object$nfeatures

  #check names
  vars = object$feature_names

  #check feature names
  vars = create_varnames(nm = vars, n = nvars)

  #--------------------------------------------------------------------------------------------#
  #out
  out = stats::setNames(object = rep(x = 0, times = nvars), nm = vars)

  #--------------------------------------------------------------------------------------------#
  # Variables used in a random forest
  if(identical(marking.system, "presence")){
    #returns a vector of integer indices giving the variables used in the entire forest
    used = rf.imp$Feature

    #update
    out[used] = 1
  } else if(identical(marking.system, "coefficient")){
    used = rf.imp$Weight

    #returns a integer vector containing frequencies that variables are used in the forest
    o = rf.imp$Frequency

    #update
    out[used] = o
  } else {
    stop("Problem while computing the feature marks for xgblinear.")
  }

  #--------------------------------------------------------------------------------------------#
  #to return a uniform output across learning methods, return a list
  if(!is.list(out)){out = list(out)}

  #--------------------------------------------------------------------------------------------#
  # out = NA
  return(out)
}

#'Features Marker
#'
#'@description This function marks the features of an object of class
#'\code{\link{xgb.Booster}}.
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
#'If \code{marking.system = "frequency"}, the frequencies that features
#'appear in trees are used as marks.
#'
#'@param obejct an object of class \linkS4class{Trained}
#'@param marking.system a length-one character vector, indicating a strategy
#'for marking the features. Available options are \code{"presence"} (default) and
#'\code{"frequency"}
#'
#'@return A list with one element per response, containing the marks for the features
#@return a named vector containing the feature marks
#'
#'@author Alessandro Barberis
mark_xgbtree <- function(object, marking.system = c("presence", "frequency")){
  #match
  marking.system = match.arg(marking.system)

  #get fit
  object = get_fit(object)

  #--------------------------------------------------------------------------------------------#
  #Get feature names

  #get importance
  rf.imp = xgboost::xgb.importance(model = object)

  #number of features
  nvars = object$nfeatures

  #check names
  vars = object$feature_names

  #check feature names
  vars = create_varnames(nm = vars, n = nvars)

  #--------------------------------------------------------------------------------------------#
  #out
  out = stats::setNames(object = rep(x = 0, times = nvars), nm = vars)

  #--------------------------------------------------------------------------------------------#
  # Variables used in a random forest
  if(identical(marking.system, "presence")){
    #returns a vector of integer indices giving the variables used in the entire forest
    used = rf.imp$Feature

    #update
    out[used] = 1
  } else if(identical(marking.system, "frequency")){
    used = rf.imp$Feature

    #returns a integer vector containing frequencies that variables are used in the forest
    o = rf.imp$Frequency

    #update
    out[used] = o
  } else {
    stop("Problem while computing the feature marks for xgbtree.")
  }

  #--------------------------------------------------------------------------------------------#
  #to return a uniform output across learning methods, return a list
  if(!is.list(out)){out = list(out)}

  #--------------------------------------------------------------------------------------------#
  # out = NA
  return(out)
}


#'Features Marker
#'
#'@description This function marks the features of an object of class
#'\code{\link{randomForest}}.
#'
#'@details The features in the \linkS4class{Trained} object in input
#'are marked, i.e. a numerical value is assigned to each feature
#'based on the choice of \code{marking.system}.
#'
#'If \code{marking.system = "presence"}, a binary value of 1/0 is assigned
#'to the features by their presence in the model:
#'\code{1} is given to features that were used in the random forest,
#'\code{0} otherwise.
#'
#'If \code{marking.system = "frequency"}, the frequencies that features
#'appear in trees are used as marks.
#'
#'@param obejct an object of class \linkS4class{Trained}
#'@param marking.system a length-one character vector, indicating a strategy
#'for marking the features. Available options are \code{"presence"} (default) and
#'\code{"frequency"}
#'
#'@return A list with one element per response, containing the marks for the features
#@return a named vector containing the feature marks
#'
#'@author Alessandro Barberis
mark_randomForest <- function(object, marking.system = c("presence", "frequency")){
  #match
  marking.system = match.arg(marking.system)

  #get fit
  object = get_fit(object)

  #--------------------------------------------------------------------------------------------#
  #Get feature names

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

  #--------------------------------------------------------------------------------------------#
  #out
  out = stats::setNames(object = rep(x = 0, times = nvars), nm = vars)

  #--------------------------------------------------------------------------------------------#
  # Variables used in a random forest
  if(identical(marking.system, "presence")){
    #returns a vector of integer indices giving the variables used in the entire forest
    used = randomForest::varUsed(x = object, by.tree = F, count = F)

    #update
    out[used] = 1
  } else {
    used = randomForest::varUsed(x = object, by.tree = F, count = F)

    #returns a integer vector containing frequencies that variables are used in the forest
    o = randomForest::varUsed(x = object, by.tree = F, count = T)

    #update
    out[used] = o
  }

  #--------------------------------------------------------------------------------------------#
  #to return a uniform output across learning methods, return a list
  if(!is.list(out)){out = list(out)}

  #--------------------------------------------------------------------------------------------#
  # out = NA
  return(out)
}

#'Features Marker
#'
#'@description This function marks the features of an object of class
#'\code{\link{glmnet}}.
#'
#'@details The features in the \linkS4class{Trained} object in input
#'are marked, i.e. a numerical value is assigned to each feature
#'based on the choice of \code{marking.system}.
#'
#'If \code{marking.system = "presence"}, a value of 1/-1/0 is assigned
#'to the features by their presence in the model:
#'\code{1} is given to features with a coefficient > 0,
#'\code{-1} is given to features with a coefficient < 0,
#'\code{0} otherwise.
#'
#'If \code{marking.system = "coefficient"}, the coefficient of the feature
#'in the model is used as mark.
#'
#'@param obejct an object of class \linkS4class{Trained}
#'@param marking.system a length-one character vector, indicating a strategy
#'for marking the features. Available options are \code{"presence"} (default) and
#'\code{"coefficient"}
#'
#'@return A list with one element per response, containing the marks for the features
#'
#'@author Alessandro Barberis
mark_glmnet <- function(object, marking.system = c("presence", "coefficient")){
  #match
  marking.system = match.arg(marking.system)

  #get fit
  object = get_fit(object)
  #get beta
  beta = object$beta
  #check if multi
  is.multi = is.list(beta)
  if(!is.multi){beta = list(beta)}

  #Record predictors
  out = lapply(X = beta, FUN = function(b, marking.system){
    #output
    out = stats::setNames(object = as.vector(b), nm = rownames(b))
    if(identical(marking.system, "presence")){
     out[out>0] = 1
     out[out<0] = -1
    }
    return(out)
  }, marking.system = marking.system)

  #return
  return(out)
}


check_provided_marker_function <- function(f){

  if(missing(f)){
    stop("'marker' is missing with no default.\n")
  } else {
    #needed formals
    formals.def = c("object")

    #get formals
    formals.fun = names(formals(fun = f))

    #check
    # if(any(!(formals.fun %in% formals.def))){
    if(any(!(formals.def %in% formals.fun))){

      stop(paste0("Provided marker function without required formal arguments. Function interface must match renoir requirements. Try with:\n
      function(", paste0(formals.def, collapse = ", "),", ...){
        #Extract data from Trained object
        #fit = get_fit(object)

        #Assign an importance score to each predictor
        #YOUR CODE

        #Return output
        return(out)
      }
      \n"))
    }
  }
}


#'@param features (optional) vector containing all the predictors. If provided,
#'the results from the marker function are checked and the missing features are added
#'with NA mark
methods::setMethod(
  f = "mark",
  signature = methods::signature(object ="Trained"),
  definition = function(
    object,
    marker,
    features,
    weights,
    ...){

    #get parameters
    parameters = get_parameters(marker)

    #check
    parameters = check_parameters(parameters, ...)

    #merge
    args = c(parameters, list(...))

    #--------------------------------------------------------------------------------------------#
    #get function
    marker_fun = get_function(marker)

    #--------------------------------------------------------------------------------------------#
    # clean
    rm(parameters)

    #mark
    out = do.call(what = marker_fun, args = c(list(object = object), args))

    #--------------------------------------------------------------------------------------------#
    #check
    if(!missing(features) && !S4Vectors::isEmpty(features) && !all(is.na(out))){
      out = lapply(X = out, FUN = function(o, features){
        #set diff
        miss = setdiff(x = features, y = names(o))
        #check
        if(length(miss)>0){
          o = c(o, stats::setNames(object = rep(x = NA, times = length(miss)), nm = miss))
        }
        #order
        o = o[features]
        #return
        return(o)

      }, features = features)
    }

    #--------------------------------------------------------------------------------------------#
    #weights
    if(isTRUE(!missing(weights) && !S4Vectors::isEmpty(weights))){
      if(length(out)!=length(weights)){
        warning("'weights' length not matching the features length: marking computed without weights.\n")
      } else {
        for(i in seq(length(out))){
          marks = out[[i]]
          w = weights[[i]]

          #check names
          hasnames = !is.null(names(marks))
          if(hasnames){
            inter = intersect(names(w), names(marks))
            if(length(inter)>0){
              out[[i]] = marks[inter] * w[inter]
            } else {warning("'weights' not matching feature names: marking computed without weights.\n")}
          } else {
            warning("Missing names: marking computed with weights by position.\n")
            out[[i]] = marks * w
          }
        }
      }
    }


    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)

methods::setMethod(
  f = "mark",
  signature = methods::signature(object ="Tuned"),
  definition = function(
    object,
    marker,
    weights,
    ...){

    #--------------------------------------------------------------------------------------------#
    #check weights
    if(missing(weights)){weights = get_stability(object)}

    #--------------------------------------------------------------------------------------------#
    #mark
    out = mark(object = get_model(object), marker = marker, weights = weights, ...)
    # out = mark(object = get_model(object), marker = marker, ...)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

methods::setMethod(
  f = "mark",
  signature = methods::signature(object ="list"),
  definition = function(
    object,
    marker,
    # weights = NULL,
    ...){

    #--------------------------------------------------------------------------------------------#
    #apply
    out = lapply(X = object, FUN = mark, marker = marker, ...)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)



#
#
# #' Marker Class
# #' An S4 class providing the methods to perform a resampled tuning and validation.
# #'
# #' The object consists of 2 slots
# #' @slot marking.system a marking system. Two systems are available for assigning a mark:
# #' \describe{
# #'   \item{\code{presence}}{a binary variable (\code{presence = 0}
# #'   or \code{presence = 1}) multiplied by the sign of the coefficient is used in the computation of the marks}
# #'   \item{\code{coefficient}}{the normalised coefficient of a feature,
# #'   i.e. the coefficient of the feature divided by the maximum absolute value
# #'of the coefficients of the features for that model so as to have a
# #'value ranging \eqn{[-1,1]}, is used in the computation of the marks}
# #' }
# #' @slot logger a \linkS4class{Logger}
# #' @author Alessandro Barberis
# methods::setClass(
#   Class = "Marker",
#   slots = c(
#     marking.system = "character",
#     logger         = "Logger"
#   )
# )
#
# #' Marker Class Constructor
# Marker <- function(
#   marking.system = c("presence", "coefficient"),
#   logger    = Logger()
# ){
#
#   marking.system = match.arg(marking.system)
#
#   methods::new(
#     Class = "Marker",
#     marking.system = marking.system,
#     logger         = logger
#   )
# }
#
# methods::setMethod(f = "get_marking_system", signature = methods::signature(object = "Marker"), definition = function(object){methods::slot(object = object, name = 'marking.system')})
# methods::setMethod(f = "get_logger",         signature = methods::signature(object = "Marker"), definition = function(object){methods::slot(object = object, name = 'logger')})
#
# #'Features Importance
# #'@description This function computes a significance score for each feature.
# #'@section Details:
# #'A feature significance score is computed as the sum of the normalised
# #'coefficient of the feature in the model multiplied by a weight for such model.
# #'The formula for the i-th feature is:
# #'
# #'\deqn{score_{i} = \frac{1}{s}\sum_{j=1}^{n} weight_{j}*coefficient_{j} ,}
# #'
# #'where \eqn{n} is the total number of models computed across the different
# #'training set sizes, \eqn{coefficient_{j}} is the normalised coefficient
# #'(i.e. the coefficient of the feature divided by the maximum absolute value
# #'of the coefficients of the features for that model so as to have a
# #'value ranging \eqn{[-1,1]}), and \eqn{s} is the sum of weights.
# #'The \eqn{weight} for a model is obtained by dividing
# #'the multiplicative inverse of the squared prediction error of the model by
# #'the max value across all models, so that models with better prediction
# #'accuracy have higher weight:
# #'
# #'\deqn{weight_{j} = \frac{w_{j}}{w_{best}},}
# #'
# #'where \eqn{w_{j}} is the multiplicative inverse of the squared prediction error of
# #'the model:
# #'
# #'\deqn{w_{j} = \frac{1}{err_{j}^2},}
# #'
# #'and
# #'
# #'\deqn{w_{best} = \max{w_{1},...,w_{j},...,w_{n}}.}
# #'
# #'Note that for the \code{auc} accuracy measure, we consider the prediction error
# #'as \eqn{err_{j} = 1 - auc_{j}}.
# #'
# #'By selecting \code{use = "presence"}, the \eqn{coefficient_{j}}
# #'term in the computation of the significance score is exchanged
# #'for \eqn{presence_{j}*sign_{j}}, where \eqn{presence_{j}} stands for the
# #'presence of the feature in the j-th model and \eqn{sign_{j}}
# #'is the sign of the coefficient of the i-th feature in the j-th model.
# #'@param object a \linkS4class{Marker} object
# #'@param learned a \linkS4class{LearnedList} object
# #'@param config which configuration to consider
# #'@param set the set of data used for the computation of the accuracy measures. For example,
# #'if \code{set = "test"} the accuracy measures to be used in the computation of the
# #'features importance are the ones calculated on the test set.
# methods::setMethod(
#   f = "mark",
#   signature = methods::signature(object = "Marker",
#                                  learned      = "LearnedList",
#                                  set          = "character",
#                                  config       = "character",
#                                  type.measure = "character"),
#   function(object,
#            learned,
#            set = c('test', 'train', 'full'),
#            config = c("opt", "1se"),
#            type.measure){
#
#
#     #--------------------------------------------------------------------------------------------#
#     #Type
#     set              = match.arg(set)
#     config           = match.arg(config)
#
#     recruitment.term = get_marking_system(object)
#
#     #--------------------------------------------------------------------------------------------#
#     #1) Get the accuracy measures
#     #Get the tested list
#     tested = get_tested(learned)
#     #Get the performance measures for each set size
#     # performance = lapply(X = tested, FUN = get_test_measure, set = set, config = config, type.measure = type.measure)
#     performance = unlist(sapply(X = tested, FUN = get_test_measure, set = set, config = config, type.measure = type.measure))
#
#     #--------------------------------------------------------------------------------------------#
#     #2) Compute the weights
#
#     #weights
#     w = performance
#
#     #If AUC measure, compute 1 - auc so to have a metric where 0
#     #corresponds to best performance
#     if(tolower(x = type.measure) %in% c("auc", "c")){ w = 1 - w}
#
#     #Add 1 to avoid 1/0 in next computation
#     w = w + 1
#
#     #get squared error
#     w = w^2;
#
#     # #Check if any is 0
#     # test = which(w == 0)
#     #
#     # #Set min value (needed to avoid 1/0 in next computation)
#     # # w[test] = .Machine$double.xmin;
#     # if(length(test)>0) {w[test] = min(min(abs(w[-test] - 0.001), na.rm = T), 1e-03, na.rm = T)};
#
#     #reverse
#     w = 1 / w;
#
#     # #get best performance measure (as we computed 1/err,
#     # #the higher the value, the better the prediction)
#     # b = max(w, na.rm = TRUE);
#     #
#     # #divide weights by best measure to have weights range between 0 and 1
#     # w = w / b;
#
#     #--------------------------------------------------------------------------------------------#
#     #3) Get all coefficients
#
#     #get tuned data
#     tuned_list = get_tuned(object = learned)
#
#     #get coefficients
#     all_coefs = lapply(X = tuned_list, FUN = predict, config = config, type = "coefficients")
#
#     #Get them as a list, where each element is the result of a run
#     all_coefs = unlist(x = all_coefs, recursive = F)
#
#     #--------------------------------------------------------------------------------------------#
#     #4) Check if we have a list of coefficients (one for each response in multi-response analysis)
#     is.multi.response = if(is.list(all_coefs[[1]])){TRUE}else{FALSE}
#
#     #--------------------------------------------------------------------------------------------#
#     #5) Compute the recruitment term
#     if(is.multi.response){
#
#       #get a list of outcome elements
#       all_coefs = do.call(what = Map, args = c(c, all_coefs))
#
#       #----------------------------------------------------------------------#
#       #get all features
#       allfeats = unique(unlist(lapply(X = all_coefs, FUN = function(x){unique(unlist(sapply(X = x, FUN = row.names)))})))
#
#       #----------------------------------------------------------------------#
#       coefs.matrix = lapply(X = all_coefs, FUN = function(coef, allfeats){
#         #force to matrix
#         coefs.m = lapply(X = coef, FUN = function(icoef, allfeats){
#           #create matrix with all features
#           out = matrix(data = 0, nrow = length(allfeats), ncol = 1, dimnames = list('features' = allfeats, 1))
#           #force to matrix
#           icoef = as.matrix(icoef)
#           #update
#           out[rownames(icoef), 1] = icoef
#
#           return(out)
#         }, allfeats = allfeats)
#
#         #Merge the coefficients from different runs in one matrix
#         coefs.m = do.call(what = cbind, args = coefs.m)
#
#       }, allfeats = allfeats)
#
#       #Compute the recruitment term
#       recruitment_term = lapply(X = coefs.matrix, FUN = get_score_recuitment_term, recruitment.term = recruitment.term)
#     } else {
#
#       #get all features
#       allfeats = unique(unlist(sapply(X = coefs, FUN = row.names)))
#
#       #force to matrix
#       coefs = lapply(X = coefs, FUN = function(icoef, allfeats){
#         #create matrix with all features
#         out = matrix(data = 0, nrow = length(allfeats), ncol = 1, dimnames = list('features' = allfeats, 1))
#         #force to matrix
#         icoef = as.matrix(icoef)
#         #update
#         out[rownames(icoef), 1] = icoef
#
#         return(out)
#       }, allfeats = allfeats)
#
#       #Merge the coefficients from different runs in one matrix
#       coefs.matrix = do.call(what = cbind, args = coefs)
#
#       #Compute the recruitment term
#       recruitment_term = get_score_recuitment_term(coefs.matrix = coefs.matrix, recruitment.term = recruitment.term)
#     }
#
#     #--------------------------------------------------------------------------------------------#
#     #4) Compute the scores, considering each accuracy measure
#     if(is.multi.response){
#       scores = lapply(X = recruitment_term, FUN = function(r, weights){
#         s = get_significance_score(weights = weights, recruitment = r);
#         return(s)
#       }, weights = w)
#     } else {
#       scores = get_significance_score(weights = w, recruitment = recruitment_term);
#     }
#
#     #--------------------------------------------------------------------------------------------#
#     #bind outcomes
#     if(is.multi.response){
#       scores = do.call(what = cbind, args = scores)
#     }
#     #set as dataframe
#     scores = as.data.frame(scores)
#
#     #create Marked object
#     scores = Marked(
#       mark           = scores,
#       type.measure   = type.measure,
#       set            = set,
#       config         = config,
#       marking.system = recruitment.term
#     )
#
#     #--------------------------------------------------------------------------------------------#
#     #return
#     return(scores)
#   })
#
#
#
# methods::setMethod(
#   f = "mark",
#   signature = methods::signature(object = "Marker",
#                                  learned      = "LearnedList",
#                                  set          = "character",
#                                  config       = "character",
#                                  type.measure = "missing"),
#   function(object,
#            learned,
#            set = c('test', 'train', 'full'),
#            config = c("opt", "1se"),
#            type.measure){
#
#     #--------------------------------------------------------------------------------------------#
#     #Set logger
#     logger = get_logger(object)
#     logger = open_con(logger)
#
#     #--------------------------------------------------------------------------------------------#
#     #match
#     set    = match.arg(set)
#     config = match.arg(config)
#
#     #--------------------------------------------------------------------------------------------#
#     #get the RenoirTested objects
#     tested = get_tested(learned)
#
#     #Get the names of the measures
#     name_measures = unique(unlist(lapply(X = tested, FUN = get_test_type_measure)))
#
#     #List to store the result
#     out = list();
#
#     #Loop over the accuracy measures
#     i = 1
#     for(nm in name_measures){
#
#       log_debug(object = logger, message = paste0("Considering '",nm,"' accuracy measure."), sep = "\n", add.level = TRUE, add.time = TRUE)
#
#       #Store
#       out[[nm]] = mark(
#         object = object,
#         learned = learned,
#         config = config,
#         set = set,
#         type.measure = nm);
#     }
#
#     #--------------------------------------------------------------------------------------------#
#     #close
#     close_con(logger)
#
#     #--------------------------------------------------------------------------------------------#
#     #return
#     return(out)
#   }
# )
#
# methods::setMethod(
#   f = "mark",
#   signature = methods::signature(object = "Marker",
#                                  learned      = "LearnedList",
#                                  set          = "missing",
#                                  config       = "character",
#                                  type.measure = "character"),
#   function(object,
#            learned,
#            set,
#            config = c("opt", "1se"),
#            type.measure){
#
#     #match
#     config = match.arg(config)
#
#     #training set
#     m.train = mark(object = object, learned = learned, set = "train", config = config, type.measure = type.measure)
#
#     #testing set
#     m.test  = mark(object = object, learned = learned, set = "test", config = config, type.measure = type.measure)
#
#     #whole set
#     m.full  = mark(object = object, learned = learned, set = "full", config = config, type.measure = type.measure)
#
#     out = list(train = m.train, test = m.test, full = m.full)
#
#   }
# )
#
# methods::setMethod(
#   f = "mark",
#   signature = methods::signature(object = "Marker",
#                                  learned      = "LearnedList",
#                                  set          = "missing",
#                                  config       = "character",
#                                  type.measure = "missing"),
#   function(object,
#            learned,
#            set,
#            config = c("opt", "1se"),
#            type.measure){
#
#     #match
#     config = match.arg(config)
#
#     #--------------------------------------------------------------------------------------------#
#     #Set logger
#     logger = get_logger(object)
#     logger = open_con(logger)
#
#     #--------------------------------------------------------------------------------------------#
#     #training set
#     log_debug(object = logger, message = "Grading features on training set.", sep = "\n", add.level = TRUE, add.time = TRUE)
#     m.train = mark(object = object, learned = learned, set = "train", config = config)
#
#     #testing set
#     log_debug(object = logger, message = "Grading features on testing set.", sep = "\n", add.level = TRUE, add.time = TRUE)
#     m.test  = mark(object = object, learned = learned, set = "test", config = config)
#
#     #whole set
#     log_debug(object = logger, message = "Grading features on full set.", sep = "\n", add.level = TRUE, add.time = TRUE)
#     m.full  = mark(object = object, learned = learned, set = "full", config = config)
#
#     #--------------------------------------------------------------------------------------------#
#     #out obj
#     out = list(train = m.train, test = m.test, full = m.full)
#
#     #--------------------------------------------------------------------------------------------#
#     #close
#     close_con(logger)
#
#     #--------------------------------------------------------------------------------------------#
#     #return
#     return(out)
#   }
# )
#
# methods::setMethod(
#   f = "mark",
#   signature = methods::signature(object = "Marker",
#                                  learned      = "LearnedList",
#                                  set          = "character",
#                                  config       = "missing",
#                                  type.measure = "missing"),
#   function(object,
#            learned,
#            set,
#            config,
#            type.measure){
#
#     #--------------------------------------------------------------------------------------------#
#     #Set logger
#     logger = get_logger(object)
#     logger = open_con(logger)
#
#     #--------------------------------------------------------------------------------------------#
#
#     #opt config
#     log_debug(object = logger, message = "Grading features from best config (opt).", sep = "\n", add.level = TRUE, add.time = TRUE)
#     m.opt = mark(object = object, learned = learned, set = set, config = "opt")
#
#     #1se config
#     log_debug(object = logger, message = "Grading features from best config (1se).", sep = "\n", add.level = TRUE, add.time = TRUE)
#     m.1se = mark(object = object, learned = learned, set = set, config = "1se")
#
#     #out obj
#     out = list('opt' = m.opt, '1se' = m.1se)
#
#     #--------------------------------------------------------------------------------------------#
#     #close
#     close_con(logger)
#
#     #--------------------------------------------------------------------------------------------#
#     #return
#     return(out)
#   }
# )
#
# methods::setMethod(
#   f = "mark",
#   signature = methods::signature(object = "Marker",
#                                  learned      = "LearnedList",
#                                  set          = "missing",
#                                  config       = "missing",
#                                  type.measure = "missing"),
#   function(object,
#            learned,
#            set,
#            config,
#            type.measure){
#
#     #--------------------------------------------------------------------------------------------#
#     #Set logger
#     logger = get_logger(object)
#     logger = open_con(logger)
#
#     #--------------------------------------------------------------------------------------------#
#     #training set
#     log_debug(object = logger, message = "Grading features on training set.", sep = "\n", add.level = TRUE, add.time = TRUE)
#     m.train = mark(object = object, learned = learned, set = "train")
#
#     #testing set
#     log_debug(object = logger, message = "Grading features on testing set.", sep = "\n", add.level = TRUE, add.time = TRUE)
#     m.test  = mark(object = object, learned = learned, set = "test")
#
#     #whole set
#     log_debug(object = logger, message = "Grading features on full set.", sep = "\n", add.level = TRUE, add.time = TRUE)
#     m.full  = mark(object = object, learned = learned, set = "full")
#
#     #--------------------------------------------------------------------------------------------#
#     #out obj
#     out = list(train = m.train, test = m.test, full = m.full)
#
#     #--------------------------------------------------------------------------------------------#
#     #close
#     close_con(logger)
#
#     #--------------------------------------------------------------------------------------------#
#     #return
#     return(out)
#   }
# )
#
# #'@param coefs.matrix coefficients matrix
# get_score_recuitment_term <- function(coefs.matrix, recruitment.term = c("coefficient", "presence")){
#
#   recruitment.term = match.arg(arg = recruitment.term)
#
#   #----------------------------------------------------------------------#
#
#   # #1)Merge the coefficients from different runs in one matrix
#   # coefs = do.call(what = cbind, args = coefs)
#
#   #----------------------------------------------------------------------#
#   #2) Compute presence term or normalised coefficients
#   if(identical(x = recruitment.term, y = "presence")){
#     #Which coefficient is greatere than 0?
#     test = coefs.matrix>0
#     #----------------------------------------------------------------------#
#     #Set recruited feature coefficient to one
#     coefs.matrix[test] = 1;
#     #Which coefficient is greatere than 0?
#     test = coefs.matrix<0
#     #----------------------------------------------------------------------#
#     #Set recruited feature coefficient to one
#     coefs.matrix[test] = -1;
#   } else {
#     #Divide each coefficient for the maximum coefficients absolute value, for each model
#     coefs.matrix = sweep(x = coefs.matrix, MARGIN = 2, STATS = apply(X = abs(coefs.matrix), MARGIN = 2, FUN = max, na.rm = TRUE), FUN = "/")
#   }
#
#   #----------------------------------------------------------------------#
#   #return
#   return(coefs.matrix)
#
# }


#'Compute Significance Score
#'@param weights the weights for the computation
#'@param recruitment the recruitment terms
#'@return the significance score for each feature
#'@keywords internal
get_significance_score <- function(weights, recruitment){

  scaling.factor = sum(weights, na.rm = T)

  # r = recruitment[1,,drop=F]
  score = apply(X = recruitment, MARGIN = 1, FUN = function(r, w, n){

    #multiply the weight by the presence in the model
    test = as.vector(r * w);

    #sum scores
    s = sum(test, na.rm = T);

    #divide by sum of weights to have a score in the range [0,1]
    s = s / n;

    return(s);
  }, w = weights, n = scaling.factor)

  return(score)
}





get_marker_formula_as_text_glmnet <- function(marking.system = c("presence", "coefficient"), add.info = T){
  marking.system = match.arg(marking.system)

  out = switch(
    marking.system,
    presence    = paste0("$$ mark_{ij} = presence_{ij} * sign_{ij}$$"),
    coefficient = paste0("$$ mark_{ij} = coefficient_{ij}$$"),
  )

  if(add.info){
    out = switch(
      marking.system,
      presence    = paste0(out, " where $presence_{ij}$ can have binary value 0/1 depending on the presence of the feature in the $j-th$ model, and $sign_{ij}$ is the sign of the coefficient"),
      coefficient = paste0(out, " where $coefficient_{ij}$ is the $i-th$ feature coefficient in the $j-th$ model"),
    )
  }

  return(out)
}

get_marker_formula_as_text_default <- function(){
  out = NULL
  return(out)
}

get_marker_formula_as_text_randomForest <- function(...){

  out = get_marker_formula_as_text_default(...)

  return(out)
}

get_marker_formula_as_text <- function(id, ...){
  out = switch(
    id,
    lasso              = get_marker_formula_as_text_glmnet(...),
    ridge              = get_marker_formula_as_text_glmnet(...),
    elnet              = get_marker_formula_as_text_glmnet(...),
    elasticnet         = get_marker_formula_as_text_glmnet(...),
    relaxed_lasso      = get_marker_formula_as_text_glmnet(...),
    relaxed_ridge      = get_marker_formula_as_text_glmnet(...),
    relaxed_elnet      = get_marker_formula_as_text_glmnet(...),
    relaxed_elasticnet = get_marker_formula_as_text_glmnet(...),
    randomForest       = get_marker_formula_as_text_randomForest(...),
    get_marker_formula_as_text_default
  )
  return(out)
}


renoir_marker_default_parameters <- function(id){
  out = switch(
    id,
    lasso              = list(marking.system = c("presence")),
    ridge              = list(marking.system = c("presence")),
    elnet              = list(marking.system = c("presence")),
    elasticnet         = list(marking.system = c("presence")),
    relaxed_lasso      = list(marking.system = c("presence")),
    relaxed_ridge      = list(marking.system = c("presence")),
    relaxed_elnet      = list(marking.system = c("presence")),
    relaxed_elasticnet = list(marking.system = c("presence")),
    randomForest       = list(marking.system = c("presence")),
    xgbtree            = list(marking.system = c("presence")),
    xgblinear          = list(marking.system = c("presence")),
    gbm                = list(marking.system = c("presence")),
    linear_SVM         = list(marking.system = c("presence")),
    polynomial_SVM     = list(marking.system = c("presence")),
    radial_SVM         = list(marking.system = c("presence")),
    sigmoid_SVM        = list(marking.system = c("presence")),
    linear_NuSVM       = list(marking.system = c("presence")),
    polynomial_NuSVM   = list(marking.system = c("presence")),
    radial_NuSVM       = list(marking.system = c("presence")),
    sigmoid_NuSVM      = list(marking.system = c("presence")),
    gknn               = list(marking.system = c("presence")),
    nsc                = list(marking.system = c("presence")),
    list()
  )
  return(out)
}
