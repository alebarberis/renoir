#' @include classes_generics.R class_Logger.R class_TunedList.R class_TrainedList.R class_Scorer.R
NULL

#' Selector Class
#' An S4 class providing the methods to select a model from a list.
#'
#' The object consists of 3 slots
#' @slot id the object identifier, it must be equivalent to the learning method name (i.e. the trainer id)
#' @slot selector function to use for selecting the model
#' @slot logger an object of class \linkS4class{Logger}
#' @author Alessandro Barberis
methods::setClass(
  Class = "Selector",
  slots = c(
    id       = "character",
    selector = "function",
    logger   = "Logger"
  )
)

#' Constructor for the S4 Selector object.
#'
#' Constructor for the S4 \linkS4class{Selector} object.
#'
# @param learning.method the learning method associated to this \linkS4class{Forecaster}.
# If learning method is one of the supported by renoir, the constructor will
# automatically select \code{prediction} and \code{selector} if they are missing.
#' @param id object identifier, must be equivalent to the id of the associated \linkS4class{Trainer}.
#' If learning method is one of the supported by renoir, the constructor will
#' automatically select a built-in \code{selector} function.
#' @param selector (optional) function used to select a model from a list.
#' Used if \code{id} is not one of the supported by renoir.
#' If \code{selector} is provided it must conform to the renoir common interface,
#' and must have the following formal arguments:
#' \describe{
#'    \item{models}{list of models}
#'    \item{...}{additional arguments}
#' }
#' It should return the index of the selected model
#' @param logger a \linkS4class{Logger}
#'
#' @return An object of class \linkS4class{Selector}.
#'
#' @export
#'
#' @author Alessandro Barberis
Selector <- function(
  id,
  selector,
  logger   = Logger(verbose = FALSE)
){

  if(id %in% supported_learning_methods()){
    if(missing(selector)){
      selector = get_model_selector_function(id)
    }
  }

  #Check provided prediction function
  check_provided_selector_function(selector)

  methods::new(
    Class = "Selector",
    id       = id,
    selector = selector,
    logger   = logger
  )
}


methods::setMethod(f = "get_id",       signature = "Selector", definition = function(object){methods::slot(object = object, name = 'id')})
methods::setMethod(f = "get_selector", signature = "Selector", definition = function(object){methods::slot(object = object, name = 'selector')})
methods::setMethod(f = "get_logger",   signature = "Selector", definition = function(object){methods::slot(object = object, name = 'logger')})


methods::setMethod(
  f = "select",
  signature = methods::signature(selector = "Selector", models = "TrainedList"),
  definition = function(selector, models, ...){

    #--------------------------------------------------------------------------------------------#
    #Check trained models are from the same trainer
    isok = all( unique(unlist(get_learning_method(models))) %in% get_id(selector) )

    if(!isok){
      missingid = setdiff(x = unique(unlist(get_learning_method(models))), y = get_id(selector))

      stop(paste("Learning method mismatch. Different learning methods found in the 'models' list:", paste0(missingid, collapse = ", "), "\n"))
    } else {

      #extract selector function
      selector_fun = get_selector(selector)

      #Select one model
      out = selector_fun(models = models, ...)
    }

    #--------------------------------------------------------------------------------------------#
    #Return
    return(out)
  }
)

#'Select a model
#'@description Selects a model from a list
#'@param selector a \linkS4class{Selector} object
#'@param models a \linkS4class{TrainedList} object containing trained models.
#'Each element corresponds to an element in \code{merr} and \code{sderr}.
#'@param merr vector of error estimates
#'@param sderr vector of standard errors of the estimates
#'@param scorer the \linkS4class{Scorer} object used to generate \code{merr} and \code{sderr}
#'@return list of 2 elements, containing the index of the model having optimal accuracy
#'and the index of the model having accuracy within 1 standard error from the optimal accuracy
methods::setMethod(
  f = "select",
  signature = methods::signature(selector = "Selector", models = "TrainedList", merr = "numeric", sderr = "numeric", scorer = "Scorer"),
  definition = function(selector, models, merr, sderr, scorer,  ...){

    #--------------------------------------------------------------------------------------------#
    #Check trained models are from the same trainer
    isok = all( unique(unlist(get_learning_method(models))) %in% get_id(selector) )

    if(!isok){
      missingid = setdiff(x = unique(unlist(get_learning_method(models))), y = get_id(selector))

      stop(paste("Learning method mismatch. Different learning methods found in the 'models' list:", paste0(missingid, collapse = ", "), "\n"))
    } else {
      #select optimal accuracy
      # opt = get_best_measure(measures = merr, type.measure = type.measure)
      opt = select_optimal_score(scorer = scorer, measures = merr)

      #keep
      keep = merr == opt

      #index
      idopt = which(keep)

      #If more than one model has similar accuracy, select the least complex
      if(length(idopt)>1){
        #index
        id = select(selector = selector, models = models[keep])
        #select
        idopt = idopt[id]
      }

      #range
      seopt = c((merr[idopt] - sderr[idopt]), (merr[idopt] + sderr[idopt]))

      #1SE rule
      keep = (merr >= min(seopt, na.rm=T)) & (merr <= max(seopt, na.rm=T))

      #index
      id1se = which(keep)

      #If more than one model has similar accuracy, select the least complex
      if(length(id1se)>1){
        #index
        id = select(selector = selector, models = models[keep])
        #select
        id1se = id1se[id]
      }


      out = list('opt' = idopt, '1se' = id1se)

    }

    #--------------------------------------------------------------------------------------------#
    #Return
    return(out)
  }
)

methods::setMethod(
  f = "select",
  signature = methods::signature(selector = "Selector", models = "TrainedList", merr = "numeric", sderr = "missing", scorer = "Scorer"),
  definition = function(selector, models, merr, sderr, scorer,  ...){

    #--------------------------------------------------------------------------------------------#
    #Check trained models are from the same trainer
    isok = all( unique(unlist(get_learning_method(models))) %in% get_id(selector) )

    if(!isok){
      missingid = setdiff(x = unique(unlist(get_learning_method(models))), y = get_id(selector))

      stop(paste("Learning method mismatch. Different learning methods found in the 'models' list:", paste0(missingid, collapse = ", "), "\n"))
    } else {
      #select optimal accuracy
      # opt = get_best_measure(measures = merr, type.measure = type.measure)
      opt = select_optimal_score(scorer = scorer, measures = merr)

      #keep
      keep = merr == opt

      #index
      idopt = which(keep)

      #If more than one model has similar accuracy, select the least complex
      if(length(idopt)>1){
        #index
        id = select(selector = selector, models = models[keep])
        #select
        idopt = idopt[id]
      }

      out = idopt

    }

    #--------------------------------------------------------------------------------------------#
    #Return
    return(out)
  }
)


#complexity = lambda
# fun = max

#if complexity is trained object, select best config

#Return a number indicating the complexity
get_complexity_glmnet <- function(object, keep){

  #get fit
  fit = get_fit(object)

  #get lambda
  out = fit$lambda[keep]

  #get

}

which_least_complex_model_glmnet <- function(models){

  #extract config from trained list
  # trainedlist = lapply(X = trainedlist, FUN = "[[", 1)
  config      = lapply(X = models, FUN = get_config)


  #extract data
  lambdas = sapply(X = config, FUN = "[[", "lambda")
  alphas  = sapply(X = config, FUN = "[[", "alpha")
  gammas  = unlist(sapply(X = config, FUN = "[[", "gamma"))

  # #subset
  # lambdas = lambdas[keep]
  # alphas  = alphas[keep]
  # gammas  = gammas[keep]

  #check
  if(is.null(gammas)){
    #select model with largest lambda as least complex
    lambda = max(lambdas, na.rm = TRUE)
    #match
    id = match(lambda, lambdas)
  } else {
    #order by lambda and gamma (1 = unrelaxed, 0 = unpenalised fit)
    id = order(lambdas, gammas, decreasing = TRUE)[1]
  }

  return(id)
}

which_least_complex_model_randomForest <- function(models){

  #extract config from trained list
  config      = lapply(X = models, FUN = get_config)

  #extract data
  ntrees = sapply(X = config, FUN = "[[", "ntree")

  #select model with minor number of trees
  ntree = min(ntrees, na.rm = TRUE)

  #match
  id = match(ntree, ntrees)

  #return
  return(id)
}

which_least_complex_model_xgboost <- function(models){

  #extract config from trained list
  config      = lapply(X = models, FUN = get_config)

  #extract data
  nrounds = sapply(X = config, FUN = "[[", "nrounds")

  #select model with minor number of trees
  nround = min(nrounds, na.rm = TRUE)

  #match
  id = match(nround, nrounds)

  #return
  return(id)
}

which_least_complex_model_gbm <- function(models){

  #extract config from trained list
  config      = lapply(X = models, FUN = get_config)

  #extract data
  ntrees = sapply(X = config, FUN = "[[", "ntree")

  #select model with minor number of trees
  ntree = min(ntrees, na.rm = TRUE)

  #match
  id = match(ntree, ntrees)

  #return
  return(id)
}

which_least_complex_model_svm <- function(models){

  #extract from trained list
  models = lapply(X = models, FUN = get_fit)

  #compute n of used features
  nf = lapply(X = models, FUN = function(object){
    rf.imp = t(object$coefs) %*% object$SV # weight vectors
    rf.imp <- apply(rf.imp, 2, function(v){sqrt(sum(v^2))})  # weight
    #get used variables
    used = !is.na(rf.imp) & rf.imp!=0
    #out
    out = sum(used)
    #r
    return(out)
  })

  #unlist
  nf = unlist(nf)

  #select model with minor number of features
  minf = min(nf, na.rm = TRUE)

  #match
  id = match(minf, nf)

  #return
  return(id)
}

#Which Least Complex
#@details It selects the model with the minimum number of considered
#neighbours.
which_least_complex_model_gknn <- function(models){

  #extract from trained list
  models = lapply(X = models, FUN = get_fit)

  #get number of neighbours
  nf = sapply(X = models, FUN = '[[', 'k')

  #unlist
  nf = unlist(nf)

  #select model with minor number of features
  minf = min(nf, na.rm = TRUE)

  #match
  id = match(minf, nf)

  #return
  return(id)
}

#'Model Selector
#'@param merr vector of error estimates
#'@param sderr vector of standard errors
#'@param trainedlist list of trained models. Each element corresponds to an element in \code{merr} and \code{sderr},
#'and it is a list of trained models with the same configuration over different sampled data
#'@param type.measure accuracy measure used in the computation of \code{merr} and \code{sderr}
select_model <- function(merr, sderr, type.measure, trainedlist, rule = c("opt", "1se"), FUN){

  #select optimal accuracy
  # opt = get_best_measure(measures = merr, type.measure = type.measure)
  opt = get_best_measure(measures = merr, type.measure = type.measure)

  #keep
  keep = merr == opt

  #index
  idopt = which(keep)

  #If more than one model has similar accuracy, select the least complex
  if(length(idopt)>1){
    #index
    id = do.call(what = FUN, args = list(trainedlist = trainedlist[keep]))
    #select
    idopt = idopt[id]
  }

  #range
  seopt = c((merr[idopt] - sderr[idopt]), (merr[idopt] + sderr[idopt]))

  #1SE rule
  keep = (merr >= min(seopt, na.rm=T)) & (merr <= max(seopt, na.rm=T))

  #index
  id1se = which(keep)

  #If more than one model has similar accuracy, select the least complex
  if(length(id1se)>1){
    #index
    id = do.call(what = FUN, args = list(trainedlist = trainedlist[keep]))
    #select
    id1se = id1se[id]
  }


  out = list('opt' = idopt, '1se' = id1se)

  return(out)
}

select_model_by_opt <- function(merr, sderr, type.measure, trainedlist, FUN){

  #select optimal accuracy
  # opt = get_best_measure(measures = merr, type.measure = type.measure)
  # opt = get_best_measure(measures = merr, type.measure = type.measure)
  opt = select_optimal_score(scorer = scorer, measures = merr)

  #keep
  keep = merr == opt

  #index
  idopt = which(keep)

  #If more than one model has similar accuracy, select the least complex
  if(length(idopt)>1){
    #index
    id = do.call(what = FUN, args = list(trainedlist = trainedlist[keep]))
    #select
    idopt = idopt[id]
  }

  return(idopt)
}

select_model_by_1se <- function(merr, sderr, type.measure, trainedlist, FUN){

  #select optimal accuracy
  idopt = select_model_by_opt(merr = merr, sderr = sderr, type.measure = type.measure, trainedlist = trainedlist, FUN = FUN)

  #range
  seopt = c((merr[idopt] - sderr[idopt]), (merr[idopt] + sderr[idopt]))

  #1SE rule
  keep = (merr >= min(seopt, na.rm=T)) & (merr <= max(seopt, na.rm=T))

  #index
  id1se = which(keep)

  #If more than one model has similar accuracy, select the least complex
  if(length(id1se)>1){
    #index
    id = do.call(what = FUN, args = list(trainedlist = trainedlist[keep]))
    #select
    id1se = id1se[id]
  }

  return(id1se)
}

get_model_selector_function <- function(learning.method){
  if(learning.method %in% glmnet_learning_method_names(all = T)){
    FUN = which_least_complex_model_glmnet
  } else {
    FUN = switch(
      learning.method,
      "randomForest"     = which_least_complex_model_randomForest,
      "xgbtree"          = which_least_complex_model_xgboost,
      "xgblinear"        = which_least_complex_model_xgboost,
      "gbm"              = which_least_complex_model_gbm,
      "linear_SVM"       = which_least_complex_model_svm,
      "polynomial_SVM"   = which_least_complex_model_svm,
      "radial_SVM"       = which_least_complex_model_svm,
      "sigmoid_SVM"      = which_least_complex_model_svm,
      "linear_NuSVM"     = which_least_complex_model_svm,
      "polynomial_NuSVM" = which_least_complex_model_svm,
      "radial_NuSVM"     = which_least_complex_model_svm,
      "sigmoid_NuSVM"    = which_least_complex_model_svm,
      "gknn"             = which_least_complex_model_gknn,
      "nsc"              = which_least_complex_model_nsc,
      function(...){1}
    )
  }

  return(FUN)
}


check_provided_selector_function <- function(f){

  if(missing(f)){
    stop("'selector' is missing with no default.\n")
  } else {
    #needed formals
    # formals.def = c("x", "y", "weights", "offset", "resp.type", "observations", "features")
    formals.def = c("models")

    #get formals
    formals.fun = names(formals(fun = f))

    #check
    # if(any(!(formals.fun %in% formals.def))){
    if(any(!(formals.def %in% formals.fun))){

      stop(paste0("Provided selector function without required formal arguments. Function interface must match renoir requirements. Try with:\n
      function(", paste0(formals.def, collapse = ", "),", ...){
        #Select optimal accuracy measure
        # out = YOUR CODE

        #Return selected accuracy
        return(out)
      }
      \n"))
    }
  }
}

