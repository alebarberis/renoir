#' @include classes_generics.R class_Evaluated.R
NULL

#'EvaluatedList Class
#'
#'An S4 class to represent a list of filters to pre-process the data.
#'
#'This subclass extends the S4  \linkS4class{List} virtual class by using
#'the  \linkS4class{SimpleList} implementation.
#'
#' The object consists of 4 slots
#' @slot listData \code{list} storing the list elements
#' @slot elementType the type of data represented in the sequence
#' @slot elementMetadata  \linkS4class{DataFrame} storing the element-wise metadata,
#' with a row for each element and a column for each metadata available. Default is \code{NULL}
#' @slot metadata \code{list} storing the global metadata annotating the object as a whole
#'
#' @keywords internal
methods::setClass(
  Class = "EvaluatedList",
  contains = "SimpleList"
  ,slots = c(listData = "list")
  # ,prototype = methods::prototype(elementType = "FilterList")
)

methods::setMethod(f = "get_id",           signature = "EvaluatedList", definition = function(object){ sapply(X = object, FUN = get_id        )})
methods::setMethod(f = "get_config",       signature = "EvaluatedList", definition = function(object){ sapply(X = object, FUN = get_config    )})
methods::setMethod(f = "get_response",     signature = "EvaluatedList", definition = function(object){ sapply(X = object, FUN = get_response  )})
methods::setMethod(f = "get_nout",         signature = "EvaluatedList", definition = function(object){ sapply(X = object, FUN = get_nout      )})
methods::setMethod(f = "get_sampling",     signature = "EvaluatedList", definition = function(object){ sapply(X = object, FUN = get_sampling  )})
methods::setMethod(f = "get_size",         signature = "EvaluatedList", definition = function(object){ sapply(X = object, FUN = get_size      )})
methods::setMethod(f = "get_N",            signature = "EvaluatedList", definition = function(object){ sapply(X = object, FUN = get_N         )})
methods::setMethod(f = "get_n",            signature = "EvaluatedList", definition = function(object){ sapply(X = object, FUN = get_n         )})
methods::setMethod(f = "get_k",            signature = "EvaluatedList", definition = function(object){ sapply(X = object, FUN = get_k         )})
methods::setMethod(f = "get_observations", signature = "EvaluatedList", definition = function(object){ lapply(X = object, FUN = get_observations)})
methods::setMethod(f = "get_models",       signature = "EvaluatedList", definition = function(object){ lapply(X = object, FUN = get_models    )})
methods::setMethod(f = "get_train",        signature = "EvaluatedList", definition = function(object){ lapply(X = object, FUN = get_train     )})
methods::setMethod(f = "get_test",         signature = "EvaluatedList", definition = function(object){ lapply(X = object, FUN = get_test      )})
methods::setMethod(f = "get_full",         signature = "EvaluatedList", definition = function(object){ lapply(X = object, FUN = get_full      )})
methods::setMethod(f = "get_stability",    signature = "EvaluatedList", definition = function(object){ lapply(X = object, FUN = get_stability )})
methods::setMethod(f = "get_performance",  signature = "EvaluatedList", definition = function(object){ lapply(X = object, FUN = get_performance )})

methods::setMethod(f = "get_learning",     signature = "EvaluatedList", definition = function(object){ unique(sapply(X = object, FUN = get_learning))})
methods::setMethod(f = "get_screening",    signature = "EvaluatedList", definition = function(object){ unique(sapply(X = object, FUN = get_screening))})

#'EvaluatedList Constructor
#'
#'Constructor for the S4 EvaluatedList object.
#'
#'Constructor for the S4 \linkS4class{EvaluatedList} object.
#' @keywords internal
EvaluatedList <- function(...){

  obj = new("EvaluatedList", S4Vectors::SimpleList(...))

  # S4Vectors::elementType(obj) = "Evaluated"
  obj@elementType = "Evaluated"
  obj

}


methods::setMethod(
  f = "subset_list",
  signature = methods::signature(object = "EvaluatedList"),
  definition = function(object, n){

    if(isFALSE(missing(n))){
      #keep
      keep = get_n(object) %in% n
      #subset
      object = object[keep]
    }

    if(length(object)==1){
      object = object[[1]]
    }

    return(object)
  }
)

#'Create EvaluatedList Object
#'
#'@param ... list of \linkS4class{Evaluated} objects
#'@param grouping logical, whether to group \linkS4class{Evaluated} objects
#'by common id/config/response/sampling
#'@return An object of class \linkS4class{EvaluatedList}
#'
#'@keywords internal
create_EvaluatedList <- function(..., grouping = T){

  out = c(...)

  if(grouping){
    id       = sapply(X = out, FUN = get_id)
    config   = sapply(X = out, FUN = get_config)
    response = sapply(X = out, FUN = get_response)
    sampling = sapply(X = out, FUN = get_sampling)

    #check config
    if(isTRUE(all(S4Vectors::isEmpty(config)))){
      #create keys
      keys = data.frame(id = id, response = response, sampling = sampling, stringsAsFactors = F)

      #get unique
      ukeys = unique(keys)

      #outlist
      outlist = list()

      i = 1
      for(i in seq(nrow(ukeys))){
        #match
        ikey = which(keys$id == ukeys[i,]$id & keys$response == ukeys[i,]$response & keys$sampling == ukeys[i,]$sampling)
        #create
        outlist[[i]] = EvaluatedList(out[ikey])
      }
    } else {
      #create keys
      keys = data.frame(id = id, config = config, response = response, sampling = sampling, stringsAsFactors = F)

      #get unique
      ukeys = unique(keys)

      #outlist
      outlist = list()

      i = 1
      for(i in seq(nrow(ukeys))){
        #match
        ikey = which(keys$id == ukeys[i,]$id & keys$config == ukeys[i,]$config & keys$response == ukeys[i,]$response & keys$sampling == ukeys[i,]$sampling)
        #create
        outlist[[i]] = EvaluatedList(out[ikey])
      }
    }



  } else {
    outlist = EvaluatedList(out)
  }

  return(outlist)
}

#'Which is the best training set size
#'
#'@description Select best \linkS4class{Evaluated} object from a list.
#'The selection is performed by looking at the optimal confidence interval
#'(defined as the confidence interval with minimum upper confidence limit,
#'or maximum lower confidence limit, depending on the considered accuracy measure)
#'@param object \linkS4class{EvaluatedList} object
#'@param scorer a \linkS4class{Scorer} object or a \linkS4class{ScorerList} object
#'containing the \linkS4class{Scorer} object that computed the \code{measure} of interest
#'@return integer indicating the index of the best \linkS4class{Evaluated} object
#'in the input list
#'
#'@keywords internal
which_best_evaluated <- function(
  object,
  set,
  measure,
  evaluator,
  scorer,
  confidence,
  distribution
  # confidence = 0.95,
  # distribution = "normal"
  ){


  #--------------------------------------------------------------------------------------------#
  if(missing(evaluator) && missing(scorer)){stop("Either 'evaluator' or 'scorer' must be provided.\n")}

  #--------------------------------------------------------------------------------------------#
  #evaluator
  if(!missing(evaluator)){
    scorer = get_scorer(evaluator)
  }

  #--------------------------------------------------------------------------------------------#
  #check scorer
  if(is.ScorerList(scorer)){
    scorer = select_by(object = scorer, id = measure)
    if(length(scorer)==0){stop("'measure' not found in 'scorer'. Please, check your data.\n")}
  }

  #--------------------------------------------------------------------------------------------#
  #Get performance
  testedlist = get_performance(object)
  #subset
  testedlist = sapply(X = testedlist, FUN = subset_list, set = set, measure = measure)
  #as TestedList
  testedlist = TestedList(testedlist)

  #--------------------------------------------------------------------------------------------#
  #Get mean error
  merr = get_mscore(testedlist)
  #Get standard error
  se   = get_sem(testedlist)

  #--------------------------------------------------------------------------------------------#
  #Compute confidence
  if(!missing(confidence) && !missing(distribution)){
    #get error
    err  = get_score(testedlist)
    #get repeats
    k = sapply(X = err, FUN = length)

    civ = list()
    for(i in seq(length(err))){
      civ[[i]] = ci(estimate = merr[i], se = se[i], confidence = confidence, distribution = distribution, n = k[i])
    }
  } else {
    civ = get_ci(testedlist)
  }

  #--------------------------------------------------------------------------------------------#
  #which best
  out = which_best_resample(merr = merr, sderr = se, opt = get_optimum(scorer), ci = civ)

  #--------------------------------------------------------------------------------------------#
  return(out)
}

which_best_resample <- function(merr, sderr, opt = c("max", "min"), ci){

  opt = match.arg(opt)

  #
  if(identical(opt, "max")){
    if(!missing(ci)){
      #get ci
      measures = sapply(X = ci, FUN = '[', 'low')
    }else{
      #compute limit
      measures = merr - sderr
    }
    #get optimum
    opt = max(measures, na.rm = T)
  } else if(identical(opt, "min")){
    if(!missing(ci)){
      #get ci
      measures = sapply(X = ci, FUN = '[', 'up')
    }else{
      #compute limit
      measures = merr + sderr
    }
    #get optimum
    opt = min(measures, na.rm = T)
  }

  #get index
  out = which(measures == opt)[1]

  #r
  return(out)

}

methods::setMethod(
  f = "which_best",
  signature = c(object ="EvaluatedList",  set = "character", measure = "character"),
  definition = which_best_evaluated)


methods::setMethod(
  f = "which_best",
  signature = c(object = "EvaluatedList", set = "missing", measure = "missing"),
  definition = function(object, ...){
    #get performance
    performance = get_performance(object)

    #get sets, measures
    sets     = unlist(lapply(X = performance, FUN = get_set))
    measures = unlist(lapply(X = performance, FUN = get_measure))
    n        = unlist(get_n(object))

    keys = data.frame(set = sets, measure = measures, stringsAsFactors = F)
    ukeys = unique(keys)

    out = ukeys
    out$n = NA
    i = 1
    for(i in seq(nrow(ukeys))){
      # tmp = compute_importance(object = object, marker = marker, set = ukeys[i, 'set'], measure = ukeys[i, 'measure'], ...)
      tmp = which_best(object = object, set = ukeys[i, 'set'], measure = ukeys[i, 'measure'], ...)
      #store as Marked
      out[i,'n'] = n[tmp]
    }

    return(out)

  }

)

#'Stability
#'@param object an object of class \linkS4class{EvaluatedList}
#'
#'@rdname stability
methods::setMethod(
  f = "stability",
  signature = methods::signature(object = "EvaluatedList"),
  definition = function(object){
    #get stability from each evaluated object
    stability = get_stability(object)
    #reshape to have one element per response
    stability = do.call(what = Map, args = c(f = cbind, stability))
    #stability
    # log_debug(object = logger, message = "Computing features stability...", sep = "", add.level = TRUE, add.time = TRUE)
    stability = lapply(X = stability, FUN = rowMeans, na.rm = T)
    # log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    return(stability)
  })

#'Compute feature importance
#'
#'@inheritParams compute_importance
#'@param ... further arguments to `compute_importance`
#'
#'@rdname importance
methods::setMethod(
  f = "importance",
  signature = methods::signature(object = "EvaluatedList"),
  definition = function(object, marker, scorer, features, ...){
    #get models
    # models = get_models(object)
    # #mark
    # # marks = mark(object = models[[1]], marker = marker)#DEBUG
    # marks = lapply(X = models, FUN = mark, marker = marker)

    #get performance
    performance = get_performance(object)

    #get sets, measures
    sets     = unlist(lapply(X = performance, FUN = get_set))
    measures = unlist(lapply(X = performance, FUN = get_measure))

    keys = data.frame(set = sets, measure = measures, stringsAsFactors = F)
    ukeys = unique(keys)

    out = list()
    i = 1
    for(i in seq(nrow(ukeys))){
      # tmp = compute_importance(object = object, marker = marker, set = ukeys[i, 'set'], measure = ukeys[i, 'measure'], ...)
      tmp = compute_importance(object = object, marker = marker, scorer = scorer, set = ukeys[i, 'set'], measure = ukeys[i, 'measure'], features = features, ...)
      #set as dataframe
      tmp = as.data.frame(x = do.call(cbind, tmp), stringsAsFactors = F)
      #store as Marked
      out[[i]] = Marked(mark = tmp, measure = ukeys[i, 'measure'], set = ukeys[i, 'set'])
    }

    #--------------------------------------------------------------------------------------------#
    #output obj
    if(length(out)>1){
      out = MarkedList(out)
    } else {
      out = out[[1]]
    }

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  })

#'Compute feature importance
#'
#'@param object an object of class \linkS4class{EvaluatedList}
#'@param marker an object of class \linkS4class{Marker}
#'@param scorer an object of class \linkS4class{Scorer}
#'@param set the data set to consider (\code{train}, \code{test} or \code{full})
#'@param measure the performance metric to consider
#'@param logger an object of class \linkS4class{Logger}
#'@param features (optional) vector containing the predictors to consider
#'@param epsilon small number to avoid division by zero
#'@param ... not currently used
#'
#'@return The computed importance for each feature.
#'
#'@keywords internal
compute_importance <- function(object, marker, scorer, set, measure, logger = Logger(verbose = F), features, epsilon = 1, ...){

  logger = open_con(logger)

  #--------------------------------------------------------------------------------------------#
  #get models
  models = get_models(object)

  #--------------------------------------------------------------------------------------------#
  #mark
  #Remember marks are already weighted by stability for tuned objects as default
  log_trace(object = logger, message = "Marking...", sep = "", add.level = TRUE, add.time = TRUE)
  # marks = mark(object = models[[1]], marker = marker)#DEBUG
  marks = lapply(X = models, FUN = mark, marker = marker, features = features)
  log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------------------------------------#
  #get performance
  log_trace(object = logger, message = "Extract performance...", sep = "", add.level = TRUE, add.time = TRUE)
  performance = get_performance(object)

  #subset
  performance = lapply(X = performance, FUN = subset_list, set = set, measure = measure)
  log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------------------------------------#
  #get problem
  mnames = get_id(scorer)
  optp = get_optimum(scorer)[which(mnames == measure)]

  #--------------------------------------------------------------------------------------------#
  #extract error
  err = lapply(X = performance, FUN = get_score)

  #--------------------------------------------------------------------------------------------#
  #unlist
  marks = unlist(marks, recursive = F)
  err   = unlist(err)

  #--------------------------------------------------------------------------------------------#
  #compute weights
  log_trace(object = logger, message = "Computing weights...", sep = "", add.level = TRUE, add.time = TRUE)
  # w = compute_importance_weights(err = err, measure = measure, epsilon = epsilon)
  w = compute_importance_weights(err = err, optimum = optp, epsilon = epsilon)
  log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------------------------------------#
  #reshape to have one element per response
  log_trace(object = logger, message = "Reshape...", sep = "", add.level = TRUE, add.time = TRUE)
  marks     = do.call(what = Map, args = c(f = cbind, marks))

  #check if multi
  is.multi = length(marks) > 1

  if(is.multi){
    #reshape
    marks = lapply(X = marks, FUN = do.call, what = cbind)
  }
  log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------------------------------------#
  #compute importance
  log_trace(object = logger, message = "Computing importance...", sep = "", add.level = TRUE, add.time = TRUE)
  out = lapply(X = marks, FUN = multiresponse_error, weights = w, multi = "average")
  log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------------------------------------#
  #Check NaN - if the mark of a feature was NA it would result in a NaN. Convert to NA
  out = lapply(X = out, FUN = function(x){x[is.nan(x)] = NA;return(x)})

  #--------------------------------------------------------------------------------------------#
  #close
  close_con(logger)

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}

#@param error accuracy measures
#@param optimum the optimisation problem to solve for selecting the optimal accuracy measure
#@param epsilon small quantity added to err to avoid division by 0
compute_importance_weights <- function(err, optimum, epsilon = 1){

  if(!identical(optimum, "max")){
    #Add small quantity to avoid 1/0 in next computation
    err = err + epsilon

    #get squared error
    err = err^2;

    #reverse
    err = 1 / err;
  } else {
    #get squared error
    err = err^2;
  }

  #return
  return(err)
}


#'Get EvaluatedList Object Summary
#'
#'@description
#'This function produces a summary of an object of class \linkS4class{EvaluatedList}.
#'
#'@param object object of class \linkS4class{EvaluatedList}, a list of
#'\linkS4class{Evaluated} elements
#'@param key (optional) character vector indicating the data to use for uniquely
#'identifying the evaluated elements from the list.
#'It can be a combination of \code{"id"}, \code{"config"},
#'\code{"response"}, and \code{"sampling"}. If missing, it is internally
#'identified with \code{\link{identify_primary_key_from_EvaluatedList}}
#'
#'@inherit summary_table.Evaluated return
#'
#'@seealso
#'\code{\link{summary_table.Renoir}},
#'\code{\link{summary_table.Evaluated}},
#'\code{\link{summary_table.TestedList}},
#'\code{\link{summary_table.Tested}},
#'\code{\link{summary_table.Tuned}},
#'\code{\link{summary_table.Trained}}
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
summary_table.EvaluatedList <- function(object, key, best = c("opt", "1se"), ...){

  #--------------------------------------------------------------------------------------------#
  #match
  best = match.arg(best)

  #--------------------------------------------------------------------------------------------#
  if(missing(key)){key = identify_primary_key_from_EvaluatedList(object)}

  #--------------------------------------------------------------------------------------------#
  #summary
  out = lapply(X = object, FUN = summary_table, key = key, best = best)

  #bind
  out = do.call(what = rbind, args = c(out, make.row.names = F, stringsAsFactors = F))
  #r
  return(out)
}


#@describeIn summary_table Summary of an object of class \linkS4class{EvaluatedList}
#
#@inheritParams summary_table.EvaluatedList
#
#@inherit summary_table.EvaluatedList return
#
#'Get the Summary of an EvaluatedList Object
#'
#'@author Alessandro Barberis
#'
#'@rdname summary_table
methods::setMethod(
  f = "summary_table",
  signature = methods::signature(object = "EvaluatedList"),
  definition = summary_table.EvaluatedList
)

#'Identify Primary Key
#'
#'@description This function identifies a primary key from a list
#'of \linkS4class{Evaluated} objects.
#'
#'@details Currently, elements can be uniquely grouped by four identifiers:
#'1) the learning method, 2) the hyperparameter configuration,
#'3) the response type, 4) the sampling strategy.
#'This function extracts the related values from the \linkS4class{EvaluatedList}
#'object and check if multiple values are present in order to
#'identify a unique key.
#'
#'@param object object of class \linkS4class{EvaluatedList}
#'
#'@return A character vector.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
identify_primary_key_from_EvaluatedList <- function(object){

  #--------------------------------------------------------------------------------------------#
  #Default
  out = "id"

  #--------------------------------------------------------------------------------------------#
  #Extract data
  # id       = get_id(object)
  config   = get_config(object)
  response = get_response(object)
  sampling = get_sampling(object)

  #--------------------------------------------------------------------------------------------#
  #Check config
  if(!isTRUE(all(S4Vectors::isEmpty(config)))){
    config = unique(unlist(config))
    #rm empty
    config = config[!(S4Vectors::isEmpty(config))]
    #check
    if(length(config)>1){out = c(out, "config")}
  }

  #--------------------------------------------------------------------------------------------#
  #Check response
  response = unique(unlist(response))
  #check
  if(length(response)>1){out = c(out, "response")}

  #--------------------------------------------------------------------------------------------#
  #Check sampling
  sampling = unique(unlist(sampling))
  #check
  if(length(sampling)>1){out = c(out, "sampling")}

  #--------------------------------------------------------------------------------------------#
  return(out)
}


# compute_importance_weights <- function(err, measure){
#
#   #check measure
#   #If AUC measure, compute 1 - auc so to have a metric where 0
#   #corresponds to best performance
#   err = get_min_as_opt(err, measure)
#
#   #Add 1 to avoid 1/0 in next computation
#   err = err + 1
#
#   #get squared error
#   err = err^2;
#
#   #reverse
#   err = 1 / err;
#
#   #return
#   return(err)
# }

#' Get Trained/Tuned Model
#'
#' @description This function extracts a model from a
#' \linkS4class{EvaluatedList} object.
#'
#' @param object \linkS4class{EvaluatedList} object
#@param train training set size
#' @param n sample size (i.e. training set size) to select
#' @param index index of model in the list
# @param isize training set size
# @param imodel index of model in the list
#'
#' @return a \linkS4class{Trained} object
#' @author Alessandro Barberis
#' @rdname get_model
methods::setMethod(
  f = "get_model",
  signature = "EvaluatedList",
  definition = function(object, n, index){
    #get evaluated object
    object = subset_list(object = object, n = n)
    #get model
    object = get_model(object = object, index = index)
    #return
    return(object)
  }
)


#'@param object \linkS4class{EvaluatedList} object
#'@param index index of sample in the \linkS4class{Evaluated} object
#'@param n integer, the sample size. Used to select the \linkS4class{Evaluated} object
#'from the \linkS4class{EvaluatedList}
#'
#' @rdname get_sample
methods::setMethod(
  f = "get_sample",
  signature = c("EvaluatedList", "numeric"),
  definition = function(object, index, n){
    #get evaluated
    object = subset_list(object = object, n = n)
    #Subset
    object = get_sample(object = object, index = index)
    #return
    return(object)
  }
)
