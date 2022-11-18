#' @include classes_generics.R class_TestedList.R
NULL

#' Evaluator Class
#'
#' @description
#' An S4 class representing an evaluation of a learning method.
#'
#'@slot id evaluated learning method
#'@slot config (optional) name of the configuration
#'@slot response response type
#'@slot nout a length-one integer vector
#'@slot sampling sampling method used during evaluation
#'@slot N population size
#'@slot n sample size (i.e. training set size)
#'@slot k the number of folds if \code{sampling = "cv"},
#' or the number of repeats if \code{sampling = "random"} and \code{sampling = "bootstrap"}
#'@slot observations indices of sampled observations used for training
#'@slot models list of Tuned or Trained elements
#'@slot stability list containing the features stability for each response across the models
#@slot evaluation errors, mean estimate and standard error
#@slot selection index of best model
#'@slot performance a \linkS4class{TestedList} object
#'
#'@author Alessandro Barberis
methods::setClass(
  Class = "Evaluated",
  slots = c(
    id           = "character",
    config       = "character",
    response     = "character",
    nout         = "integer",
    sampling     = "character",
    N            = "integer",
    # size         = "integer",
    n            = "integer",
    k            = "integer",
    observations = "list",
    models       = "list",
    # evaluation = "list",
    # selection  = "list",
    stability    = "list",
    performance  = "TestedList"
  )
)

### -------------------------------------------------------------------------
#' Constructor for the S4 Evaluated object.
#'
#'@description
#' Constructor for the S4 \linkS4class{Evaluated} object.
#'
#'@param id evaluated learning method
#'@param config (optional) name of the configuration
#'@param nout number of outcomes
#'@param sampling sampling method used during evaluation
#'@param N population size
#'@param n sample size (i.e. training set size)
#'@param k the number of folds if \code{sampling = "cv"},
#' or the number of repeats if \code{sampling = "random"} and \code{sampling = "bootstrap"}
#'@param observations indices of sampled observations used for training
#'@param models list of Tuned or Trained elements
#'@param stability list containing the features stability for each response across the models
#'@param performance a \linkS4class{TestedList} object
#'
#' @return An object of class \linkS4class{Evaluated}.
#'
#' @author Alessandro Barberis
Evaluated <- function(
  id           = character(),
  config       = character(),
  response     = character(),
  nout         = integer(),
  sampling     = character(),
  N            = integer(),
  n            = integer(),
  k            = integer(),
  observations = list(),
  models       = list(),
  stability    = list(),
  performance  = TestedList()
  ){

  methods::new(
    Class = "Evaluated",
    id           = id      ,
    config       = config  ,
    response     = response,
    nout         = nout,
    sampling     = sampling,
    N            = N       ,
    n            = n       ,
    k            = k       ,
    observations = observations,
    models       = models  ,
    stability    = stability,
    performance  = performance
  )
}


methods::setMethod(f = "get_id",           signature = "Evaluated", definition = function(object){ methods::slot(object = object, name = 'id')})
methods::setMethod(f = "get_config",       signature = "Evaluated", definition = function(object){ methods::slot(object = object, name = 'config')})
methods::setMethod(f = "get_response",     signature = "Evaluated", definition = function(object){ methods::slot(object = object, name = 'response')})
methods::setMethod(f = "get_nout",         signature = "Evaluated", definition = function(object){ methods::slot(object = object, name = 'nout')})
methods::setMethod(f = "get_sampling",     signature = "Evaluated", definition = function(object){ methods::slot(object = object, name = 'sampling') })
methods::setMethod(f = "get_size",         signature = "Evaluated", definition = function(object){ methods::slot(object = object, name = 'size') })
methods::setMethod(f = "get_N",            signature = "Evaluated", definition = function(object){ methods::slot(object = object, name = 'N') })
methods::setMethod(f = "get_n",            signature = "Evaluated", definition = function(object){ methods::slot(object = object, name = 'n') })
methods::setMethod(f = "get_k",            signature = "Evaluated", definition = function(object){ methods::slot(object = object, name = 'k') })
methods::setMethod(f = "get_observations", signature = "Evaluated", definition = function(object){ methods::slot(object = object, name = 'observations') })
methods::setMethod(f = "get_models",       signature = "Evaluated", definition = function(object){ methods::slot(object = object, name = 'models') })
methods::setMethod(f = "get_stability",    signature = "Evaluated", definition = function(object){ methods::slot(object = object, name = 'stability') })
methods::setMethod(f = "get_performance",  signature = "Evaluated", definition = function(object){ methods::slot(object = object, name = 'performance') })

methods::setMethod(f = "set_models",       signature = "Evaluated", definition = function(object, value){ methods::slot(object = object, name = 'models') = value; return(object)})

methods::setMethod(
  f = "get_learning",
  signature = "Evaluated",
  definition = function(object){
    #get models
    models = get_models(object)
    # #check if tuned
    # isT = is.Tuned(object = models[[1]])
    # #out
    # out = if(isTRUE(isT)){"tuning"}else{"training"}
    out = get_learning(models[[1]])
    #return
    return(out)
  }
)

methods::setMethod(
  f = "get_screening",
  signature = "Evaluated",
  definition = function(object){
    #get models
    models = get_models(object)
    #
    out = get_screening(models[[1]])
    #return
    return(out)
  }
)


methods::setMethod(
  f = "clean",
  signature = "Evaluated",
  definition = function(object, rm.fit = F, rm.call = F){
    #get models
    models = get_models(object)
    #clean
    models = lapply(X = models, FUN = clean, rm.fit = rm.fit, rm.call = rm.call)
    #set models
    object = set_models(object = object, value = models)
    #return
    return(object)
})



#'Summary Table of an Evaluated Object
#'
#'@description
#'This function produces a summary table of an object of class \linkS4class{Evaluated}.
#'
#'@param object object of class \linkS4class{Evaluated}
#'
#'@inheritParams summary_table.TestedList
#'@inheritParams summary_table.Trained
#'@inheritParams generate_name
#'
#'@return A \code{data.frame} with the following elements
#'\describe{
#'\item{\code{name}}{character string indicating a name for the considered settings. It is generated by \code{\link{generate_name}}}
#'\item{\code{config}}{name of the selected tuned configuration}
#'\item{\code{response}}{response type}
#'\item{\code{sampling}}{sampling strategy}
#'\item{\code{training_set_size}}{training-set size}
#'\item{\code{k}}{number of iteration of the sampling procedure}
#'\item{\code{tuning}}{character string, the tuning strategy}
#'\item{\code{screening}}{character string, the screening strategy}
#'\item{\code{screened}}{integer, the number of screened features}
#'\item{\code{learning}}{character string, the learning method}
#'\item{\code{nfeatures}}{integer, the number of features in the model}
#'\item{\code{configuration}}{string, the configuration, i.e. the fixed hyperparameters of the model}
#'\item{\code{imodel}}{index of the model}
#'\item{\code{set}}{name of the set}
#'\item{\code{measure}}{name of the considered performance metric}
#'\item{\code{score}}{performance metrics}
#'\item{\code{mean_score}}{mean performance metric}
#'\item{\code{standard_error}}{standard error of the mean performance metric}
#'\item{\code{upper_ci}}{upper bound of confidence interval}
#'\item{\code{lower_ci}}{lower bound of confidence interval}
#'\item{\code{best_model}}{logical indicating which is the best model}
#'\item{\code{selection_rule}}{character string indicating the rule adopted for selection of tuned/trained model across k runs}
#'}
#'
#'@seealso
#'\code{\link{summary_table.Renoir}},
#'\code{\link{summary_table.EvaluatedList}},
#'\code{\link{summary_table.TestedList}},
#'\code{\link{summary_table.Tested}},
#'\code{\link{summary_table.Tuned}},
#'\code{\link{summary_table.Trained}},
#'\code{\link{generate_name}}
#'
#'@author Alessandro Barberis
summary_table.Evaluated <- function(object, best = c("opt", "1se"), key){

  #--------------------------------------------------------------------------------------------#
  #match
  best = match.arg(best)

  #--------------------------------------------------------------------------------------------#
  #get training set size
  n = get_n(object)

  #get repeats
  k = get_k(object)

  #--------------------------------------------------------------------------------------------#
  # MODELS
  #--------------------------------------------------------------------------------------------#
  models  = get_models(object)
  imodels = seq(length(models))

  #get summary
  mst = lapply(X = models, FUN = summary_table)

  #bind
  mst = do.call(what = rbind, args = mst)

  #add index
  # mst$irepeat = imodels
  mst$imodel = imodels


  #get number of features
  # nfeatures = sapply(X = models, FUN = get_nfeatures)

  #--------------------------------------------------------------------------------------------#
  #Check if tuning/screening
  ##tuning
  if(isTRUE(is.na(pmatch(x = "tuning", table = colnames(mst))))){mst$tuning = NA}
  ##screening
  if(isTRUE(is.na(pmatch(x = "screening", table = colnames(mst))))){mst$screening = NA;mst$screened = NA;}

  #col order
  column.order = unique(c(c("tuning", "screening", "screened"), colnames(mst)))
  mst = mst[,column.order,drop=F]

  #--------------------------------------------------------------------------------------------#
  # PERFORMANCE
  #--------------------------------------------------------------------------------------------#
  performance = get_performance(object)

  #get summary
  performance = summary_table(object = performance, best = best)


  #--------------------------------------------------------------------------------------------#
  # DATAFRAME
  #--------------------------------------------------------------------------------------------#
  #out
  # out = data.frame(
  #   # key               = key,
  #   name              = generate_name(object, key = key),
  #   id                = get_id(object),
  #   config            = get_config(object),
  #   response          = get_response(object),
  #   sampling          = get_sampling(object),
  #   training_set_size = n,
  #   k                 = k,
  #   text              = txt,
  #   imodel            = imodels,
  #   nfeatures          = nfeatures,
  #   stringsAsFactors  = F,
  #   row.names         = NULL
  # )

  out = data.frame(
    # key               = key,
    name              = generate_name(object, key = key),
    # id                = get_id(object),
    config            = get_config(object),
    response          = get_response(object),
    sampling          = get_sampling(object),
    training_set_size = n,
    k                 = k,
    mst,
    # text              = txt,
    # imodel            = imodels,
    # nfeatures          = nfeatures,
    stringsAsFactors  = F,
    row.names         = NULL
  )


  #corder
  column.order = unique(c(colnames(out), colnames(performance)))
  # out = cbind(out, performance)
  #merge
  out = merge(x = out, y = performance, by = "imodel", sort = F)

  #order
  out = out[,column.order,drop=F]

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}


#'Get the Summary of an Evaluated Object
#'
#@describeIn summary_table Summary of an object of class \linkS4class{Evaluated}
#'
#@inheritParams summary_table.Evaluated
#
#@inherit summary_table.Evaluated return
#
#'@author Alessandro Barberis
methods::setMethod(
  f = "summary_table",
  signature = methods::signature(object = "Evaluated"),
  definition = summary_table.Evaluated)


#'Generate name
#'
#'@description This function generates a name from a key selection.
#'
#'@param object object of class \linkS4class{Evaluated}
#'@param key character vector indicating the elements to use for the name generation.
#'Allowed strings are \code{"id"}, \code{"config"}, \code{"response"}, and \code{"sampling"}
#'
#'@return A character string.
#'
#'@author Alessandro Barberis
generate_name <- function(
  object,
  key = c("id", "config", "response", "sampling")
){

  #--------------------------------------------------------------------------------------------#
  #Allowed
  key.supported = c("id", "config", "response", "sampling")

  #--------------------------------------------------------------------------------------------#
  #Check input

  #key
  if(missing(key)){key = "id"}

  if(isTRUE(anyNA(pmatch(x = key, table = key.supported)))){
    stop(paste("Provided 'key' not supported. Please, check your input. Allowed values are:", key.supported))
  }

  #--------------------------------------------------------------------------------------------#
  #Get elements
  id       = get_id(object)
  config   = get_config(object)
  response = get_response(object)
  sampling = get_sampling(object)

  #--------------------------------------------------------------------------------------------#
  #build key
  out = id

  if(("config" %in% key) && !S4Vectors::isEmpty(config)){
    out = paste0(out, " (",config, ")")
  }

  if("response" %in% key){
    out = paste0(out, " - ",response)
  }

  if("sampling" %in% key){
    out = paste0(out, " - ",sampling)
  }
  #--------------------------------------------------------------------------------------------#
  return(out)
}


#' Get Trained/Tuned Model
#'
#' @description This function extracts a model from a
#' \linkS4class{Evaluated} object.
#'
#' @param object \linkS4class{Evaluated} object
#' @param n sample size (i.e. training set size) to select
#' @param index index of model in the list
#'
#' @return a \linkS4class{Trained} or \linkS4class{Tuned} object
#'
#' @author Alessandro Barberis
methods::setMethod(
  f = "get_model",
  signature = "Evaluated",
  definition = function(object, index){
    #get trained/tuned models
    object = get_models(object)
    #Subset
    object = object[[index]]
    #return
    return(object)
  }
)

methods::setMethod(
  f = "get_sample",
  signature = c("Evaluated", "numeric"),
  definition = function(object, index){
    #get models
    object = get_observations(object)
    #Subset
    object = object[[index]]
    #return
    return(object)
  }
)

