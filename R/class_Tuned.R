#' @include classes_generics.R class_Logger.R class_Trainer.R class_Screened.R class_Trained.R class_TrainedList.R
NULL

setClassUnion(name = "TrainedOrTrainedList", members = c("Trained", "TrainedList"))

#' Tuned Class
#' @description An S4 class providing a container for the results of a tuning procedure.
#' @slot model a \linkS4class{Trained} object
#' @slot config a character string
#' @slot mscore a length-one numeric vector
#' @slot sem a length-one numeric vector
#' @slot tuning a character string
#' @slot sampling a character string
#' @slot measure a character string
#' @slot screened a \linkS4class{Screened} object
#' @slot xvars a length-one integer vector
#' @slot xobs a length-one integer vector
#' @slot index a length-one integer vector
#' @slot trained a list
#' @slot validated a list
#' @slot stability a list
#' @author Alessandro Barberis
methods::setClass(
  Class = "Tuned",
  slots = c(
    model     = "Trained",
    config    = "character",
    mscore    = "numeric",
    sem       = "numeric",
    tuning    = "character",
    sampling  = "character",
    measure   = "character",
    screened  = "Screened",
    xvars     = "integer",
    xobs      = "integer",
    index     = "integer",
    trained   = "list",
    validated = "list",
    # importance= "list"
    stability = "list"
  )
)

#' Constructor for the S4 Tuned object.
#'
#' Constructor for the S4 \linkS4class{Tuned} object.
#'
#' @param model the tuned model
#' @param config (optional) name of the configuration selected for the final model
#' (e.g. 'opt' for configuration giving optimal accuracy during the repeated sampling procedure and
#' '1se' for configuration giving least complex model having performance metric within
#' 1SE from the optimal)
#' @param mscore the average performance metric across the sampling procedure
#' @param sem the standard error of the \code{mscore}
#' @param tuning the tuning method
#' @param sampling the sampling method used to tune the model
#' @param measure the accuracy metric
#' @param screened a \linkS4class{Screened}
#' @param xvars the number of features in the input matrix
#' @param xobs the number of observations in the input matrix
#' @param index the index of the selected configuration
#' @param trained a list containing the models trained during the resampling
#' @param validated a list containing the validation of the trained models
#' @param stability a list containing the features stability
#'
#'@rdname Tuned-class
Tuned <- function(
  model     = Trained(),
  config    = character(),
  mscore    = numeric(),
  sem       = numeric(),
  tuning    = character(),
  sampling  = character(),
  measure   = character(),
  screened  = Screened(),
  xvars     = integer(),
  xobs      = integer(),
  index     = integer(),
  trained   = list(),
  validated = list(),
  # importance= list()
  stability = list()
){
  methods::new(
    Class = "Tuned",
    model      = model    ,
    config     = config   ,
    mscore     = mscore   ,
    sem        = sem      ,
    tuning     = tuning   ,
    sampling   = sampling ,
    measure    = measure  ,
    screened   = screened ,
    xvars      = xvars    ,
    xobs       = xobs     ,
    index      = index    ,
    trained    = trained  ,
    validated  = validated,
    stability  = stability
    # importance = importance
    # extra
  )
}

methods::setMethod(f = "get_model",             signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'model')})
methods::setMethod(f = "get_config",            signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'config')})
methods::setMethod(f = "get_mean_score",        signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'mscore')})
methods::setMethod(f = "get_sem",               signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'sem') })
methods::setMethod(f = "get_tuning_method",     signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'tuning')})
methods::setMethod(f = "get_resampling_method", signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'sampling')})
methods::setMethod(f = "get_sampling",          signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'sampling')})
methods::setMethod(f = "get_measure",           signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'measure')})
methods::setMethod(f = "get_screened",          signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'screened')})
methods::setMethod(f = "get_xvars",             signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'xvars') })
methods::setMethod(f = "get_xobs",              signature = methods::signature(object = "Tuned"), definition = function(object){methods::slot(object = object, name = 'xobs') })
methods::setMethod(f = "get_index",             signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'index') })
methods::setMethod(f = "get_trained",           signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'trained')})
methods::setMethod(f = "get_validated",         signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'validated')})
# methods::setMethod(f = "get_nvars",             signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'nvars') })
# methods::setMethod(f = "get_nobs",              signature = methods::signature(object = "Tuned"), definition = function(object,...){methods::slot(object = object, name = 'nobs') })
methods::setMethod(f = "get_learning_method",   signature = methods::signature(object = "Tuned"), definition = function(object){get_learning_method(get_model(object)) })
methods::setMethod(f = "get_nfeatures",         signature = methods::signature(object = "Tuned"), definition = function(object){get_nfeatures(get_model(object)) })

methods::setMethod(f = "get_stability",         signature = "Tuned", definition = function(object){methods::slot(object = object, name = 'stability')})

methods::setMethod(f = "set_model",     signature = "Tuned", definition = function(object, value){methods::slot(object = object, name = 'model') = value; return(object) })
methods::setMethod(f = "set_trained",   signature = "Tuned", definition = function(object, value){methods::slot(object = object, name = 'trained') = value; return(object) })
methods::setMethod(f = "set_validated", signature = "Tuned", definition = function(object, value){methods::slot(object = object, name = 'validated') = value; return(object) })
methods::setMethod(f = "set_config",    signature = "Tuned", definition = function(object, value){methods::slot(object = object, name = 'config') = value; return(object) })

#'@keywords internal
methods::setMethod(
  f = "clean",
  signature = "Tuned",
  definition = function(object, rm.fit = FALSE, rm.call = FALSE){
    #trained
    trained = get_model(object)
    #clean
    trained = clean(trained, rm.call = rm.call)
    #update
    object = set_model(object = object, value = trained)
    #remove sampling fits
    if(rm.fit){
      #remove fits
      object = set_trained(object = object, value = list())
      #remove validate
      object = set_validated(object = object, value = list())
    }
    #return
    return(object)
  }
)

is.Tuned <- function(object){
  out = !is.na(match("Tuned", table = class(object)))
  return(out)
}


methods::setMethod(
  f = "get_learning",
  signature = methods::signature(object = "Tuned"),
  definition = function(object){
    #get method
    method = get_tuning_method(object);
    #switch name
    method = switch(
      method,
      'grid.search' = 'grid-search'
    )
    #get sampling
    sampling = get_sampling(object)
    #set
    out = paste("tuning by", method, "with", sampling)
    #ret
    return(out)
  }
)

methods::setMethod(
  f = "get_screening",
  signature = methods::signature(object = "Tuned"),
  definition = function(object){
    #get screened
    screened = get_screened(object)
    #get screening
    out = get_screening(screened)

    #ret
    return(out)
  }
)

#'Summary Table of a Tuned Object
#'
#'@description
#'This function produces a summary table of an object of class \linkS4class{Tuned}.
#'
#'@param object an object of class \linkS4class{Tuned}
#@param show.config logical, whether to report information on the configuration
#'
#'@return A data frame with the following elements:
#'\describe{
#'\item{\code{tuning}}{character string, the tuning strategy}
#'\item{\code{screening}}{character string, the screening strategy}
#'\item{\code{screened}}{integer, the number of screened features}
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
#'\code{\link{summary_table.Screened}}
#'\code{\link{summary_table.Trained}}
#'
#'@author Alessandro Barberis
summary_table.Tuned <- function(object){

  #--------------------------------------------------------------------------------------------#
  #Get tuning
  tuning = get_learning(object)

  #--------------------------------------------------------------------------------------------#
  #get Trained summary
  trained = summary_table(object = get_model(object))

  #--------------------------------------------------------------------------------------------#
  #get Screened summary
  screened = summary_table(object = get_screened(object))

  #--------------------------------------------------------------------------------------------#
  #bind
  out = data.frame(
    tuning = tuning,
    screened,
    trained,
    stringsAsFactors = F
  )

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}


#'Summary Table of a Tuned Object
#'
#@describeIn summary_table Summary of an object of class \linkS4class{Tuned}
#'
#'@inheritParams summary_table.Tuned
#'@inherit summary_table.Tuned return
#'
#'@author Alessandro Barberis
#'@export
#'
#'@rdname summary_table
methods::setMethod(
  f = "summary_table",
  signature = methods::signature(object = "Tuned"),
  definition = summary_table.Tuned
)
