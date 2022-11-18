#' @include classes_generics.R class_Forecaster.R utils.R
NULL

#' Forecaster List Class
#'
#' An S4 class to represent a scorer list.
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
methods::setClass(
  Class = "ForecasterList",
  contains = "SimpleList",
  slots = c(listData = "list")
)


#'ForecasterList Constructor
#'
#' @title Constructor for the S4 ForecasterList object.
#'
#'Constructor for the S4 \linkS4class{ForecasterList} object.
ForecasterList <- function(...){

  obj = new("ForecasterList", S4Vectors::SimpleList(...))

  # S4Vectors::elementType(obj) = "Forecaster"
  obj@elementType = "Forecaster"
  obj

}

methods::setMethod(f = "get_forecaster",      signature = "ForecasterList", definition = function(object){sapply(X = object, FUN = get_forecaster)})
# methods::setMethod(f = "get_selector",        signature = "ForecasterList", definition = function(object){sapply(X = object, FUN = get_selector)})
# methods::setMethod(f = "get_learning_method", signature = "ForecasterList", definition = function(object){sapply(X = object, FUN = get_learning_method)})
methods::setMethod(f = "get_id", signature = "ForecasterList", definition = function(object){sapply(X = object, FUN = get_id)})

#'Select forecaster
#'@description Select the first forecaster from the list matching the \code{learning.method}
#'@param object a S4 \linkS4class{ForecasterList} object
#'@param by the learning method to match. If missing, the \code{object} is returned
#'as it is. In case of no match \code{NULL} is returned.
#'@return a \linkS4class{Forecaster} object
methods::setMethod(
  f = "select_forecaster",
  signature = methods::signature(object = "ForecasterList", by = "character"),
  definition = function(object, by){select_by_id(object, by)}
)

methods::setMethod(
  f = "select_forecaster",
  signature = methods::signature(object ="ForecasterList", by = "Trained"),
  definition = function(object, by){
    if(!missing(by)){
      #get learning method
      learning.method = get_learning_method(by)

      #select
      object = select_forecaster(object = object, by = learning.method)
    }

    return(object)
  }
)

#'Subset forecaster
#'@description Select the forecasters from the list matching the learning method in \code{by}
#'@param object a S4 \linkS4class{ForecasterList} object
#'@param by the learning method to match. If missing, the \code{object} is returned
#'as it is. In case of no match \code{NULL} is returned.
#'@return a \linkS4class{ForecasterList} object
methods::setMethod(
  f = "subset_forecaster",
  signature = methods::signature(object = "ForecasterList", by = "character"),
  definition = function(object, by){
    if(!missing(by)){
      #match the learning method
      index = match(x = by, table = get_id(object))

      #subset
      object = object[index]
    }

    return(object)
  }
)

methods::setMethod(
  f = "get_selector",
  signature = "ForecasterList",
  definition = function(object, learning.method, ...){

    #subset
    object = select_forecaster(object = object, by = learning.method)

    #get selector
    out = sapply(X = object, FUN = get_selector, ...)

    #return
    return(out)
  }
)

#'Get prediction type
#'@description Select the appropriate prediction type given the accuracy measure
#'@param object a S4 \linkS4class{ForecasterList} object
#'@param type.measure the accuracy measure
#'@param learning.method (optional) learning method(s) to consider. If provided,
#'the \code{learning.method} is matched against the learning methods in the \linkS4class{ForecasterList}
#'object.
#'@return the prediction type required to compute the prediction values expected
#'in input by a \linkS4class{Scorer} to calculate the score.
#'If \code{learning.method} is missing or multiple values are provided then the
#'output is a vector containing the matching prediction types, one for each
#'learning method in \code{learning.method}.
methods::setMethod(
  f = "get_prediction_type",
  signature = "ForecasterList",
  definition = function(object, type.measure, learning.method, ...){

    #subset
    if(!missing(learning.method)){
      object = subset_forecaster(object = object, by = learning.method)
    }

    #get prediction type
    out = sapply(X = object, FUN = get_prediction_type, type.measure = type.measure, ...)

    #return
    return(out)
  }
)


#Coerce to ForecasterList
#
#@description Function to coerce a \linkS4class{Forecaster} to a \linkS4class{ForecasterList}
#@return a \linkS4class{ForecasterList}
methods::setAs(
  from = "Forecaster",
  to = "ForecasterList",
  def = function(from)
    ForecasterList(list(from))
)



methods::setMethod(
  f = "forecast",
  signature = methods::signature(forecaster = "ForecasterList", models = "Trained"),
  definition = function(forecaster, models, ...){

    #Select a Forecaster from the ForecasterList to match the
    #learning method of Trained
    forecaster = select_forecaster(forecaster, by = models)

    #compute prediction
    out = forecast(forecaster = forecaster, models = models, ...)

    #return
    return(out)
  }
)


methods::setMethod(
  f = "forecast",
  signature = methods::signature(forecaster = "ForecasterList", models = "TrainedList"),
  definition = function(forecaster, models, ...){

    #Check forecasters are provided for each used learning method
    isok = all( unique(unlist(get_learning_method(models))) %in% unique(unlist(get_id(forecaster))) )

    if(!isok){
      missingid = setdiff(x = unique(unlist(get_learning_method(models))), y = unique(unlist(get_id(forecaster))))

      stop(paste("Missing Forecaster for the following learning methods found in the 'models' list:", paste0(missingid, collapse = ", "), "\n"))
    } else {
      #Compute predictions
      out = lapply(X = models, FUN = forecast, forecaster = forecaster, ...)
    }

    #return
    return(out)
  }
)
