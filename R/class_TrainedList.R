#' @include classes_generics.R class_Trained.R
NULL

# setOldClass(c("List", "SimpleList"))
# setOldClass("List")


#' TrainedList Class
#' An S4 class to represent a renoir trained and tested models list
#'
#' This subclass extends the S4  \linkS4class{List} virtual class by using
#' the  \linkS4class{SimpleList} implementation.
#'
#' The object consists of 4 slots
#' @slot listData \code{list} storing the list elements
#' @slot elementType the type of data represented in the sequence
#' @slot elementMetadata  \linkS4class{DataFrame} storing the element-wise metadata,
#' with a row for each element and a column for each metadata available. Default is \code{NULL}
#' @slot metadata \code{list} storing the global metadata annotating the object as a whole
methods::setClass(
  Class = "TrainedList",
  contains = "SimpleList"
  ,slots = c(listData = "list")
  # ,prototype = methods::prototype(elementType = "RenoirTrainedAndTested")
)


#' TrainedList Constructor
#' Constructor for the S4 TrainedList object.
#'
#' Constructor for the S4 \linkS4class{TrainedList} object.
TrainedList <- function(...){

  obj = new("TrainedList", S4Vectors::SimpleList(...))

  # S4Vectors::elementType(obj) = "RenoirTrainedAndTested"
  obj@elementType = "Trained"
  obj

}


methods::setMethod(f = "get_fit",             signature = "TrainedList", definition = function(object){lapply(X = object, FUN = get_fit)})
methods::setMethod(f = "get_config",          signature = "TrainedList", definition = function(object){lapply(X = object, FUN = get_config)})
methods::setMethod(f = "get_learning_method", signature = "TrainedList", definition = function(object){lapply(X = object, FUN = get_learning_method)})

methods::setMethod(f = "add_screened_nvar_to_config",   signature = "TrainedList", definition = function(object, value){TrainedList(lapply(X = object, FUN = add_screened_nvar_to_config, value = value))})
methods::setMethod(f = "del_screened_nvar_from_config", signature = "TrainedList", definition = function(object){TrainedList(lapply(X = object, FUN = del_screened_nvar_from_config))})
methods::setMethod(f = "get_screened_nvar_from_config", signature = "TrainedList", definition = function(object){TrainedList(lapply(X = object, FUN = get_screened_nvar_from_config))})


#'
#' @rdname  predict-methods
#' @aliases predict
#'
#' @param  object     object of type \linkS4class{TrainedList}.
#'
#' @export
methods::setMethod(
  f = "predict",
  signature = "TrainedList",
  definition = function(object, ...){

    out = lapply(X = object, FUN = predict, ...)

    return(out)
  }
)


methods::setMethod(
  f = "add_config_element",
  signature = "TrainedList",
  definition = function(object, value){

    out = lapply(X = object, FUN = add_config_element, value = value)

    return(out)
  }
)

methods::setMethod(
  f = "del_config_element",
  signature = "TrainedList",
  definition = function(object, value){

    out = lapply(X = object, FUN = del_config_element, value = value)

    return(out)
  }
)

methods::setMethod(
  f = "clean",
  signature = "TrainedList",
  definition = function(object){

    out = lapply(X = object, FUN = clean)

    return(out)
  }
)

methods::setAs(
  from = "Trained",
  to = "TrainedList",
  def = function(from)
    TrainedList(list(from))
)

is.TrainedList <- function(object){
  out = !is.na(match("TrainedList", table = class(object)))
  return(out)
}

#' #'@keywords internal
#' methods::setMethod(
#'   f = "get_predlist",
#'   signature = methods::signature("TrainedList"),
#'   definition = function(object, ...)
#'   {
#'
#'     predlist = lapply(X = object, FUN = get_predlist, ...)
#'
#'     return(predlist)
#' })
