#' @include classes_generics.R class_Tuned.R
NULL


#' TunedList Class
#' An S4 class to represent a list of tuned models.
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
  Class = "TunedList",
  contains = "SimpleList"
  ,slots = c(listData = "list")
  # ,prototype = methods::prototype(elementType = "Tuned")
)


#' TunedList Constructor
#' Constructor for the S4 TunedList object.
#'
#' Constructor for the S4 \linkS4class{TunedList} object.
TunedList <- function(...){

  obj = new("TunedList", S4Vectors::SimpleList(...))

  # S4Vectors::elementType(obj) = "TunedList"
  obj@elementType = "Tuned"
  obj

}

methods::setMethod(f = "get_model",             signature = "TunedList", definition = function(object){lapply(X = object, FUN = get_model            )})
methods::setMethod(f = "get_config",            signature = "TunedList", definition = function(object){sapply(X = object, FUN = get_config           )})
methods::setMethod(f = "get_mean_score",        signature = "TunedList", definition = function(object){sapply(X = object, FUN = get_mean_score       )})
methods::setMethod(f = "get_sem",               signature = "TunedList", definition = function(object){sapply(X = object, FUN = get_sem              )})
methods::setMethod(f = "get_tuning_method",     signature = "TunedList", definition = function(object){sapply(X = object, FUN = get_tuning_method    )})
methods::setMethod(f = "get_resampling_method", signature = "TunedList", definition = function(object){sapply(X = object, FUN = get_resampling_method)})
methods::setMethod(f = "get_measure",           signature = "TunedList", definition = function(object){sapply(X = object, FUN = get_measure          )})
methods::setMethod(f = "get_screened",          signature = "TunedList", definition = function(object){lapply(X = object, FUN = get_screened         )})
methods::setMethod(f = "get_xvars",             signature = "TunedList", definition = function(object){sapply(X = object, FUN = get_xvars            )})
methods::setMethod(f = "get_xobs",              signature = "TunedList", definition = function(object){sapply(X = object, FUN = get_xobs             )})
methods::setMethod(f = "get_index",             signature = "TunedList", definition = function(object){sapply(X = object, FUN = get_index            )})
methods::setMethod(f = "get_trained",           signature = "TunedList", definition = function(object){lapply(X = object, FUN = get_trained          )})
methods::setMethod(f = "get_validated",         signature = "TunedList", definition = function(object){lapply(X = object, FUN = get_validated        )})
methods::setMethod(f = "clean",                 signature = "TunedList", definition = function(object){lapply(X = object, FUN = clean                )})

is.TunedList <- function(object){
  out = !is.na(match("TunedList", table = class(object)))
  return(out)
}

