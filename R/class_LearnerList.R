#' @include classes_generics.R class_Learner.R utils.R
NULL

#' Learner List Class
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
  Class = "LearnerList",
  contains = "SimpleList",
  slots = c(listData = "list")
)


#'LearnerList Constructor
#'
#'@description Constructor for the S4 \linkS4class{LearnerList} object.
#'
#'@author Alessandro Barberis
#'
#'@rdname LearnerList-class
LearnerList <- function(...){

  obj = new("LearnerList", S4Vectors::SimpleList(...))

  # S4Vectors::elementType(obj) = "Learner"
  obj@elementType = "Learner"
  obj

}

methods::setMethod(f = "get_forecaster", signature = "LearnerList", definition = function(object){lapply(X = object, FUN = get_forecaster)})
methods::setMethod(f = "get_id",         signature = "LearnerList", definition = function(object){sapply(X = object, FUN = get_id)})


methods::setMethod(
  f = "subset_list",
  signature = methods::signature(object = "LearnerList"),
  definition = function(object, id){

    if(!missing(id)){
      #keep
      keep = get_id(object) %in% id
      #subset
      object = object[keep]
    }

    if(length(object)==1){
      object = object[[1]]
    }

    return(object)
  }
)

is.LearnerList <- function(object){
  out = !is.na(match("LearnerList", table = class(object)))
  return(out)
}
