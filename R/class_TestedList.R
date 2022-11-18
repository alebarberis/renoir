#' @include classes_generics.R class_Tested.R
NULL

#' TestedList Class
#' An S4 class to represent a renoir tuned and tested models list
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
  Class = "TestedList",
  contains = "SimpleList"
  ,slots = c(listData = "list")
  # ,prototype = methods::prototype(elementType = "TunedAndTestedList")
)


#' TestedList Constructor
#' Constructor for the S4 \linkS4class{TestedList} object.
#' @param ... a comma-separated list of S4 \linkS4class{TestedList} objects.
TestedList <- function(...){

  obj = new("TestedList", S4Vectors::SimpleList(...))

  # S4Vectors::elementType(obj) = "TunedAndTested"
  obj@elementType = "Tested"
  obj
}

methods::setMethod(f = "get_set",     signature = "TestedList", definition = function(object){ sapply(X = object, FUN = get_set    )})
methods::setMethod(f = "get_measure", signature = "TestedList", definition = function(object){ sapply(X = object, FUN = get_measure)})
methods::setMethod(f = "get_score",   signature = "TestedList", definition = function(object){ lapply(X = object, FUN = get_score    )})
methods::setMethod(f = "get_mscore",  signature = "TestedList", definition = function(object){ sapply(X = object, FUN = get_mscore   )})
methods::setMethod(f = "get_sem",     signature = "TestedList", definition = function(object){ sapply(X = object, FUN = get_sem  )})
methods::setMethod(f = "get_ci",      signature = "TestedList", definition = function(object){ lapply(X = object, FUN = get_ci     )})
methods::setMethod(f = "get_opt",     signature = "TestedList", definition = function(object){ sapply(X = object, FUN = get_opt    )})
methods::setMethod(f = "get_1se",     signature = "TestedList", definition = function(object){ sapply(X = object, FUN = get_1se    )})

methods::setMethod(
  f = "subset_list",
  signature = methods::signature(object = "TestedList"),
  definition = function(object, set, measure){

    if(!missing(set)){
      #keep
      keep = get_set(object) %in% set
      #subset
      object = object[keep]
    }

    if(!missing(measure)){
      #keep
      keep = get_measure(object) %in% measure
      #subset
      object = object[keep]
    }

    if(length(object)==1){
      object = object[[1]]
    }

    return(object)
  }
)

methods::setMethod(
  f = "select_by",
  signature = methods::signature(object = "TestedList"),
  definition = function(object, set, measure){

    if(!missing(set)){
      #match the learning method
      index = match(x = set, table = get_set(object))

      #subset
      object = object[index]
    }

    if(!missing(measure)){
      #match the learning method
      index = match(x = measure, table = get_measure(object))
      #subset
      object = object[index]
    }

    if(length(object)==1){
      object = object[[1]]
    }

    return(object)
  }
)



#'Get the Summary of a TestedList Object
#'
#'@description
#'This function produces a summary of an object of class \linkS4class{TestedList}.
#'
#'@inheritParams summary_table.Tested
#'
#'@inherit summary_table.Tested return
#'
#'@seealso
#'\code{\link{summary_table.Renoir}},
#'\code{\link{summary_table.Evaluated}},
#'\code{\link{summary_table.EvaluatedList}},
#'\code{\link{summary_table.Tested}},
#'\code{\link{summary_table.Tuned}},
#'\code{\link{summary_table.Trained}}
#'
#'@author Alessandro Barberis
summary_table.TestedList <- function(object, best){
  #summary
  out = lapply(X = object, FUN = summary_table, best = best)
  #bind
  out = do.call(what = rbind, args = c(out, make.row.names = F, stringsAsFactors = F))
  #r
  return(out)
}


#'Get the Summary of a TestedList Object
#'
#@describeIn summary_table Summary of an object of class \linkS4class{TestedList}
#
#@inheritParams summary_table.TestedList
#
#@inherit summary_table.TestedList return
#
#@author Alessandro Barberis
methods::setMethod(
  f = "summary_table",
  signature = methods::signature(object = "TestedList"),
  definition = summary_table.TestedList)
