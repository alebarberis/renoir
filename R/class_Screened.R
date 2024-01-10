#' @include classes_generics.R class_Screened.R
NULL

#' Screened Class
#' @description An S4 class providing a container for the results of a screening procedure.
#' @slot method the used screening method
#' @slot n the maximum number of variables kept
#' @slot index  the index of kept features
#' @slot score the score values resulting from screening procedure
#' @author Alessandro Barberis
methods::setClass(
  Class = "Screened",
  slots = c(
    method = "character",
    n      = "numeric",
    index  = "numeric",
    score  = "numeric"
  )
)

#'@export
#'@rdname Screened-class
Screened <- function(
  method = character(),
  n      = numeric(),
  index  = numeric(),
  score  = numeric()
){
  methods::new(
    Class = "Screened",
    method = method,
    n      = n,
    index  = index,
    score  = score
  )
}

methods::setMethod(f = "get_method", signature = "Screened", definition = function(object){methods::slot(object = object, name = 'method')})
methods::setMethod(f = "get_n",      signature = "Screened", definition = function(object){methods::slot(object = object, name = 'n')})
methods::setMethod(f = "get_index",  signature = "Screened", definition = function(object){methods::slot(object = object, name = 'index')})
methods::setMethod(f = "get_score",  signature = "Screened", definition = function(object){methods::slot(object = object, name = 'score')})

methods::setMethod(f = "set_method", signature = "Screened", definition = function(object, value){ methods::slot(object = object, name = 'method') = value; return(object)})
methods::setMethod(f = "set_n",      signature = "Screened", definition = function(object, value){ methods::slot(object = object, name = 'n') = value; return(object)})
methods::setMethod(f = "set_index",  signature = "Screened", definition = function(object, value){ methods::slot(object = object, name = 'index') = value; return(object)})
methods::setMethod(f = "set_score",  signature = "Screened", definition = function(object, value){ methods::slot(object = object, name = 'score') = value; return(object)})

methods::setMethod(
  f = "update_n",
  signature = "Screened",
  definition = function(object, value){

    #get n
    n = get_n(object)
    #check
    if(value <= n){
      #set n
      object = set_n(object, value)
      #get index
      index = get_index(object)
      #update
      index = index[1:value]
      #set index
      object = set_index(object, index)
    } else {
      warning("Values greater than the one used for feature selection not allowed. Screened object not updated.")
    }
    #return
    return(object)
})


methods::setMethod(
  f = "isEmpty",
  signature = "Screened",
  definition = function(x){

    obj = Screened(
      method = character(),
      n      = numeric(),
      index  = numeric(),
      score  = numeric()
    )

    out = identical(x, obj)

    #return
    return(out)
  })


methods::setMethod(
  f = "get_screening",
  signature = methods::signature(object = "Screened"),
  definition = function(object){

    #check
    if(!isEmpty(object)){
      #get method
      method = get_method(object);
      if(isTRUE(!is.na(method) & method!="")){
        l = length(method)
        if(l>1){
          #get method name
          method = lapply(X = method, FUN = get_name_screener_by_id)
          #paste
          out = paste0("combined strategies (", paste(method, collapse = ", "), ")")
        } else {
          out = get_name_screener_by_id(method)
        }
      }

    } else {
      out = ""
    }

    #ret
    return(out)
  }
)


#'Summary Table of a Screened Object
#'
#'@description
#'This function produces a summary table of an object of class \linkS4class{Screened}.
#'
#'@param object an object of class \linkS4class{Screened}
#'
#'@return A data frame with the following elements:
#'\describe{
#'\item{\code{screening}}{character string, the screening strategy}
#'\item{\code{screened}}{integer, the number of screened features}
#'}
#'
#'@seealso
#'\code{\link{summary_table.Renoir}},
#'\code{\link{summary_table.Evaluated}},
#'\code{\link{summary_table.EvaluatedList}},
#'\code{\link{summary_table.Tested}}
#'\code{\link{summary_table.TestedList}},
#'\code{\link{summary_table.Tuned}}
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
summary_table.Screened <- function(object){

  #check
  if(isEmpty(x = object)){
    # out = data.frame(stringsAsFactors = F)
    screening = screened = NA
  } else {
    #get method
    # screening = get_method(object)
    screening = get_screening(object)

    #get number of features
    screened = length(get_index(object))

    # #create output
    # out = data.frame(
    #   screening = screening,
    #   screened  = screened,
    #   stringsAsFactors = F
    # )
  }

  #create output
  out = data.frame(
    screening = screening,
    screened  = screened,
    stringsAsFactors = F
  )

  #return
  return(out)
}


#'Summary Table of a Screened Object
#'
#@describeIn summary_table Summary of an object of class \linkS4class{Screened}
#'
#'@inheritParams summary_table.Screened
#'@inherit summary_table.Screened return
#'
#'@author Alessandro Barberis
#'@export
#'@rdname summary_table
methods::setMethod(
  f = "summary_table",
  signature = methods::signature(object = "Screened"),
  definition = summary_table.Screened
)
