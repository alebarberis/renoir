#' @include classes_generics.R
NULL

#' Filtered Class
#' An S4 class providing a container for the results of pre-processing.
#' @slot method a character string
#' @slot filtered the filtered data
#' @slot index vector containing the index of the retained features in the original matrix
#' @slot summary a text summary of the pre-processing
#' @author Alessandro Barberis
methods::setClass(
  Class = "Filtered",
  slots = c(
    method     = "character",
    filtered   = "ANY",
    index      = "integer",
    summary    = "character"
  )
)

#'@keywords internal
Filtered <- function(
  method     = character(),
  filtered   = matrix(),
  index      = integer(),
  summary    = character()
){

  methods::new(
    Class = "Filtered",
    method     = method,
    filtered   = filtered,
    index      = index,
    summary    = summary
  )
}

methods::setMethod(f = "get_method",   signature = "Filtered", definition = function(object){methods::slot(object = object, name = 'method')})
methods::setMethod(f = "get_filtered", signature = "Filtered", definition = function(object){methods::slot(object = object, name = 'filtered')})
methods::setMethod(f = "get_index",    signature = "Filtered", definition = function(object){methods::slot(object = object, name = 'index')})
methods::setMethod(f = "get_summary",  signature = "Filtered", definition = function(object){methods::slot(object = object, name = 'summary')})

# methods::setMethod(f = "print",  signature = "Filtered", definition = function(object){methods::slot(object = object, name = 'summary')})

#'Summary Table of a Filtered Object
#'
#'@description
#'This function produces a summary table of an object of class \linkS4class{Filtered}.
#'
#'@param object an object of class \linkS4class{Filtered}
#@param show.config logical, whether to report information on the configuration
#'
#'@return A data frame with the following elements:
#'\describe{
#'\item{filtering}{character string, the filtering strategy}
#'\item{filtered}{character, a textual summary of the performed pre-processing}
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
#'
#'@keywords internal
summary_table.Filtered <- function(object){

  #get method
  filtering = get_method(object = object)

  #get summary
  filtered = get_summary(object = object)

  #create output
  out = data.frame(
    filtering = filtering,
    filtered  = filtered,
    stringsAsFactors = F
  )

  #r
  return(out)
}

#'Summary Table of a Filtered Object
#'
#@describeIn get_summary Summary of an object of class \linkS4class{Filtered}
#'
#'@inheritParams summary_table.Filtered
#'@inherit summary_table.Filtered return
#'
#'@author Alessandro Barberis
#'@export
#'
#'@rdname summary_table
methods::setMethod(
  f = "summary_table",
  signature = "Filtered",
  definition = summary_table.Filtered
)
