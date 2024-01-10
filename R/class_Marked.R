#' @include classes_generics.R
NULL

#' Marked
#' An S4 class to represent a marked model
#'
#' The Marked object consists of 3 slots
#' @slot mark A \code{data.frame} containing the feature grades computed on the data
# @slot type.measure the considered accuracy measure
#' @slot measure the considered accuracy measure
# @slot config the model configuration
#' @slot set the data set considered
# @slot marking.system the name of the considered marking system
methods::setClass(
  Class = "Marked",
  slots = c(
    mark            = "data.frame",
    # config          = "character",
    measure         = "character",
    # type.measure    = "character",
    # marking.system  = "character",
    set             = "character"
  )
)

### -------------------------------------------------------------------------
#' Constructor for the S4 Marked object.
#'
#' Constructor for the S4 \linkS4class{Marked} object.
#'
#' @rdname Marked-class
Marked <- function(
  mark = data.frame(),
  # config          = character(),
  measure         = character(),
  # type.measure    = character(),
  set             = character()
  # ,marking.system  = character()
  ){

  methods::new(
    Class = "Marked",
    mark           = mark,
    # config         = config,
    measure        = measure,
    # type.measure   = type.measure,
    # marking.system = marking.system
    set            = set

  )
}


methods::setMethod(f = "get_mark",           signature = "Marked", definition = function(object){methods::slot(object = object, name = 'mark')})
# methods::setMethod(f = "get_config",         signature = "Marked", definition = function(object){methods::slot(object = object, name = 'config')})
# methods::setMethod(f = "get_measure",        signature = "Marked", definition = function(object){methods::slot(object = object, name = 'type.measure')})
methods::setMethod(f = "get_measure",        signature = "Marked", definition = function(object){methods::slot(object = object, name = 'measure')})
methods::setMethod(f = "get_set",            signature = "Marked", definition = function(object){methods::slot(object = object, name = 'set')})
# methods::setMethod(f = "get_marking_system", signature = "Marked", definition = function(object){methods::slot(object = object, name = 'marking.system')})


# methods::setMethod(f = "set_data",   signature = "Learned", definition = function(object, value){methods::slot(object = object, name = 'data') = value; return(object)})

methods::setMethod(
  f = "isEmpty",
  signature = "Marked",
  definition = function(x){

    obj = Marked(
      mark    = data.frame(),
      measure = character(),
      set     = character()
    )

    out = identical(x, obj)

    #return
    return(out)
  })

#'@keywords internal
get_annotated_marks <- function(x, annotation, feat.index = 0, check.names = TRUE){

  #create df
  if(is.na(match(x = 'features', colnames(x)))){
    x = data.frame(features = rownames(x), x, stringsAsFactors = FALSE, check.names = check.names);
  } else {x = x}

  #if annotation is present, match the data with the current features
  if(!missing(annotation) && !is.na(annotation) && !is.null(annotation)){
    if(identical(x = feat.index, y = 0)){
      annotation = data.frame(features = rownames(annotation), annotation, stringsAsFactors = FALSE, check.names = check.names);
      feat.index = 1;
    }
    #merge
    x = merge(
      x = x,
      y = annotation,
      by.x = "features",
      by.y = colnames(annotation)[1],
      all.x = T
    )

  }

  #ret
  return(x)

}
