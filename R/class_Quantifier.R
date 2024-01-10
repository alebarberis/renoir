#' @include classes_generics.R class_Logger.R confidence.R utils.R
NULL

#' Quantifier Class
#'
#' @description
#' An S4 class providing the methods to quantify uncertainty.
#'
#' The object consists of 6 slots
#' @slot id a character string
#' @slot quantifier a function
#' @slot parameters a list
#' @slot logger a \linkS4class{Logger}
#' @author Alessandro Barberis
methods::setClass(
  Class = "Quantifier",
  slots = c(
    id         = "character",
    quantifier = "function",
    parameters = "list",
    logger     = "Logger"
  )
)


#' Constructor for the S4 Quantifier object.
#'
#' @description
#' Constructor for the S4 \linkS4class{Quantifier} object.
#'
#' @param id the quantifier method name associated to this \linkS4class{Quantifier}.
#' If the method is one of the supported by renoir, the constructor will
#' automatically select a \code{quantifier}. See \code{supported_uncertainty_quantifiers()}
#' for the supported methods.
#' @param quantifier (optional) function to quantify uncertainty.
#' Must be provided if \code{id} is not one of the supported by renoir.
# If \code{scorer} is provided it must conform to the renoir common interface,
# and must have the following formal arguments:
# \describe{
#    \item{x}{a vector (or a matrix) of performance metrics. If a matrix is provided, a multi-response is assumed}
#    \item{w}{priors of the observations}
#    \item{...}{additional arguments can be added}
# }
#' @param parameters list containing the parameters to fix for the chosen quantifier
#' @param logger a \linkS4class{Logger}
#' @author Alessandro Barberis
#'
#' @rdname Quantifier-class
Quantifier <- function(
  id = "ciwm",
  quantifier,
  parameters,
  logger  = Logger()
){

  if(missing(quantifier) && (id %in% supported_uncertainty_quantifiers())){
    quantifier   = get_quantifier_function(id)
  } else if(!missing(quantifier)){
    if(id %in% supported_uncertainty_quantifiers()){
      stop(paste("'id' not valid, please change name. It must be different from the built-in renoir method names:\n",paste(supported_uncertainty_quantifiers(), collapse = ", "),"\n"))
    }
  } else {
    stop("Please provide a valid 'id' and/or 'quantifier'.\n")
  }

  #Check parameters
  if(missing(parameters) || is.null(parameters) || length(parameters)==0){
    parameters = list()
  }

  methods::new(
    Class = "Quantifier",
    id         = id,
    quantifier = quantifier,
    parameters = parameters,
    logger     = logger
  )
}

#'@keywords internal
get_quantifier_function <- function(id){

  #set function
  fun = switch(
    id,
    'ciwm' = ciwm,
    ciwm
  )

  return(fun)
}

is.Quantifier <- function(object){
  out = !is.na(match("Quantifier", table = class(object)))
  return(out)
}


methods::setMethod(f = "get_id",          signature = "Quantifier", definition = function(object){methods::slot(object = object, name = 'id')})
methods::setMethod(f = "get_quantifier",  signature = "Quantifier", definition = function(object){methods::slot(object = object, name = 'quantifier')})
methods::setMethod(f = "get_parameters",  signature = "Quantifier", definition = function(object){methods::slot(object = object, name = 'parameters')})
methods::setMethod(f = "get_logger",      signature = "Quantifier", definition = function(object){methods::slot(object = object, name = 'logger')})

#'Quantify Uncertainty
#'@description This function quantitatively characterizes the uncertainties in the model performance.
#'@param quantifier a \linkS4class{Quantifier} object
#'@param x a vector (or a matrix) of values. If a matrix is provided,
#'a multi-response is assumed
#'@param weights observation weights
#'@param ... further arguments to quantifier function
#'@return the computed quantification of the uncertainty
#'@author Alessandro Barberis
#'@rdname quantify
methods::setMethod(
  f = "quantify",
  signature = methods::signature(quantifier = "Quantifier"),
  # definition = function(quantifier, x, weights = NULL, ...){
  definition = function(quantifier, ...){

    #get parameters
    parameters = get_parameters(quantifier)

    #check
    parameters = check_parameters(parameters, ...)

    #merge
    args = c(parameters, list(...))

    #get scorer
    quantifier = get_quantifier(quantifier)

    #compute score
    # out = do.call(what = quantifier, args = c(list(x = x, weights = weights), args))
    out = do.call(what = quantifier, args = args)

    #return
    return(out)
  }
)

#'@keywords internal
supported_uncertainty_quantifiers <- function(){
  out = c("ciwm")
  return(out)
}
