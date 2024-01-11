#' @include classes_generics.R class_Scorer.R utils.R
NULL

#'ScorerList Class
#'
#'An S4 class to represent a scorer list.
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
  Class = "ScorerList",
  contains = "SimpleList"
  ,slots = c(listData = "list")
  # ,prototype = methods::prototype(elementType = "Scorer")
)


#'ScorerList Constructor
#'
#'Constructor for the S4 ScorerList object.
#'
#'Constructor for the S4 \linkS4class{ScorerList} object.
#'
#'@param ... \linkS4class{Scorer} objects
#'
#'@return a \linkS4class{ScorerList} object
#'
#' @export
#' @rdname ScorerList-class
ScorerList <- function(...){

  obj = new("ScorerList", S4Vectors::SimpleList(...))

  # S4Vectors::elementType(obj) = "Scorer"
  obj@elementType = "Scorer"
  obj

}

# methods::setMethod(f = "get_measure", signature = "ScorerList", definition = function(object){sapply(X = object, FUN = get_measure)})
methods::setMethod(f = "get_id",      signature = "ScorerList", definition = function(object){sapply(X = object, FUN = get_id)})
methods::setMethod(f = "get_scorer",  signature = "ScorerList", definition = function(object){lapply(X = object, FUN = get_scorer)})
methods::setMethod(f = "get_grouped", signature = "ScorerList", definition = function(object){sapply(X = object, FUN = get_grouped)})
# methods::setMethod(f = "get_multi",   signature = "ScorerList", definition = function(object){sapply(X = object, FUN = get_multi)})
methods::setMethod(f = "get_parameters",   signature = "ScorerList", definition = function(object){lapply(X = object, FUN = get_parameters)})
methods::setMethod(f = "get_logger",  signature = "ScorerList", definition = function(object){lapply(X = object, FUN = get_logger)})
methods::setMethod(f = "get_optimum",   signature = "ScorerList", definition = function(object){sapply(X = object, FUN = get_optimum)})
methods::setMethod(f = "get_quantifier",   signature = "ScorerList", definition = function(object){lapply(X = object, FUN = get_quantifier)})

is.ScorerList <- function(object){
  out = !is.na(match("ScorerList", table = class(object)))
  return(out)
}

#'ScorerList Constructor
#'
#'@description Creates a list of supported scorers for the selected response type
#'@param resp.type response type
#'@return a \linkS4class{ScorerList}
#'
#'@rdname ScorerList-class
create_ScorerList <- function(
  resp.type,
  measures,
  grouped = TRUE,
  # multi   = c("average", "sum", "raw"),
  parameters,
  logger  = Logger()){

  #get supported measures
  if(missing(measures)){
    measures = get_available_summary_statistics(resp.type = resp.type)
  }

  #create scorers
  out = list()
  for(measure in measures){
    # out[[measure]] = Scorer(type.measure = measure, resp.type = resp.type, grouped = grouped, multi = multi, logger = logger)
    # out[[measure]] = Scorer(type.measure = measure, grouped = grouped, multi = multi, logger = logger)
    out[[measure]] = Scorer(id = measure, grouped = grouped, parameters = parameters, logger = logger)
  }

  #set as ScorerList
  out = ScorerList(out)

  return(out)
}

#Coerce to ScorerList
#
#@description Function to coerce a \linkS4class{Scorer} to a \linkS4class{ScorerList}
#@return a \linkS4class{ScorerList}
methods::setAs(
  from = "Scorer",
  to = "ScorerList",
  def = function(from)
    ScorerList(list(from))
)


#'Score
#'
#'@param scorer a \linkS4class{ScorerList} object
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'@param weights observation weights
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#'   \item{\code{raw}}{returns a vector containing one error for each output}
#'}
#'@param logger a \linkS4class{Logger} object
#'@param ... further arguments to scorer function
#'@return the computed accuracy measure for each \linkS4class{Scorer} in the \linkS4class{ScorerList}
#'
#'@rdname score
methods::setMethod(
  f = "score",
  signature = methods::signature(scorer = "ScorerList"),
  definition = function(scorer, true, pred, weights = NULL, multi = c("average", "sum", "raw"), logger, ...){

    #--------------------------------------------------------------------------------------------#
    if(missing(logger)){logger = get_logger(scorer[[1]])}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #match
    multi = match.arg(multi)

    #--------------------------------------------------------------------------------------------#
    out = list()

    for(i in seq(length(scorer))){
      #scorer
      iscorer = scorer[[i]]

      #get measure
      measure = get_id(iscorer)

      #log
      log_debug(object = logger, message = paste("Computing", measure), sep = "\n", add.level = TRUE, add.time = TRUE)

      #compute
      out[[measure]] = score(scorer = iscorer, true = true, pred = pred, weights = weights, multi = multi, ...)
    }

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)

#'Score
#'
#'@description Computes scores for different sets of data
#'@param scorer a \linkS4class{ScorerList} object
#'@param true a list of vectors (or matrices) of observed values. If list elements are matrices,
#'a multi-response is assumed
#'@param pred a list of vectors (or matrices) of predicted values
#'@param weights a list of vectors of observation weights
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#'}
#'@param grouped logical, whether to compute separate statistics when lists of values are provided
#'@param ... further arguments to scorer function
#'@return the computed accuracy measure for each \linkS4class{Scorer} in the \linkS4class{ScorerList}
#'
#'@rdname score
methods::setMethod(
  f = "score",
  signature = methods::signature(scorer = "ScorerList", true = "list", pred = "list"),
  definition = function(scorer, true, pred, weights = NULL, multi = c("average", "sum"), grouped = TRUE, logger, ...){

    #--------------------------------------------------------------------------------------------#
    if(missing(logger)){logger = get_logger(scorer[[1]])}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #match
    multi = match.arg(multi)

    #--------------------------------------------------------------------------------------------#
    out = list()

    for(i in seq(length(scorer))){
      #scorer
      iscorer = scorer[[i]]

      #get measure
      measure = get_id(iscorer)

      #log
      log_debug(object = logger, message = paste("Computing", measure), sep = "\n", add.level = TRUE, add.time = TRUE)

      #compute
      out[[measure]] = score(scorer = iscorer, true = true, pred = pred, weights = weights, multi = multi, grouped = grouped, ...)
    }

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)

#'Score Estimate
#'
#'@description Computes a score estimate given different sets of data
#'@param scorer a \linkS4class{ScorerList} object
#'@param true a list of vectors (or matrices) of observed values. If list elements are matrices,
#'a multi-response is assumed
#'@param pred a list of vectors (or matrices) of predicted values
#'@param weights a list of vectors of observation weights
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#'}
#'@param grouped logical, whether to compute separate statistics when lists of values are provided
#'@param logger a \linkS4class{Logger} object
#'@param ... further arguments to scorer function
#'@return list containing the computed score estimate (the average error) for each \linkS4class{Scorer} in the \linkS4class{ScorerList}
#'
#'@rdname mean_score
methods::setMethod(
  f = "mean_score",
  signature = methods::signature(scorer = "ScorerList", true = "list", pred = "list"),
  definition = function(scorer, true, pred, weights = NULL, multi = c("average", "sum"), grouped = TRUE, logger, ...){

    #--------------------------------------------------------------------------------------------#
    if(missing(logger)){logger = get_logger(scorer[[1]])}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #match
    multi = match.arg(multi)

    #--------------------------------------------------------------------------------------------#
    out = list()

    for(i in seq(length(scorer))){
      #scorer
      iscorer = scorer[[i]]

      #get measure
      measure = get_id(iscorer)

      #log
      log_debug(object = logger, message = paste("Computing", measure), sep = "\n", add.level = TRUE, add.time = TRUE)

      #compute
      out[[measure]] = mean_score(scorer = iscorer, true = true, pred = pred, weights = weights, multi = multi, grouped = grouped, ...)
    }

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)


#'Score
#'
#'@description Computes a score estimate given different sets of data, and returns the estimate
#'and the relative standard error
#'@param scorer a \linkS4class{ScorerList} object
#'@param true a list of vectors (or matrices) of observed values. If list elements are matrices,
#'a multi-response is assumed
#'@param pred a list of vectors (or matrices) of predicted values
#'@param weights a list of vectors of observation weights
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#'}
#'@param grouped logical, whether to compute separate statistics when lists of values are provided
#'@param logger a \linkS4class{Logger} object
#'@param ... further arguments to scorer function
#'@return list containing the score estimate and its standard error for each \linkS4class{Scorer} in the \linkS4class{ScorerList}
#'
#'@rdname summary_score
methods::setMethod(
  f = "summary_score",
  signature = methods::signature(scorer = "ScorerList", true = "list", pred = "list"),
  definition = function(scorer, true, pred, weights = NULL, multi = c("average", "sum"), grouped = TRUE, logger, ...){

    #--------------------------------------------------------------------------------------------#
    if(missing(logger)){logger = get_logger(scorer[[1]])}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #match
    multi = match.arg(multi)

    #--------------------------------------------------------------------------------------------#
    out = list()

    for(i in seq(length(scorer))){
      #scorer
      iscorer = scorer[[i]]

      #get measure
      measure = get_id(iscorer)

      #log
      log_debug(object = logger, message = paste("Computing", measure), sep = "\n", add.level = TRUE, add.time = TRUE)

      #compute
      out[[measure]] = summary_score(scorer = iscorer, true = true, pred = pred, weights = weights, multi = multi, grouped = grouped, ...)
    }

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)


methods::setMethod(
  f = "select_by",
  signature = methods::signature(object = "ScorerList"),
  definition = select_by_id
)
