#' @include classes_generics.R class_Screener.R
NULL

#'ScreenerList Class
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
#'
methods::setClass(
  Class = "ScreenerList",
  contains = "SimpleList"
  ,slots = c(listData = "list")
  # ,prototype = methods::prototype(elementType = "Screener")
)


#'ScreenerList Constructor
#'
#'Constructor for the S4 ScreenerList object.
#'
#'Constructor for the S4 \linkS4class{ScreenerList} object.
#'@param ... \linkS4class{Screener} objects to be stored in a \linkS4class{ScreenerList}
#'@return a \linkS4class{ScreenerList} object
#' @export
#'@author Alessandro Barberis
ScreenerList <- function(...){

  obj = new("ScreenerList", S4Vectors::SimpleList(...))

  # S4Vectors::elementType(obj) = "Screener"
  obj@elementType = "Screener"
  obj

}

methods::setMethod(f = "get_id",         signature = "ScreenerList", definition = function(object){sapply(X = object, FUN = get_id)})
methods::setMethod(f = "get_screener",   signature = "ScreenerList", definition = function(object){lapply(X = object, FUN = get_screener)})
methods::setMethod(f = "get_parameters", signature = "ScreenerList", definition = function(object){sapply(X = object, FUN = get_parameters)})

#'Features screening
#'@description This function performs a feature screening by using the provided screeners. It combines
#'the results from the different screeners and return a \linkS4class{Screened} object
#'@param screener a \linkS4class{ScreenerList} object
#'@param x the input matrix, where rows are observations and columns are variables.
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}.
#'@param weights priors of the observations
#'@param resp.type the response type
#'@param observations (optional) indices of observations to keep
#'@param combine how to combine the results from multiple screeners
#'\describe{
#'   \item{\code{max}}{the max value of scores across multiple \linkS4class{Screened} objects is selected to get a single value for each observation}
#'   \item{\code{average}}{scores of multiple \linkS4class{Screened} objects are averaged to get a single value for each observation}
#'   \item{\code{sum}}{scores of multiple \linkS4class{Screened} objects are summed up to get a single value for each observation}
#'}
#'@param order whether to order the results by the score values
#'\describe{
#'   \item{\code{increasing}}{results are sorted in increasing order}
#'   \item{\code{decreasing}}{results are sorted in decreasing order}
#'   \item{\code{none}}{results are not sorted}
#'}
#'@param cutoff (optional) numeric, the value to use as a threshold for filtering not significant variables
#'@param maxvars integer, the maximum number of variables to keep. If the filtering is returning an higher number of variables,
#' only the most significant \code{maxvars} variables are kept.
#'@return a \linkS4class{Screened} object
#'@author Alessandro Barberis
methods::setMethod(
  f = "screen",
  signature = methods::signature(screener ="ScreenerList"),
  definition = function(
    screener,
    x,
    y,
    weights = NULL,
    # offset,
    resp.type,
    observations = NULL,
    # features,
    combine = c("max", "average", "sum"),

    #further args
    order = c("increasing", "decreasing"),
    cutoff = NULL,
    maxvars = NULL,

    logger = Logger(verbose = F),

    ...){

    #--------------------------------------------------------------------------------------------#
    #match
    combine = match.arg(combine)
    order   = match.arg(order)

    #--------------------------------------------------------------------------------------------#
    #logger
    if(missing(logger)){logger = get_logger(screener[[1]])}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    if(length(maxvars)>1){
      warning("Multiple values passed as 'maxvars'. Only maximum value is kept.\n")
      maxvars = max(maxvars, na.rm = TRUE)
    }

    #--------------------------------------------------------------------------------------------#
    #loop over methods
    log_trace(object = logger, message = "Loop over screening methods", sep = "\n", add.level = TRUE, add.time = TRUE)
    screened = lapply(X = screener, FUN = screen, x = x, y = y, weights = weights, resp.type = resp.type, observations = observations,
           order = "none", cutoff = NULL, maxvars = NULL, logger = logger, ...)

    #--------------------------------------------------------------------------------------------#
    #Get data
    log_trace(object = logger, message = "Extract results", sep = "\n", add.level = TRUE, add.time = TRUE)
    scores  = lapply(X = screened, FUN = get_score)

    # #--------------------------------------------------------------------------------------------#
    # #order by input features
    # log_trace(object = logger, message = "Order results", sep = "\n", add.level = TRUE, add.time = TRUE)
    # score = score[get_index(screened)]

    #--------------------------------------------------------------------------------------------#
    #Combine results
    log_trace(object = logger, message = "Bind results", sep = "\n", add.level = TRUE, add.time = TRUE)
    scores = do.call(what = cbind, args = scores)

    #--------------------------------------------------------------------------------------------#
    #Select the least significant among the values
    log_trace(object = logger, message = paste("Combine results by selecting the", combine, "values..."), sep = "", add.level = TRUE, add.time = TRUE)
    score = multiresponse_screener(screened = scores, multi = combine)
    log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#
    #Get an ordered list of elements (increasing p-value)
    if(identical(order, "increasing")){
      log_trace(object = logger, message = "Order by increasing significance...", sep = "", add.level = TRUE, add.time = TRUE)
      ordered = order(score, decreasing = FALSE, na.last = TRUE);
      log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    } else if(identical(order, "decreasing")){
      log_trace(object = logger, message = "Order by decreasing significance...", sep = "", add.level = TRUE, add.time = TRUE)
      ordered = order(score, decreasing = T, na.last = TRUE);
      log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    } else {
      ordered = seq(length(score))
    }

    #--------------------------------------------------------------------------------------------#
    #If threshold was provided, get the index of the elements
    #whose significance is < than threshold (ordered by significance)
    if(!is.null(cutoff) && !S4Vectors::isEmpty(cutoff)){
      log_trace(object = logger, message = "Select most significant...", sep = "", add.level = TRUE, add.time = TRUE)
      index = which(score < cutoff);
      #intersect
      ordered = intersect(x = ordered, y = index);
      log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    }

    #--------------------------------------------------------------------------------------------#
    #If the number of significant elements is greater than maxvars, limit the results
    if(!S4Vectors::isEmpty(maxvars) && length(ordered)>maxvars){
      #Select first n elements
      log_trace(object = logger, message = paste("Select top", maxvars, "elements..."), sep = "", add.level = TRUE, add.time = TRUE)
      ordered = ordered[1:maxvars]
      log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    }

    #--------------------------------------------------------------------------------------------#
    #set output object

    #Check there are variables left
    if(isTRUE(is.na(unique(ordered))) | isTRUE(S4Vectors::isEmpty(ordered))){
      out = Screened(
        method = get_id(screener),
        n      = 0,
        index  = NA,
        score  = score
      )
    } else {
      out = Screened(
        method = get_id(screener),
        n      = length(ordered),
        index  = ordered,
        score  = score
      )
    }
    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)
