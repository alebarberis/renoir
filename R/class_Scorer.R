#' @include classes_generics.R class_Logger.R class_Quantifier.R accuracy.R performance.R utils.R
NULL

#' Scorer Class
#'
#' @description
#' An S4 class providing the methods to compute an accuracy measure.
#'
#' The object consists of 6 slots
#' @slot id the accuracy measure
#' @slot scorer function to be used for scoring
#' @slot parameters list containing the parameters to fix for the scoring function
#' @slot grouped logical, whether to compute separate statistics when lists of values are provided
# @slot multi what to do when response has multiple output values
#\describe{
#   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#   \item{\code{raw}}{returns a vector containing one error for each output}
#}
#When lists of values are provided, \code{multi} must be one of \code{average} or \code{sum}
#' @slot selector function to select optimal measure given a vector of values
#' @slot optimum the optimisation problem to solve for selecting the optimal accuracy measure.
#' Two possible values are \code{min} and \code{max}
#' @slot quantifier a \linkS4class{Quantifier}
#' @slot logger a \linkS4class{Logger}
#' @author Alessandro Barberis
methods::setClass(
  Class = "Scorer",
  slots = c(
    id         = "character",
    scorer     = "function",
    parameters = "list",
    grouped    = "logical",
    # multi      = "character",
    selector   = "function",
    optimum    = "character",
    quantifier = "Quantifier",
    logger     = "Logger"
  )
)

#' Constructor for the S4 Scorer object.
#'
#' Constructor for the S4 \linkS4class{Scorer} object.
#' @param id the scoring method name associated to this \linkS4class{Scorer}.
#' If scoring method is one of the accuracy measures supported by renoir, the constructor will
#' automatically select a \code{scorer} and a \code{selector}. See \code{supported_scoring_methods()}
#' for the supported methods.
#' @param scorer (optional) function to evaluate a model performance.
#' Must be provided if \code{id} is not one of the accuracy measures supported by renoir.
#' If \code{scorer} is provided it must conform to the renoir common interface,
#' and must have the following formal arguments:
#' \describe{
#'    \item{true}{a vector (or a matrix) of observed values. If a matrix is provided, a multi-response is assumed}
#'    \item{pred}{a vector (or a matrix) of predicted values}
#'    \item{weights}{priors of the observations}
#'    \item{multi}{what to do when response has multiple output values}
#'    \item{...}{additional arguments can be added}
#' }
#' @param parameters list containing the parameters to fix for the chosen scoring method
#' @param resp.type (optional) response type. If provided, the selected \code{id} is checked
#' to see if available for the considered response type.
#' @param grouped logical, whether to compute separate statistics when lists of values are provided
#' @param selector (optional) function to select optimal accuracy measure from multiple values
#' Must be provided if \code{id} is not one of the accuracy measures supported by renoir.
#' If \code{selector} is provided it must conform to the renoir common interface,
#' and must have the following formal arguments:
#' \describe{
#'    \item{measures}{a vector of accuracy measures returned by this \linkS4class{Scorer}}
#' }
#' @param optimum the optimisation problem to solve for selecting the optimal accuracy measure
#' from multiple values
#' @param quantifier a \linkS4class{Quantifier}
#' @param logger a \linkS4class{Logger}
#' @author Alessandro Barberis
#' @export
Scorer <- function(
  # type.measure = c("default", "mse", "deviance", "class", "auc", "mae", "C", "rmse", "mape", "r2", "msle"),
  # type.measure = c("mse", "mdev", "mclass", "auc", "mae", "C", "rmse", "mape", "r2", "msle",
  #                  "squared_error", "deviance", "class", "absolute_error", "ape", "sle"),
  id,
  scorer,
  parameters,
  resp.type,
  grouped = TRUE,
  # multi   = c("average", "sum", "raw"),
  selector,
  optimum,
  quantifier = Quantifier(),
  logger  = Logger()
){

  if(missing(scorer) && (id %in% supported_scoring_methods())){

    #check
    if(!missing(resp.type)){
      id = check_type_measure(type.measure = id, resp.type = resp.type)
    }

    scorer   = get_scorer_function(id)
    selector = get_optimal_score_selector_function(id)
    optimum  = get_optimisation_problem(id)
  } else if(!missing(scorer)){
    if(id %in% supported_scoring_methods()){
      stop(paste("'id' not valid, please change name. It must be different from the built-in renoir method names:\n",paste(supported_scoring_methods(), collapse = ", "),"\n"))
    }
  } else {
    stop("Please provide a valid 'id' and/or 'scorer'.\n")
  }

  #Check provided prediction function
  check_provided_scorer_function(scorer)
  check_provided_optimal_score_selector_function(selector)

  #Check parameters
  if(missing(parameters) || is.null(parameters) || length(parameters)==0){
    parameters = list()
  }

  methods::new(
    Class = "Scorer",
    id         = id,
    scorer     = scorer,
    selector   = selector,
    optimum    = optimum,
    parameters = parameters,
    grouped    = grouped,
    quantifier = quantifier,
    logger     = logger
  )

  # multi = match.arg(multi)
  #
  # if(missing(scorer)){
  #   # #match arg
  #   # type.measure = match.arg(type.measure)
  #   #
  #   # #check
  #   # type.measure = check_type_measure(type.measure = type.measure, resp.type = resp.type)
  #
  #   #get fun
  #   # scorer = get_scorer_function(type.measure = type.measure)
  #   scorer = get_scorer_function(type.measure = type.measure, resp.type = resp.type)
  # } else {
  #   if(!is.function(scorer)){stop("Provided 'scorer' is not a function. Please, check the input data.\n")}
  # }
  #
  # methods::new(
  #   Class = "Scorer",
  #   type.measure = type.measure,
  #   scorer       = scorer,
  #   grouped      = grouped,
  #   multi        = multi,
  #   logger       = logger
  # )
}

get_scorer_function <- function(id){

  # #set misclassification
  # misclassification_error = switch(
  #   resp.type,
  #   binomial = binomial_misclassification,
  #   multinomial = multinomial_misclassification,
  #   binomial_misclassification
  # )

  #set function
  fun = switch(
    id,
    #measures
    'squared_error' = squared_error,
    'absolute_error'= absolute_error,
    'ape'           = absolute_percentage_error,
    'sle'           = squared_log_error,
    'true_pred'     = true_pred,
    'false_pred'    = false_pred,

    #summary measures
    ##REGRESSION
    'mse'      = mean_squared_error,
    'msle'     = mean_squared_log_error,
    'rmse'     = root_mean_squared_error,
    'mae'      = mean_absolute_error,
    'mape'     = mean_absolute_percentage_error,
    'r2'       = r2_score,
    ##CLASSIFICATION
    'acc'         = accuracy_score,
    'class'       = classification_error_rate,
    'precision'   = positive_predictive_value,
    'sensitivity' = true_positive_rate,
    'f1s'         = f1_score,
    'jaccard'     = threat_score,
    'auc'         = area_under_roc_curve

    #STILL TO IMPLEMENT
    # 'C'           = c_index,
    # 'deviance'    = deviance
  )

  return(fun)
}

check_provided_scorer_function <- function(scorer){

  if(missing(scorer)){
    stop("'scorer' is missing with no default.\n")
  } else {
    #needed formals
    # formals.def = c("x", "y", "weights", "offset", "resp.type", "observations", "features")
    formals.def = c("true", "pred", "weights", "multi")

    #get formals
    formals.fun = names(formals(fun = scorer))

    #check
    # if(any(!(formals.fun %in% formals.def))){
    if(any(!(formals.def %in% formals.fun))){

      stop(paste0("Provided scorer function without required formal arguments.Function interface must match renoir requirements. Try with:\n
      function(", paste0(formals.def, collapse = ", "),"...){
        #Compute accuracy measure
        # out = YOUR CODE

        #Return computed accuracy
        return(out)
      }
      \n"))
    }
  }
}


get_optimal_score_selector_function <- function(id){

  id = tolower(id)

  out = if(id %in% c("auc", "c", "c-index", "acc", "f1s", "jaccard", "precision", "sensitivity")){
    function(measures){max(measures, na.rm = TRUE)}
  }else{
    function(measures){min(measures, na.rm = TRUE)}
  }
  return(out)
}

get_optimisation_problem <- function(id){

  #default is minimisation problem
  out = "min"

  #lower
  id = tolower(id)

  if(id %in% c("auc", "c", "c-index", "acc", "f1s", "jaccard", "precision", "sensitivity")){
    out = "max"
  }

  return(out)
}

check_provided_optimal_score_selector_function <- function(f){

  if(missing(f)){
    stop("'selector' is missing with no default.\n")
  } else {
    #needed formals
    # formals.def = c("x", "y", "weights", "offset", "resp.type", "observations", "features")
    formals.def = c("measures")

    #get formals
    formals.fun = names(formals(fun = f))

    #check
    # if(any(!(formals.fun %in% formals.def))){
    if(any(!(formals.def %in% formals.fun))){

      stop(paste0("Provided selector function without required formal arguments. Function interface must match renoir requirements. Try with:\n
      function(", paste0(formals.def, collapse = ", "),", ...){
        #Select optimal accuracy measure
        # out = YOUR CODE

        #Return selected accuracy
        return(out)
      }
      \n"))
    }
  }
}

# methods::setMethod(f = "get_metric", signature = "Scorer", definition = function(object){methods::slot(object = object, name = 'metric')})

methods::setMethod(f = "get_measure", signature = "Scorer", definition = function(object){methods::slot(object = object, name = 'id')})
methods::setMethod(f = "get_id",      signature = "Scorer", definition = function(object){methods::slot(object = object, name = 'id')})
methods::setMethod(f = "get_scorer",  signature = "Scorer", definition = function(object){methods::slot(object = object, name = 'scorer')})
methods::setMethod(f = "get_grouped", signature = "Scorer", definition = function(object){methods::slot(object = object, name = 'grouped')})
# methods::setMethod(f = "get_multi",   signature = "Scorer", definition = function(object){methods::slot(object = object, name = 'multi')})
methods::setMethod(f = "get_multi",   signature = "Scorer", definition = function(object){get_parameters(object)$multi})
methods::setMethod(f = "get_logger",  signature = "Scorer", definition = function(object){methods::slot(object = object, name = 'logger')})
methods::setMethod(f = "get_parameters",  signature = "Scorer", definition = function(object){methods::slot(object = object, name = 'parameters')})
methods::setMethod(f = "get_selector",  signature = "Scorer", definition = function(object){methods::slot(object = object, name = 'selector')})
methods::setMethod(f = "get_optimum",   signature = "Scorer", definition = function(object){methods::slot(object = object, name = 'optimum')})
methods::setMethod(f = "get_quantifier",   signature = "Scorer", definition = function(object){methods::slot(object = object, name = 'quantifier')})

is.Scorer <- function(object){
  out = !is.na(match("Scorer", table = class(object)))
  return(out)
}

methods::setMethod(
  f = "select_optimal_score",
  signature = "Scorer",
  definition = function(scorer, measures, ...){

    #extract selector function
    selector_fun = get_selector(scorer)

    #Select optimal score
    out = selector_fun(measures = measures, ...)

    #return
    return(out)
})


methods::setMethod(
  f = "update_scorer",
  signature = "Scorer",
  definition = function(object, measure){
    #get scorer measure
    type.measure = get_id(object)
    #check
    if(!identical(type.measure, measure)){
      object = Scorer(
        id = measure,
        grouped = get_grouped(object),
        # multi   = get_multi(object),
        parameters = get_parameters(object),
        logger  = get_logger(object)
      )
    }
    #return
    return(object)
})


#'Score
#'@param scorer a \linkS4class{Scorer} object
#'@param true a vector (or a matrix) of observed values. If a matrix is provided,
#'a multi-response is assumed
#'@param pred a vector (or a matrix) of predicted values
#'@param weights observation weights
#@param multi what to do when response has multiple output values
#\describe{
#   \item{\code{average}}{errors of multiple outputs are averaged to get a single value for each observation}
#   \item{\code{sum}}{errors of multiple outputs are summed up to get a single value for each observation}
#   \item{\code{raw}}{returns a vector containing one error for each output}
#}
#'@param ... further arguments to scorer function
#'@return the computed accuracy measure
methods::setMethod(
  f = "score",
  signature = methods::signature(scorer = "Scorer"),
  definition = function(scorer, true, pred, weights = NULL, ...){

    #get parameters
    parameters = get_parameters(scorer)

    #check
    parameters = check_parameters(parameters, ...)

    #merge
    args = c(parameters, list(...))

    #get scorer
    scorer = get_scorer(scorer)

    #compute score
    out = do.call(what = scorer, args = c(list(true = true, pred = pred, weights = weights), args))

    #return
    return(out)
  }
)


# methods::setMethod(
#   f = "score",
#   signature = methods::signature(scorer = "Scorer"),
#   definition = function(scorer, true, pred, weights = NULL, multi = c("average", "sum", "raw"), ...){
#
#     #multi - set default
#     multi = match.arg(multi)
#
#     #get parameters
#     parameters = get_parameters(scorer)
#
#     #get further aguments
#     args = list(...)
#
#     #check if any provided and override
#     m = match(x = names(parameters), table = names(args))
#     #remove na
#     m = m[!is.na(m)]
#     #check
#     if(length(m)>0){parameters = parameters[-m]}
#     #merge
#     args = c(parameters, args)
#
#     #get scorer
#     scorer = get_scorer(scorer)
#
#     #compute score
#     out = do.call(what = scorer, args = c(list(true = true, pred = pred, weights = weights, multi = multi), args))
#
#     #return
#     return(out)
#   }
# )
#
# methods::setMethod(
#   f = "score",
#   signature = methods::signature(scorer = "Scorer", true = "matrix", pred = "matrix", weights = "matrix"),
#   definition = function(object, true, pred, weights = NULL, multi = c("average", "sum", "raw"), ...){
#
#     #multi - set default
#     multi = match.arg(multi)
#
#     #get scorer
#     scorer = get_scorer(object)
#
#     #compute score
#     out = do.call(what = scorer, args = c(list(true = true, pred = pred, weights = weights, multi = multi), list(...)))
#
#     #return
#     return(out)
#   }
# )

#'Score
#'@description Computes scores for different sets of data
#'@param scorer a \linkS4class{Scorer} object
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
#'@param min.obs integer, minimum number of observations per list. If the number of
#'observations per list is less than \code{min.obs}, an attempt to compute errors at the
#'observation level (for the unlisted observations)
#'and then summarise is made (equivalent to \code{grouped = FALSE}). It is working only for certain
#'accuracy measures. Default is \code{min.obs = 3}. Set \code{min.obs = 0} to suppress.
#'@param logger a \linkS4class{Logger}
#'@param ... further arguments to scorer function
#'@return the computed accuracy measure
methods::setMethod(
  f = "score",
  signature = methods::signature(scorer = "Scorer", true = "list", pred = "list"),
  definition = function(scorer, true, pred, weights = NULL, multi = c("average", "sum"), grouped = TRUE, min.obs = 3, logger, ...){

    ltrue = length(true)
    lpred = length(pred)

    #Set observation weights to 1 if missing
    if(is.null(unique(unlist(weights)))){
      weights = lapply(X = true, FUN = get_nobs, na.rm = FALSE)
      weights = lapply(X = weights, FUN = rep.int, x = 1L)
    }
    lw    = length(weights)

    if(ltrue!=lpred || ltrue!=lw || lpred!=lw){stop("Input lists must have same lenght.\n")}

    #--------------------------------------------------------------------------------------------#
    if(missing(logger)){logger = get_logger(scorer)}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    if(missing(grouped)){grouped = get_grouped(object = scorer)}

    #--------------------------------------------------------------------------------------------#
    #get min number of obs
    minobs = min(sapply(X = true, FUN = get_nobs, na.rm = FALSE), na.rm = T)

    if ((minobs < min.obs) && grouped && is_supported_ungrouped(get_id(scorer))) {
      warning(paste("Option grouped=FALSE enforced, since <", min.obs, "observations per set.\n"), call. = FALSE)
      log_debug(object = logger, message = paste("Minimum number of observations:", minobs), sep = "\n", add.level = TRUE, add.time = TRUE)
      #set grouped
      grouped = FALSE
      #get new measure
      measure = switch_to_supported_ungrouped_measure(measure = get_id(scorer))
      log_debug(object = logger, message = paste("Accuracy measure switched to", measure), sep = "\n", add.level = TRUE, add.time = TRUE)
      #update scorer
      scorer = update_scorer(object = scorer, measure = measure)
    }

    #--------------------------------------------------------------------------------------------#
    #multi - set default
    multi = match.arg(multi)
    #check the provided
    # tmp = get_multi(scorer)
    tmp = get_parameter(scorer, "multi")
    if(!is.na(tmp) && !identical(tmp, "raw")){multi = tmp; rm(tmp);}

    #--------------------------------------------------------------------------------------------#
    #set error list
    scorelist = list()

    #--------------------------------------------------------------------------------------------#
    #Compute score
    if(grouped){
      log_debug(object = logger, message = paste("Computing scores for each sample set"), sep = "\n", add.level = TRUE, add.time = TRUE)

      N = ltrue

      #Compute score for each list element (i.e. sample run)
      for(i in seq(N)){
        #select
        itrue = true[[i]]
        ipred = pred[[i]]
        iw    = weights[[i]]

        #Set infinite to NA
        ipred[is.infinite(ipred)] = NA

        #compute measure
        log_trace(object = logger, message = paste(i, "Computing scores..."), sep = "", add.level = TRUE, add.time = TRUE)
        scorelist[[i]] = do.call(what = score, args = c(list(scorer = scorer, true = itrue, pred = ipred, weights = iw, multi = multi), list(...)))
        log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

        # #compute mean error
        # err = stats::weighted.mean(x = err, w = iw, na.rm = TRUE)

        #update weights
        weights[[i]] = sum(iw, na.rm = TRUE)
      }

      #Create
      scorev  = do.call(what = c, args = scorelist)
      weights = do.call(what = c, args = weights)

      #clean
      rm(itrue, ipred, iw)
    } else {
      #Compute score using all data
      # log_debug(object = logger, message = paste("Computing scores using all data"), sep = "\n", add.level = TRUE, add.time = TRUE)

      #Unlist and combine
      pred    = do.call(what = c, args = pred)
      true    = do.call(what = c, args = true)
      weights = do.call(what = c, args = weights)

      #N observations
      N = get_nobs(pred, na.rm = TRUE)

      #Compute score
      log_debug(object = logger, message = paste("Computing scores using all data..."), sep = "", add.level = TRUE, add.time = TRUE)
      scorev = do.call(what = score, args = c(list(scorer = scorer, true = true, pred = pred, weights = weights, multi = multi), list(...)))
      log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    }

    # #--------------------------------------------------------------------------------------------#
    # #Compute mean error
    # log_debug(object = logger, message = paste(i, "Computing mean error..."), sep = "", add.level = TRUE, add.time = TRUE)
    # wmeanscore = stats::weighted.mean(x = scorev, w = weights, na.rm = TRUE)
    # log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    #
    # #--------------------------------------------------------------------------------------------#
    # #Compute standard error
    # log_debug(object = logger, message = paste(i, "Computing estimate of standard error..."), sep = "", add.level = TRUE, add.time = TRUE)
    # #Biased weighted sample variance
    # wvar = stats::weighted.mean(x = (scorev - meanscore)^2, w = weights, na.rm = TRUE)
    # sderr = sqrt(x = wvar/(N - 1))
    # sderr = wse(x = scorev, w = weights, m = wmeanscore, bessel = TRUE)
    # log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    #
    # #--------------------------------------------------------------------------------------------#
    # #create output object
    # out = list(merr = meanscore, sderr = sderr)

    out = scorev

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)

#'Score Estimate
#'@description Computes a score estimate given different sets of data
#'@param scorer a \linkS4class{Scorer} object
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
#'@param min.obs integer, minimum number of observations per list. If the number of
#'observations per list is less than \code{min.obs}, an attempt to compute errors at the
#'observation level (for the unlisted observations)
#'and then summarise is made (equivalent to \code{grouped = FALSE}). It is working only for certain
#'accuracy measures. Default is \code{min.obs = 3}. Set \code{min.obs = 0} to suppress.
#'@param logger a \linkS4class{Logger}
#'@param ... further arguments to scorer function
#'@return the computed score estimate (the average error)
methods::setMethod(
  f = "mean_score",
  signature = methods::signature(scorer = "Scorer", true = "list", pred = "list"),
  definition = function(scorer, true, pred, weights = NULL, multi = c("average", "sum"), grouped = TRUE, min.obs = 3, logger, ...){

    #--------------------------------------------------------------------------------------------#
    if(missing(logger)){logger = get_logger(scorer)}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #match
    multi = match.arg(multi)
    #check the provided
    # tmp = get_multi(scorer)
    tmp = get_parameter(scorer, "multi")
    if(!is.na(tmp) && !identical(tmp, "raw")){multi = tmp; rm(tmp);}

    #--------------------------------------------------------------------------------------------#
    #Check
    if(missing(grouped)){grouped = get_grouped(object = scorer)}

    #Set observation weights to 1 if missing
    if(is.null(unique(unlist(weights)))){
      weights = lapply(X = true, FUN = get_nobs, na.rm = FALSE)
      weights = lapply(X = weights, FUN = rep.int, x = 1L)
    }

    #--------------------------------------------------------------------------------------------#
    #get min number of obs
    minobs = min(sapply(X = true, FUN = get_nobs, na.rm = FALSE), na.rm = T)

    if ((minobs < min.obs) && grouped && is_supported_ungrouped(get_id(scorer))) {
      warning(paste("Option grouped=FALSE enforced, since <", min.obs, "observations per set.\n"), call. = FALSE)
      #set grouped
      grouped = FALSE
      #get new measure
      measure = switch_to_supported_ungrouped_measure(measure = get_id(scorer))
      log_debug(object = logger, message = paste("Accuracy measure switched to", measure), sep = "\n", add.level = TRUE, add.time = TRUE)
      #update scorer
      scorer = update_scorer(object = scorer, measure = measure)
    }

    #--------------------------------------------------------------------------------------------#
    #Compute scores
    log_debug(object = logger, message = paste("Computing scores"), sep = "\n", add.level = TRUE, add.time = TRUE)
    scores = score(scorer = scorer, true = true, pred = pred, weights = weights, multi = multi,
                   grouped = grouped, min.obs = min.obs, logger = logger, ...)

    #--------------------------------------------------------------------------------------------#

    if(grouped){
      #set sample size
      N = length(true)
      #
      weights = lapply(X = weights, FUN = sum, na.rm = TRUE)

    } else {
      #Unlist and combine
      pred    = do.call(what = c, args = pred)
      #N observations
      N = get_nobs(pred, na.rm = TRUE)
    }

    #Unlist and combine
    weights = do.call(what = c, args = weights)

    #--------------------------------------------------------------------------------------------#
    #Compute mean error
    # log_debug(object = logger, message = paste("Computing mean error..."), sep = "", add.level = TRUE, add.time = TRUE)
    log_debug(object = logger, message = paste("Computing weighted mean as estimate of the error..."), sep = "", add.level = TRUE, add.time = TRUE)
    out = stats::weighted.mean(x = scores, w = weights, na.rm = TRUE)
    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)

#Standard Error of the Mean (SEM) score
#'@param min.obs integer, minimum number of observations per list. If the number of
#'observations per list is less than \code{min.obs}, an attempt to compute errors at the
#'observation level (for the unlisted observations)
#'and then summarise is made (equivalent to \code{grouped = FALSE}). It is working only for certain
#'accuracy measures. Default is \code{min.obs = 3}. Set \code{min.obs = 0} to suppress.
#'@param logger a \linkS4class{Logger}
methods::setMethod(
  f = "se_score",
  signature = methods::signature(scorer = "Scorer", true = "list", pred = "list"),
  definition = function(
    scorer, true, pred, weights = NULL, multi = c("average", "sum"),
    grouped = TRUE, min.obs = 3,
    logger, ...){

    #--------------------------------------------------------------------------------------------#
    if(missing(logger)){logger = get_logger(scorer)}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #match
    multi = match.arg(multi)
    #check the provided
    # tmp = get_multi(scorer)
    tmp = get_parameter(scorer, "multi")
    if(!is.na(tmp) && !identical(tmp, "raw")){multi = tmp; rm(tmp);}

    #--------------------------------------------------------------------------------------------#
    #Check
    if(missing(grouped)){grouped = get_grouped(object = scorer)}

    #Set observation weights to 1 if missing
    if(is.null(unique(unlist(weights)))){
      weights = lapply(X = true, FUN = get_nobs, na.rm = FALSE)
      weights = lapply(X = weights, FUN = rep.int, x = 1L)
    }

    #--------------------------------------------------------------------------------------------#
    #get min number of obs
    minobs = min(sapply(X = true, FUN = get_nobs, na.rm = FALSE), na.rm = T)

    if ((minobs < min.obs) && grouped && is_supported_ungrouped(get_id(scorer))) {
      warning(paste("Option grouped=FALSE enforced, since <", min.obs, "observations per set.\n"), call. = FALSE)
      #set grouped
      grouped = FALSE
      #get new measure
      measure = switch_to_supported_ungrouped_measure(measure = get_id(scorer))
      log_debug(object = logger, message = paste("Accuracy measure switched to", measure), sep = "\n", add.level = TRUE, add.time = TRUE)
      #update scorer
      scorer = update_scorer(object = scorer, measure = measure)
    }

    #--------------------------------------------------------------------------------------------#
    # #set args
    # args = c(as.list(environment()), list(...));

    #--------------------------------------------------------------------------------------------#
    #Compute scores
    log_debug(object = logger, message = paste("Computing scores"), sep = "\n", add.level = TRUE, add.time = TRUE)
    scores = score(scorer = scorer, true = true, pred = pred, weights = weights, multi = multi,
                   grouped = grouped, min.obs = min.obs, logger = logger, ...)
    #
    #     #clean
    #     rm(args)

    #--------------------------------------------------------------------------------------------#

    if(grouped){
      #set sample size
      N = length(true)
      #
      weights = lapply(X = weights, FUN = sum, na.rm = TRUE)

    } else {
      #Unlist and combine
      pred    = do.call(what = c, args = pred)
      #N observations
      N = get_nobs(pred, na.rm = TRUE)
    }

    #Unlist and combine
    weights = do.call(what = c, args = weights)

    #--------------------------------------------------------------------------------------------#
    #Compute mean error
    log_debug(object = logger, message = paste("Computing weighted mean as estimate of the error..."), sep = "", add.level = TRUE, add.time = TRUE)
    merr = stats::weighted.mean(x = scores, w = weights, na.rm = TRUE)
    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#
    #Compute standard error
    log_debug(object = logger, message = paste("Computing estimate of standard error..."), sep = "", add.level = TRUE, add.time = TRUE)
    #Biased weighted sample variance
    # wvar = stats::weighted.mean(x = (scores - merr)^2, w = weights, na.rm = TRUE)
    # sderr = sqrt(x = wvar/(N - 1))
    out = wse(x = scores, w = weights, m = merr, bessel = TRUE, n = N)
    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)

#'Score
#'@description Computes a score estimate given different sets of data, and returns the estimate
#'and the relative standard error
#'@param scorer a \linkS4class{Scorer} object
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
#'@param min.obs integer, minimum number of observations per list. If the number of
#'observations per list is less than \code{min.obs}, an attempt to compute errors at the
#'observation level (for the unlisted observations)
#'and then summarise is made (equivalent to \code{grouped = FALSE}). It is working only for certain
#'accuracy measures. Default is \code{min.obs = 3}. Set \code{min.obs = 0} to suppress.
#'@param logger a \linkS4class{Logger}
#'@param return.raw logical, whether to return scores used to compute estimate
#'@param ... further arguments to scorer function
#'@return list of 2 elements containing a score estimate and its standard error
methods::setMethod(
  f = "summary_score",
  signature = methods::signature(scorer = "Scorer", true = "list", pred = "list"),
  definition = function(
    scorer, true, pred, weights = NULL, multi = c("average", "sum"), grouped = TRUE, min.obs = 3,
    logger, return.raw = FALSE,
    confidence = 0.95, distribution = "normal", ...){

    #--------------------------------------------------------------------------------------------#
    if(missing(logger)){logger = get_logger(scorer)}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #match
    multi = match.arg(multi)
    #check the provided
    # tmp = get_multi(scorer)
    tmp = get_parameter(scorer, "multi")
    if(!is.na(tmp) && !identical(tmp, "raw")){multi = tmp; rm(tmp);}

    #--------------------------------------------------------------------------------------------#
    #Check
    # if(missing(grouped)){grouped = get_grouped(object = scorer)}

    #Set observation weights to 1 if missing
    if(is.null(unique(unlist(weights)))){
      weights = lapply(X = true, FUN = get_nobs, na.rm = FALSE)#get number of observations
      weights = lapply(X = weights, FUN = rep.int, x = 1L)#set to 1
    }

    #--------------------------------------------------------------------------------------------#
    #get min number of obs
    minobs = min(sapply(X = true, FUN = get_nobs, na.rm = FALSE), na.rm = T)

    if ((minobs < min.obs) && grouped && is_supported_ungrouped(get_id(scorer))) {
      warning(paste("Option grouped=FALSE enforced, since <", min.obs, "observations per set.\n"), call. = FALSE)
      log_debug(object = logger, message = paste("Minimum number of observations:", minobs), sep = "\n", add.level = TRUE, add.time = TRUE)
      #set grouped
      grouped = FALSE
      #get new measure
      measure = switch_to_supported_ungrouped_measure(measure = get_id(scorer))
      log_debug(object = logger, message = paste("Accuracy measure switched to", measure), sep = "\n", add.level = TRUE, add.time = TRUE)
      #update scorer
      scorer = update_scorer(object = scorer, measure = measure)
    }

    #--------------------------------------------------------------------------------------------#
    # #set args
    # args = c(as.list(environment()), list(...));

    #--------------------------------------------------------------------------------------------#
    #Compute scores
    log_debug(object = logger, message = paste("Computing scores"), sep = "\n", add.level = TRUE, add.time = TRUE)
    scores = score(scorer = scorer, true = true, pred = pred, weights = weights, multi = multi,
                   grouped = grouped, min.obs = min.obs, logger = logger, ...)
    #
    #     #clean
    #     rm(args)

    #--------------------------------------------------------------------------------------------#

    if(grouped){
      #set sample size
      N = length(true)
      #sum observation weights in sample
      weights = lapply(X = weights, FUN = sum, na.rm = TRUE)

    } else {
      #Unlist and combine
      pred    = do.call(what = c, args = pred)
      #N observations
      N = get_nobs(pred, na.rm = TRUE)
    }

    #Unlist and combine
    weights = do.call(what = c, args = weights)

    #--------------------------------------------------------------------------------------------#
    # Compute mean metric and related uncertainty
    log_debug(object = logger, message = paste("Computing the weighted mean as estimate of the performance and the related uncertainty..."), sep = "", add.level = TRUE, add.time = TRUE)
    out = quantify(quantifier = get_quantifier(scorer), x = scores, weights = weights, confidence = confidence, distribution = distribution, n = N)
    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#

    if(return.raw){
      out = c(out, list(s = scores, w = weights, N = ifelse(test = N>1, yes = N - 1, no = N)))
    }

    class(out) = "Scored"
    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)

check_resampling_type_measure = function (
  type.measure = "mse",
  resp.type = "gaussian",
  grouped = TRUE)
{

  type.measure = type.measure[1]

  type.measure = check_type_measure(type.measure = type.measure, resp.type = resp.type)

  if(grouped){
    supported.measures = c("mse", "mdev", "mclass", "auc", "mae", "C", "rmse", "mape", "r2", "msle")
  } else {
    #supported measures
    supported.measures = c("squared_error", "deviance", "class", "absolute_error", "ape", "sle")
  }


  #check if supported
  is.supported = type.measure %in% supported.measures

  if(!is.supported){
    logMsg = paste("The selected accuracy measure is not supported. Please, select one of the following:\n",supported.measures,"\n")
    stop(logMsg)
  }

  return(type.measure)
}


get_score_full_name <- function(type = c("mean_score", "summary_score", "se_score", "score")){
  type = match.arg(score.type)

  type = switch(
    type,
    "mean_score"    = "estimate of mean score",
    "summary_score" = "estimate of mean score and its standard error",
    "se_score"      = "standard error of the estimate",
    "score"         = "score"
  )

}

#'Switch accuracy
#'@description Utility function used to switch accuracy measure when
#'\code{grouped = FALSE}
#'@return measure name
switch_to_supported_ungrouped_measure <- function(measure){

  measure = switch(
    measure,
    "mse"            = "squared_error",
    "rmse"           = "squared_error",
    "mape"           = "ape",
    "msle"           = "sle",
    "rmsle"          = "sle",
    "mae"            = "absolute_error",
    "squared_error"  = "squared_error",
    "absolute_error" = "absolute_error",
    "ape"            = "ape",
    "sle"            = "sle",
    "class"          = "false_pred",
    "acc"            = "true_pred",
    "mbd"            = "ubd",
    "mpd"            = "upd",
    NA
  )

  return(measure)

}

has_matched_supported_ungrouped_measures <- function(measure){
  out = measure %in% grouped_measures_w_matched_supported_ungrouped_measures()
  return(out)
}

grouped_measures_w_matched_supported_ungrouped_measures <- function(){
  out = c("mse", "deviance", "class", "mae", "C", "rmse", "mape", "r2", "msle", "ape", "sle", "acc")

  return(out)
}

supported_ungrouped_measures <- function(){
  out = c("squared_error", "absolute_error", "ape", "sle", "deviance", "C", "true_pred", "false_pred")

  return(out)
}

is_supported_ungrouped_measure <- function(measure){
  out = measure %in% supported_ungrouped_measures()
  return(out)
}

is_supported_ungrouped <- function(measure){
  out = is_supported_ungrouped_measure(measure) || has_matched_supported_ungrouped_measures(measure)
  return(out)
}

supported_scoring_methods <- function(){

  #distances
  out = supported_errors()

  #summary statistics
  out = unique(c(out, supported_summary_statistics()))

  #return
  return(out)
}

supported_errors = function(){
  out = c("squared_error", "absolute_error", "ape", "sle", "true_pred", "false_pred")
  return(out)
}

supported_summary_statistics = function(){
  out = c("mse", "deviance", "class", "mae", "C", "rmse", "mape", "r2", "msle", "f1s", "acc", "precision", "sensitivity", "jaccard", "auc")
}
