#' @include classes_generics.R
NULL

#' Tested
#' An S4 class to represent a tested model
#'
#' The Tested object consists of 3 slots
#' @slot set object of class "character"
#' @slot measure object of class "character"
#' @slot score a numeric vector
#' @slot mscore a length-one numeric vector
#' @slot sem a length-one numeric vector
#' @slot ci a numeric vector
#' @slot opt a length-one integer vector
#' @slot 1se a length-one integer vector
methods::setClass(
  Class = "Tested",
  slots = c(
    set     = "character",
    measure = "character",
    score   = "numeric",
    mscore  = "numeric",
    sem     = "numeric",
    ci      = "numeric",
    opt     = "integer",
    `1se`   = "integer"
    # config  = "character",
    # type.measure    = "character",
    # marking.system  = "character"
  )
)

methods::setMethod(f = "get_set",     signature = "Tested", definition = function(object){ methods::slot(object = object, name = 'set')})
methods::setMethod(f = "get_measure", signature = "Tested", definition = function(object){ methods::slot(object = object, name = 'measure')})
methods::setMethod(f = "get_score",   signature = "Tested", definition = function(object){ methods::slot(object = object, name = 'score')})
methods::setMethod(f = "get_mscore",  signature = "Tested", definition = function(object){ methods::slot(object = object, name = 'mscore') })
methods::setMethod(f = "get_sem",     signature = "Tested", definition = function(object){ methods::slot(object = object, name = 'sem') })
methods::setMethod(f = "get_ci",      signature = "Tested", definition = function(object){ methods::slot(object = object, name = 'ci') })
methods::setMethod(f = "get_opt",     signature = "Tested", definition = function(object){ methods::slot(object = object, name = 'opt') })
methods::setMethod(f = "get_1se",     signature = "Tested", definition = function(object){ methods::slot(object = object, name = '1se') })



### -------------------------------------------------------------------------
#' Constructor for the S4 Tested object.
#'
#' Constructor for the S4 \linkS4class{Tested} object.
#'
#' @param set name of the set
#' @param measure name of the considered performance metric
#' @param score performance metrics
#' @param mscore mean of the performance metrics
#' @param sem standard error of the mean
#' @param ci confidence interval
#' @param opt index of model associated to optimal performance
#' @param 1se index of least complex model associated to performance within 1SE from the optimal
#'
#' @author Alessandro Barberis
Tested <- function(
  set     = character(),
  measure = character(),
  score   = numeric(),
  mscore  = numeric(),
  sem     = numeric(),
  ci      = numeric(),
  opt     = integer(),
  `1se`   = integer()){

  methods::new(
    Class = "Tested",
    set     = set    ,
    measure = measure,
    score   = score  ,
    mscore  = mscore ,
    sem     = sem    ,
    ci      = ci,
    opt     = opt    ,
    `1se`   = `1se`
  )
}

#'Get the Summary of a Tested Object
#'
#@aliases summary_table,Tested-method summary_table.Tested
#'
#'@description
#'This function produces a summary of an object of class \linkS4class{Tested}.
#'
#'@param object object of class \linkS4class{Tested}
#'@param best character string, the model selection rule to consider (\code{"best"} or \code{"1se"})
#'
#'@return A \code{data.frame} with the following elements
#'\describe{
#'\item{\code{set}}{name of the set}
#'\item{\code{measure}}{name of the considered performance metric}
#'\item{\code{score}}{performance metrics}
#'\item{\code{mean_score}}{mean performance metric}
#'\item{\code{standard_error}}{standard error of the mean performance metric}
#'\item{\code{upper_ci}}{upper bound of confidence interval}
#'\item{\code{lower_ci}}{lower bound of confidence interval}
#'\item{\code{best_model}}{logical indicating which is the best model}
#'\item{\code{imodel}}{index of the model}
#'\item{\code{selection_rule}}{character string indicating the adopted model selection rule}
#'}
#'
#'@seealso
#'\code{\link{summary_table.Renoir}},
#'\code{\link{summary_table.Evaluated}},
#'\code{\link{summary_table.EvaluatedList}},
#'\code{\link{summary_table.TestedList}},
#'\code{\link{summary_table.Tuned}},
#'\code{\link{summary_table.Trained}}
#'
#'@keywords internal
#'
#'@author Alessandro Barberis
summary_table.Tested <- function(object, best = c("opt", "1se")){

  #--------------------------------------------------------------------------------------------#
  #match
  best = match.arg(best)

  #--------------------------------------------------------------------------------------------#
  ibest = switch(
    best,
    'opt' = get_opt(object),
    '1se' = get_1se(object)
  )

  nmodels = length(get_score(object))
  best_model = vector(mode = 'logical', length = nmodels)
  best_model[ibest] = TRUE
  #--------------------------------------------------------------------------------------------#
  #out
  out = data.frame(
    set               = get_set(object),
    measure           = get_measure(object),
    score             = get_score(object),
    mean_score        = get_mscore(object),
    standard_error    = get_sem(object),
    upper_ci          = get_ci(object)['up'],
    lower_ci          = get_ci(object)['low'],
    best_model        = best_model,
    imodel            = seq(nmodels),
    selection_rule    = best,
    stringsAsFactors = F,
    row.names = NULL
  )

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}


#'Get the Summary of a Tested Object
#'
#@describeIn summary_table Summary of an object of class \linkS4class{Tested}
#'
#@keywords internal
#@inheritParams summary_table.Tested
#@inherit summary_table.Tested return
#
#'@author Alessandro Barberis
methods::setMethod(
  f = "summary_table",
  signature = methods::signature(object = "Tested"),
  definition = summary_table.Tested
)
