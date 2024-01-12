#' @include classes_generics.R class_Filtered.R class_EvaluatedList.R class_MarkedList.R class_Learner.R class_Looper.R class_Evaluator.R class_Logger.R class_FilterList.R
NULL

#' Renoir Class
#'
#'@description
#'An S4 class representing the evaluation of a learning method by repeated sampling.
#'
#'@slot id a character string
#'@slot config a character string
#'@slot response a character string
#'@slot nout a one-length integer vector
#'@slot grid a integer vector
#'@slot k a one-length integer vector
#'@slot sampling a character string
#'@slot learning a character string
#'@slot screening a character string
#'@slot scoring a character string
#'@slot filter a \linkS4class{Filtered} object
#'@slot evaluation a \linkS4class{EvaluatedList} object
#'@slot stability a list object
#'@slot marks a \linkS4class{MarkedList} object
#'@slot nbest a data.frame object
#'@slot call list
#'
#'@seealso
#'\code{\link{Renoir}}
#'
#'@author Alessandro Barberis
methods::setClass(
  Class = "Renoir",
  slots = c(
    id           = "character",
    config       = "character",
    response     = "character",
    nout         = "integer",
    grid         = "integer",
    k            = "integer",
    sampling     = "character",
    learning     = "character",
    screening    = "character",
    scoring      = "character",
    filter       = "Filtered",
    evaluation   = "EvaluatedList",
    stability    = "list",
    marks        = "MarkedList",
    nbest        = "data.frame",
    call         = "list"
  )
)

#' Constructor for the S4 Renoir object.
#'
#' Constructor for the S4 \linkS4class{Renoir} object.
#'
#'@param id evaluated learning method
#'@param config name of the configuration
#'@param response response type
#'@param nout the number of responses
#'@param grid integer vector, the considered training set sizes
#'@param k integer, the number of sampling repeats
#'@param sampling string, sampling method used for evaluation
#'@param learning string, the learning procedure (i.e. training or tuning)
#'@param screening string, the feature screening strategy adopted
#'@param scoring string, the accuracy measure(s) used during the evaluation
#'@param filter a \linkS4class{Filtered} object containing the summary of the pre-processing
#'@param evaluation a \linkS4class{EvaluatedList} object containing the evaluation of the
#'learning method
#'@param stability list, containing the features stability for each response,
#'i.e. the frequency of features recruitment per response across all the computed models
#'@param marks a \linkS4class{MarkedList} object containing the features importance
#'@param nbest data.frame, it contains the indices of the automatically selected best training set size for
#'different settings (e.g. assessment on training, testing, full set of data)
#'@param call list, containing the arguments to the "renoir" function call that creates the \linkS4class{Renoir} object
#'
#'@return a \linkS4class{Renoir} object
#'
#'@author Alessandro Barberis
#'@export
#'
#'@rdname Renoir-class
Renoir <- function(
  id         = character(),
  config     = character(),
  response   = character(),
  nout       = integer(),
  grid       = integer(),
  k          = integer(),
  sampling   = character(),
  learning   = character(),
  screening  = character(),
  scoring    = character(),
  filter     = Filtered(),
  evaluation = EvaluatedList(),
  stability  = list(),
  marks      = MarkedList(),
  nbest      = data.frame(),
  call       = list()
){

  methods::new(
    Class = "Renoir",
    id         = id      ,
    config     = config  ,
    response   = response,
    nout       = nout,
    grid       = grid,
    k          = k,
    sampling   = sampling,
    learning   = learning,
    screening  = screening,
    scoring    = scoring,
    filter     = filter,
    evaluation = evaluation,
    stability  = stability,
    marks      = marks,
    nbest      = nbest,
    call       = call
  )
}


methods::setMethod(f = "get_id",         signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'id')})
methods::setMethod(f = "get_config",     signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'config')})
methods::setMethod(f = "get_response",   signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'response')})
methods::setMethod(f = "get_nout",       signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'nout')})
methods::setMethod(f = "get_grid",       signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'grid')})
methods::setMethod(f = "get_k",          signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'k')})
methods::setMethod(f = "get_sampling",   signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'sampling')})
methods::setMethod(f = "get_learning",   signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'learning')})
methods::setMethod(f = "get_screening",  signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'screening')})
methods::setMethod(f = "get_scoring",    signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'scoring')})
methods::setMethod(f = "get_filter",     signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'filter')})
methods::setMethod(f = "get_evaluation", signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'evaluation')})
methods::setMethod(f = "get_stability",  signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'stability')})
methods::setMethod(f = "get_marks",      signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'marks')})
methods::setMethod(f = "get_nbest",      signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'nbest')})
methods::setMethod(f = "get_call",       signature = "Renoir", definition = function(object){methods::slot(object = object, name = 'call')})

is.Renoir <- function(object){
  out = !is.na(match("Renoir", table = class(object)))
  return(out)
}

#'Evaluation of a learning method
#'
#'@description This function runs an evaluation of a learning method
#'applied over a predictors matrix \code{x} and a response variable
#'\code{y}.
#'See the **Details** section below for further information.
#'
#'
#'@param x the input matrix, where rows are observations and columns are variables.
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}.
#'@param weights (optional) vector of observation weights
#'@param offset vector containing an offset, used for linear models. Default is \code{NULL}.
#'@param resp.type the response type
#'@param hyperparameters named list containing the hyperparameters of the learning method to tune
#'@param ngrid (optional) grid of sample sizes to consider
#'@param nmin minimum sample size (i.e. number of samples to use for training)
#'@param npoints number of sample sizes to consider (i.e. number of training set sizes in the grid)
#'@param filter a \linkS4class{Filter} or \linkS4class{FilterList} object
#'@param looper a \linkS4class{Looper} object
#'@param learner a \linkS4class{Learner} object
#'@param evaluator a \linkS4class{Evaluator} object
#'@param logger a \linkS4class{Logger} object
#'@param filename (optional) name without extension for the output file
#'@param outdir path to the output directory
#'@param restore logical, whether to try to restore a previously ran analysis
#'if it stopped before the end (\code{outdir} should have been set in the previous analysis)
#'@param rm.call logical, whether to remove the call from the models. Helpful if object size is expected to be big
#'@param rm.fit logical, whether to remove the model fits used for tuning the hyperparameters . Helpful if object size is expected to be big
#'object can be huge.
#'@param grouping logical, whether to group results by learning method, configuration, response type, sampling method
#'@param ... further arguments to the evaluate function
#'
#'@return The output depends on the adopted learning strategy.
#'If the tuning was required then the output is a list containing
#'2 \linkS4class{Renoir} objects, one per assessed configuration.
#'If tuning was not required, the output is a \linkS4class{Renoir}
#'object.
#'
#'@details
#'This function uses the provided \linkS4class{Learner} to
#'train a model.
#'
#'The \linkS4class{Evaluator} in input contains
#'
#'
#'@author Alessandro Barberis
#'
#'@export
#'
#'@name renoir
#'@rdname renoir
renoir = function(
  #Pre-processing
  filter,

  #Training set size
  npoints = 3,
  ngrid,
  nmin = round(nrow(x)/2),

  #Loop
  looper = Looper(),

  #Store
  filename = "renoir",
  outdir   = NULL,
  restore  = TRUE,

  #Learn
  learner,

  #Evaluate
  evaluator,

  #Mark
  # marker,

  #Log
  logger,

  #Data for training
  hyperparameters,
  x,
  y,
  weights = NULL,
  offset  = NULL,
  resp.type,

  #space
  rm.call = FALSE,
  rm.fit  = FALSE,

  #Group results
  grouping = TRUE,

  #Further arguments for trainer
  ...){

  #--------------------------------------------------------------------------------------------#
  #Get and set logger
  if(missing(logger)){ logger = get_logger(evaluator) }
  logger = open_con(logger)

  #--------------------------------------------------------------------------------------------#
  hasOutdir = check_output_dir(outdir);

  #--------------------------------------------------------------------------------------------#
  #FILTER FEATURES
  if(!missing(filter)){
    log_info(object = logger, message = paste("Pre-processing"), sep = "\n", add.level = TRUE, add.time = TRUE)
    #filter
    filtered = filter(filter = filter, x = x, logger = logger)
    #check if data was filtered
    filtered.x = get_filtered(filtered)
    if(!is.na(filtered.x) && !S4Vectors::isEmpty(filtered.x)){
      #update
      x = filtered.x

    } else {
      # warning("Output of filter seems unusual. Please check your data.\n")
      stop("The pre-processed data seems unusual. Please check the provided 'x' and 'filter'.\n")
    }
  } else {
    filtered = Filtered()
  }

  #--------------------------------------------------------------------------------------------#
  #check
  # if(missing(ngrid)){
  #   #create grid
  #   log_info(object = logger, message = paste("Creating grid of sample sizes..."), sep = "", add.level = TRUE, add.time = TRUE)
  #   ngrid = get_grid(object = get_sampler(evaluator), nmin = nmin, npoints = npoints)
  #   log_info(object = logger, message = paste("DONE:", paste0(ngrid, collapse = ", ")), sep = "\n", add.level = F, add.time = F)
  # }

  #--------------------------------------------------------------------------------------------#
  #LEARNER
  log_info(object = logger, message = paste("Evaluation"), sep = "\n", add.level = TRUE, add.time = TRUE)
  evaluated = evaluate(
    learner         = learner,
    evaluator       = evaluator,
    npoints         = npoints,
    ngrid           = ngrid,
    nmin            = nmin,
    hyperparameters = hyperparameters,
    x               = x,
    y               = y,
    weights         = weights,
    offset          = offset,
    resp.type       = resp.type,
    logger          = logger,
    rm.call         = rm.call,
    rm.fit          = rm.fit,
    outdir          = outdir,
    filename        = filename,
    ...
  )

  # #--------------------------------------------------------------------------------------------#
  # #Features importance
  # log_info(object = logger, message = "Computing the features importance.", sep = "\n", add.level = TRUE, add.time = TRUE)
  # # feat.imp = get_features_importance(object = tat.list, recruitment.term = importance.term)
  # scored = mark(object = marker, learned = learned)
  # #unlist
  # scored = unlist(scored, recursive = T, use.names = F)
  # #create MarkedList
  # scored = MarkedList(scored)
  #
  # log_info(object = logger, message = "Computing the feature final marks.", sep = "\n", add.level = TRUE, add.time = TRUE)
  # marked = marked(object = scored, marking.system = get_marking_system(object = marker), logger = logger)
  # #unlist
  # marked = unlist(x = marked, recursive = T, use.names = F)
  # #create MarkedList
  # marked = MarkedList(marked)
  #
  #--------------------------------------------------------------------------------------------#
  #reshape
  evaluation = create_EvaluatedList(unlist(evaluated), grouping = grouping)

  #n of different output objects
  nout = length(evaluation)

  #--------------------------------------------------------------------------------------------#
  #stability
  log_debug(object = logger, message = "Computing features stability...", sep = "", add.level = TRUE, add.time = TRUE)
  stabilityl = list()
  for(i in seq(nout)){
    stabilityl[[i]] = stability(object = evaluation[[i]])
  }
  log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------------------------------------#
  #marks
  log_debug(object = logger, message = "Marking...", sep = "", add.level = TRUE, add.time = TRUE)
  marks = list()
  for(i in seq(nout)){
    # marks[[i]] = importance(object = evaluation[[i]], marker = get_marker(learner))
    marks[[i]] = importance(object = evaluation[[i]], marker = get_marker(learner), scorer = get_scorer(evaluator), features = colnames(x = x, do.NULL = F, prefix = "V"))#set features to consider all p in input
  }
  log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------------------------------------#
  #Best training set size
  log_debug(object = logger, message = "Selecting best size...", sep = "", add.level = TRUE, add.time = TRUE)
  nbest = list()
  for(i in seq(nout)){
    nbest[[i]] = which_best(object = evaluation[[i]], evaluator = evaluator)
  }
  log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------------------------------------#
  #Create output object, one for each evaluated group
  log_debug(object = logger, message = "Creating output object...", sep = "", add.level = TRUE, add.time = TRUE)
  out = list()

  for(i in seq(nout)){

    out[[i]] = Renoir(
      id         = get_id(learner),
      config     = get_config(evaluation[[i]][[1]]),
      response   = get_response(evaluation[[i]][[1]]),
      nout       = get_num_of_responses(y = y, resp.type = resp.type),
      grid       = get_n(evaluation[[i]]),
      k          = get_k(evaluation[[i]]),
      # sampling   = get_sampling(evaluation[[i]][[1]]),
      sampling   = get_name_sampling_strategy(object = get_sampler(object = evaluator), use.k = F),
      learning   = get_learning(evaluation[[i]]),
      screening  = get_screening(evaluation[[i]]),
      scoring    = get_id(get_scorer(object = evaluator)),
      # ngrid      = ngrid,
      filter     = filtered,
      evaluation = evaluation[[i]],
      stability  = stabilityl[[i]],
      marks      = marks[[i]],
      nbest      = nbest[[i]],
      call       = list()
    )
  }
  log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  if(nout==1){
    out = unlist(out)
  }

  # log_info(object = logger, message = "Creating output object...", sep = "", add.level = TRUE, add.time = TRUE)
  # out = Renoir(
  #   filter  = filtered,
  #   evaluation = evaluation,
  #   learned  = learned,
  #   scored   = scored,
  #   marked   = marked
  # )
  # log_info(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------------------------------------#
  #close
  close_con(logger)

  #--------------------------------------------------------------------------------------------#
  #out
  # return(evaluated)
  return(out)
}


#' Object Summaries
#'
#' @description Generic function used to produce summary tables of various
#' objects generated by `renoir`. The function invokes particular
#' methods which depend on the class of the first argument.
#'
#'@details Methods for different classes are implemented, e.g.
#'\linkS4class{Renoir}, \linkS4class{EvaluatedList}, \linkS4class{Evaluated},
#'\linkS4class{TestedList}, \linkS4class{Tested}.
#'
#' @param object an object for which a summary is desired
#' @param ... additional arguments affecting the summary produced
#'
#' @return The form of the returned object depends on the class of its
#' argument. See the documentation of the particular method for
#' further details.
#'
#' @name summary_table
#' @seealso
#'\code{\link{summary_table.Renoir}},
#'\code{\link{summary_table.EvaluatedList}},
#'\code{\link{summary_table.Evaluated}},
#'\code{\link{summary_table.TestedList}},
#'\code{\link{summary_table.Tested}},
#'\code{\link{summary_table.Tuned}},
#'\code{\link{summary_table.Trained}},
#'\code{\link{summary_table.Screened}}
#'
#'@export
NULL


#'Get Renoir Object Summary
#'
#'@description
#'This function produces a summary of an object of class \linkS4class{Renoir}.
#'
#'@param object an object of class \linkS4class{Renoir}
#'@param ... further arguments to \code{\link{summary_table.EvaluatedList}}
#'
#'@return A \code{data.frame}
#'
#'@seealso
#'\code{\link{summary_table.EvaluatedList}},
#'\code{\link{summary_table.Evaluated}},
#'\code{\link{summary_table.TestedList}},
#'\code{\link{summary_table.Tested}}
#'\code{\link{summary_table.Tuned}}
#'\code{\link{summary_table.Trained}}
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
summary_table.Renoir <- function(object, ...){

  #--------------------------------------------------------------------------------------------#
  #summary
  out = summary_table(object = get_evaluation(object), ...)

  #--------------------------------------------------------------------------------------------#
  #store colnames order
  cnames = colnames(out)

  #--------------------------------------------------------------------------------------------#
  #Add best training set size
  nbest = get_nbest(object)

  #--------------------------------------------------------------------------------------------#
  #bind
  out = merge(
    x = out,
    y = nbest,
    by = c('set', 'measure'),
    all.x = T,
    sort = F
  )

  #--------------------------------------------------------------------------------------------#
  #Set as logical
  # nbest = nbest$training_set_size == nbest$n
  out$best_resample = out$training_set_size == out$n
  out$n = NULL

  #--------------------------------------------------------------------------------------------#
  #column names order
  cnames = unique(c(cnames, colnames(out)))

  #update
  out = out[,cnames]

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}

#'Get Object Summary
#'
#@describeIn summary_table Summary of an object of class \linkS4class{Renoir}
#'
#'@inheritParams summary_table.Renoir
#'@inherit summary_table.Renoir return
#'
#'@author Alessandro Barberis
#'
#'@rdname summary_table
methods::setMethod(
  f = "summary_table",
  signature = methods::signature(object = "Renoir"),
  definition = summary_table.Renoir
)

#'Get Object Summary
#'@keywords internal
summary_table.RenoirList <- function(object, key, ...){
  #--------------------------------------------------------------------------------------------#
  #check all element in list are of class Renoir
  if(!all(unlist(sapply(X = object, FUN = is.Renoir)))){stop("'object' must contain elements of class 'Renoir'.\n")}

  #--------------------------------------------------------------------------------------------#
  #Check is same response
  response = sapply(X = object, FUN = get_response)
  if(length(unique(response))>1){stop("Different response types found: only methods in similar settings can be compared.\n")}

  #--------------------------------------------------------------------------------------------#
  #get key
  if(missing(key)){key = identify_primary_key_from_EvaluatedList(EvaluatedList(do.call(what = c, args = lapply(X = object, FUN = get_evaluation))))}

  #--------------------------------------------------------------------------------------------#
  #summary
  data = lapply(
    X = object,
    FUN = summary_table,
    key = key, ...)

  data = do.call(what = rbind, args = c(data, make.row.names = F, stringsAsFactors = F))

  #--------------------------------------------------------------------------------------------#
  return(data)
}


#'Plot method for `Renoir` object
#'
#'@description This function produces a performance evaluation plot
#'for a \linkS4class{Renoir} object.
#'
#'@details A plot showing the mean performance and the related
#'95\% confidence interval of a learning method
#'across different training-set sizes is produced.
#'
#'@param x an object of class \linkS4class{Renoir}
#'@param y unused parameter
#'@param ... optional graphical parameters to \code{\link{plot_evaluation}} (if
#'\code{interactive = FALSE}) or \code{\link{plotly_evaluation}} (if
#'\code{interactive = TRUE})
#'@param set the data set to consider (\code{train}, \code{test} or \code{full})
#'@param measure the performance metric to plot
#'@param interactive logical, whether to draw an interactive plot
#'@param key a character vector indicating how to group elements
#'
#'@return An object of class \code{ggplot} (if \code{interactive = FALSE})
#'or \code{plotly} (if \code{interactive = TRUE})
#'
#'@seealso
#'\code{\link{plot_evaluation}},
#'\code{\link{plot_single_evaluation}},
#'\code{\link{plot_multi_evaluation}},
#'\code{\link{plotly_evaluation}},
#'\code{\link{plotly_single_evaluation}},
#'\code{\link{plotly_multi_evaluation}}
#'
#'@author Alessandro Barberis
#'
#'@export
#'@export plot.Renoir
plot.Renoir <- function(
  x,
  y,
  ...,

  #Summary
  # key = c("id", "config", "response", "sampling"),
  key,

  #Set data to plot
  measure,
  set,

  #Plot type
  interactive = FALSE

){

  #--------------------------------------------------------------------------------------------#
  #summary
  data = summary_table(object = x, key = key)

  #--------------------------------------------------------------------------------------------#
  #subset
  data = data[data$set == set & data$measure == measure,,drop=F]

  data$key = data$name

  #--------------------------------------------------------------------------------------------#
  #reorder data frame
  data = data[order(data$training_set_size, decreasing = F),,drop=F]

  #--------------------------------------------------------------------------------------------#
  #Get name
  ylab = get_measure_names(type.measure = measure)

  #--------------------------------------------------------------------------------------------#
  #Check thr
  if(identical(measure, "class")){
    #Get the number of classes
    nc = get_nout(x)
    #Compute the threshold
    thr = (nc - 1)/nc;
  } else if(identical(measure, "auc")){
    thr = 0.5
  } else {
    thr = NULL
  }

  #--------------------------------------------------------------------------------------------#
  #Plot
  if(!interactive){
    out = plot_evaluation(data = data, thr = thr, ylab = ylab, ...)
  } else {
    out = plotly_evaluation(data = data, thr = thr, ylab = ylab, ...)
  }

  #--------------------------------------------------------------------------------------------#
  return(out)
}


#'Plot method for `RenoirList` object
#'
#'@inheritParams plot.Renoir
#'
#'@inherit plot.Renoir return seealso
#'
#'@export plot.RenoirList
#'@export
plot.RenoirList <- function(
  x,
  y,

  #Summary
  # key = c("id", "config", "response", "sampling"),
  key,

  #Set data to plot
  measure,
  set,

  #Plot type
  interactive = FALSE,

  ...
){


  #--------------------------------------------------------------------------------------------#
  #check all element in list are of class Renoir
  if(!all(unlist(sapply(X = x, FUN = is.Renoir)))){stop("'x' must contain elements of class 'Renoir'.\n")}

  #--------------------------------------------------------------------------------------------#
  #Check is same response
  response = sapply(X = x, FUN = get_response)
  if(length(unique(response))>1){stop("Different response types found: only methods in similar settings can be compared.\n")}

  #--------------------------------------------------------------------------------------------#
  #get key
  if(missing(key)){key = identify_primary_key_from_EvaluatedList(EvaluatedList(do.call(what = c, args = lapply(X = x, FUN = get_evaluation))))}

  #--------------------------------------------------------------------------------------------#
  #summary
  data = lapply(
    X = x,
    FUN = summary_table,
    key = key)

  data = do.call(what = rbind, args = c(data, make.row.names = F, stringsAsFactors = F))

  #--------------------------------------------------------------------------------------------#
  #subset
  data = data[data$set == set & data$measure == measure,,drop=F]

  # data$key = data$name

  #--------------------------------------------------------------------------------------------#
  #Get name
  ylab = get_measure_names(type.measure = measure)

  #--------------------------------------------------------------------------------------------#
  #Check thr
  if(identical(measure, "class")){
    #Get the number of classes
    nc = get_nout(x[[1]])
    #Compute the threshold
    thr = (nc - 1)/nc;
  } else if(identical(measure, "auc")){
    thr = 0.5
  } else {
    thr = NULL
  }

  #--------------------------------------------------------------------------------------------#
  #Plot
  if(!interactive){
    out = plot_evaluation(data = data, thr = thr, ylab = ylab, ...)
  } else {
    out = plotly_evaluation(data = data, thr = thr, ylab = ylab, ...)
  }

  #--------------------------------------------------------------------------------------------#
  return(out)
}


#'Plot method for `RenoirSummaryTable` object
#'
#'@inheritParams plot.Renoir
#'
#'@inherit plot.Renoir return seealso
#'
#'@export plot.RenoirSummaryTable
#'@export
plot.RenoirSummaryTable <- function(
  x,
  y,

  #Summary
  key,

  #Set data to plot
  measure,
  set,

  #Plot type
  interactive = FALSE,

  ...
){
  #--------------------------------------------------------------------------------------------#
  #subset
  data = x[x$set == set & x$measure == measure,,drop=F]

  #--------------------------------------------------------------------------------------------#
  #Get name
  ylab = get_measure_names(type.measure = measure)

  #--------------------------------------------------------------------------------------------#
  #Check thr
  if(identical(measure, "class")){
    #Get the number of classes
    nc = get_nout(x[[1]])
    #Compute the threshold
    thr = (nc - 1)/nc;
  } else if(identical(measure, "auc")){
    thr = 0.5
  } else {
    thr = NULL
  }

  #--------------------------------------------------------------------------------------------#
  #Plot
  if(!interactive){
    out = plot_evaluation(data = data, thr = thr, ylab = ylab, ...)
  } else {
    out = plotly_evaluation(data = data, thr = thr, ylab = ylab, ...)
  }

  #--------------------------------------------------------------------------------------------#
  return(out)
}

#' @rdname  plot-methods
#' @aliases plot
#'
#' @title Plot method for `Renoir` object
#'
#'@inherit plot.Renoir description details return
#'
#'@inheritParams plot.Renoir
#'
#'@seealso
#'\code{\link{plot_evaluation}},
#'\code{\link{plot_single_evaluation}},
#'\code{\link{plot_multi_evaluation}},
#'\code{\link{plotly_evaluation}},
#'\code{\link{plotly_single_evaluation}},
#'\code{\link{plotly_multi_evaluation}}
#'
#'@export
#'
#'@author Alessandro Barberis
methods::setMethod(
  f = "plot",
  signature = methods::signature(x = "Renoir", y="missing"),
  definition = plot.Renoir
  # definition = function(x,y,...){plot.Renoir(x=x,...)}
)

# @rdname  plot-methods
# @aliases plot
# @export
# methods::setMethod(
#   f = "plot",
#   signature = methods::signature(x = "RenoirList", y="missing"),
#   definition = plot.RenoirList
# )

#'Create an interactive report
#'
#'@description Create an interactive report for the computed analysis.
#'
#'@param object an object of class \linkS4class{Renoir}
#'@param report.type the type of report to create. If \code{short}, the feature importance tables
#'are not created (this could solve the pandoc "out of memory" issue when the number of considered features is high)
#'@inheritParams renoir
#'@param annotation a data frame containing features annotation
#'@param feat.index numeric, the index of the column in \code{annotation} matching \code{x} feature names.
#'\code{0} means rownames
#'@param ... further arguments to \code{\link[rmarkdown]{render}} \code{params} argument. Possible options are:
#'\describe{
#'   \item{\code{yml_author}}{A character vector, name of the author(s)}
#'   \item{\code{yml_affiliation}}{The author’s affiliation; must match length of name,
#'   e.g. if name has length of two, affiliation must as well; use NA if you don’t want
#'   to include an affiliation for a given author.Note that not all formats support the affiliation field.}
#'   \item{\code{yml_email}}{The author email address. Note that not all formats support the email field.}
#'   \item{\code{yml_title}}{A character vector, the title of the document}
#'   \item{\code{yml_subtitle}}{A character vector, the subtitle of the document.
#'   Not all R Markdown formats use subtitles, so it may depend on what you use in the output field.
#'   It is available in \code{pdf_document}, \code{html_document}, and \code{word_document} by default}
#'   \item{\code{yml_abstract}}{A character vector, the abstract. Long character vectors
#'   are automatically wrapped using valid YAML syntax.
#'   This field is not available in all output formats; it is available in \code{pdf_document} and \code{html_document} by default.}
#'   \item{\code{yml_name}}{A character vector, the name of the document}
#   \item{\code{yml_toc}}{logical, whether to use a Table Of Contents}
#   \item{\code{yml_toc_depth}}{integer, the depth of headers to use in the TOC}
#   \item{\code{yml_toc_float}}{logical, to float the table of contents to the left of the main document}
#'   \item{\code{tabset}}{logical, whether to use tabbed sections (if any)}
#'}
#'@return The compiled report is written into the output file.
#'
#'@author Alessandro Barberis
#'
#'@export
methods::setMethod(
  f = "create_report",
  signature = methods::signature(object = "Renoir"),
  definition = function(
    object,

    #General Options
    output_format = "html_document",
    outdir = tempdir(),
    filename = NULL,
    report.type = c("full", "short"),
    #Params
    annotation = NULL,
    feat.index = 0,
    ...){

    # needed_args = c("x", "y", "object", "resp.type")
    # needed_args = c("object")
    # provided_args = names(as.list(match.call())[-1])
    provided_args = names(as.list(match.call()))
    # if(any(!(needed_args %in% provided_args))){stop(paste("Missing values:",setdiff(x = needed_args, y = provided_args),"must be provided."))}

    report.type = match.arg(report.type)

    #check if train.size and model.index are provided
    needed_args = c("train.size", "model.index")
    if(all((needed_args %in% provided_args))){
      # filename_report_rmd = if(identical(report.type, "full")){paste0('report_renoir_model.Rmd')}else{paste0('report_renoir_model_short.Rmd')}
      filename_report_rmd = 'report_renoir_model.Rmd'
    } else {
      # filename_report_rmd = paste0('report_',name, '.Rmd')
      filename_report_rmd = if(identical(report.type, "full")){paste0('report_renoir.Rmd')}else{paste0('report_renoir_short.Rmd')}

    }


    #get the path to the r markdown
    filepath = system.file("rmd", filename_report_rmd, package = get_name_package());

    #Render the rmarkdown and generate the report
    rmarkdown::render(input = filepath,
                      output_format = output_format,
                      output_file = filename,
                      output_dir = outdir,
                      clean = TRUE,
                      params = c(list(object = object,
                                      annotation = annotation,
                                      feat.index = feat.index),
                                 list(...)))
    }
)


#'Create an interactive report
#'
#'@description Create an interactive report for the computed analysis.
#'
#'@param object an object of class \linkS4class{Trained}
#'@inheritParams create_report,Renoir-method
#'
#'@return The compiled report is written into the output file.
#'
#'@author Alessandro Barberis
#'
#'@export
methods::setMethod(
  f = "create_report",
  signature = methods::signature(object = "Trained"),
  definition = function(
    #General Options
    output_format = "html_document",
    outdir = tempdir(),
    filename = NULL,
    report.type = c("full", "short"),
    #Params
    object,
    x, y, weights = NULL, offset = NULL,
    annotation = NULL,
    feat.index = 0,
    ...){

    # needed_args = c("x", "y", "object", "resp.type")
    needed_args = c("object","x", "y", "weights", "offset")
    provided_args = names(as.list(match.call())[-1])

    report.type = match.arg(report.type)

    if(any(!(needed_args %in% provided_args))){stop(paste("Missing values:",setdiff(x = needed_args, y = provided_args),"must be provided."))}


    # filename_report_rmd = paste0('report_',name, '.Rmd')
    filename_report_rmd = if(identical(report.type, "full")){paste0('report_model.Rmd')}else{paste0('report_model_short.Rmd')}

    #get the path to the r markdown
    filepath = system.file("rmd", filename_report_rmd, package = get_name_package());

    #Render the rmarkdown and generate the report
    rmarkdown::render(input = filepath,
                      output_format = output_format,
                      output_file = filename,
                      output_dir = outdir,
                      clean = TRUE,
                      params = c(list(object = object,
                                      annotation = annotation,
                                      feat.index = feat.index),
                                 list(...)))
  }
)




print_density <- function(object, y, xaxis, header = '##'){

  #get call
  call = get_call(object)

  #Check y
  if(missing(y)){y = call$y}

  if(!is.null(y)){
    #Get filtered
    filtered = get_filter(object = object)
    #Get data
    data = get_filtered(object = filtered);

    #create groups
    groups.list = list()
    if(is.factor(y)){
      lvls = levels(y);

      for(i in seq(length(lvls))){
        group.name = lvls[i]

        groups.list[[group.name]] = data[which(y == group.name),,drop=F]
      }

    } else {
      groups.list = NULL
    }

    #Print
    cat(paste(header, 'Density plots \n\n', '\n' ))

    print(
      htmltools::tagList(
        plotly.density(data = data, groups = groups.list, xaxis = xaxis, title.plot = "Density curve (after pre-processing)")
      )
    )

    cat('\n\n\n')
    #--------------------------------------------------------------------------------------------#
    if(!is.null(call)){
      if(!is.null(call$x) && is.matrix(call$x) && ncol(call$x)!=ncol(data)){
        data = call$x;
        y    = call$y

        #create groups
        groups.list = list()
        if(is.factor(y)){
          lvls = levels(y);

          for(i in seq(length(lvls))){
            group.name = lvls[i]

            groups.list[[group.name]] = data[which(y == group.name),,drop=F]
          }

        } else {
          groups.list = NULL
        }

        print(
          htmltools::tagList(
            plotly.density(data = data, groups = groups.list, xaxis = xaxis, title.plot = "Density curve (before pre-processing)")
          )
        )

        cat('\n\n\n')
      }
    }
  }
}



#'Get Trained/Tuned Model
#'
#'@description This function extracts a model from a
#' \linkS4class{Renoir} object.
#'
#'@param object an object of class \linkS4class{Renoir}
#'@param n sample size (i.e. training set size) to select
#'@param index index of model to select
#'
#'@return a \linkS4class{Trained} or \linkS4class{Tuned} object
#'
#'@author Alessandro Barberis
#'
#'@export
#'
#'@rdname get_model
methods::setMethod(
  f = "get_model",
  signature = "Renoir",
  definition = function(object, n, index){
    #get evaluated list
    object = get_evaluation(object = object)
    #get model
    object = get_model(object = object, n = n, index = index)
    #return
    return(object)
  }
)

methods::setMethod(
  f = "get_sample",
  signature = "Renoir",
  definition = function(object, index, n){
    #get evaluated list
    object = get_evaluation(object = object)
    #subset
    object = get_sample(object = object, n = n, index = index)
    #return
    return(object)
  }
)

#'Extract a signature
#'
#'@description Features with non-zero coefficients (where applicable) from
#'selected model are considered as signature
#'@param grouped logical, whether to merge results from a multi-response
#'to obtain a single set of features
#'
#'@rdname signature
#'
#'@export
methods::setMethod(
  f = "signature",
  signature = methods::signature(object = "Renoir", index = "numeric", n = "numeric", cutoff = "missing", measure = "missing", set = "missing"),
  definition = function(object, index, n, cutoff, measure, set, grouped = F){
    #get evaluated list
    object = get_evaluation(object = object)
    #subset
    object = get_model(object = object, n = n, index = index)
    #features
    out = features(object = object, type = "nonzero")
    #check
    if(grouped){out = unique(unlist(out))}
    #return
    return(out)
  }
)


#'@rdname signature
#'
#'@export
methods::setMethod(
  f = "signature",
  signature = methods::signature(object = "Renoir", index = "missing", n = "missing", cutoff = "numeric", measure = "character", set = "character"),
  definition = function(object, index, n, cutoff, measure, set = c("train", "test", "full"), ...){
    #get signature
    out = signature(object = object, measure = measure, set = set, ...)
    #subset
    object = subset_list(object = object, set = set, measure = measure)
    #check
    if(length(object)>1){use.rankstat = T}else{use.rankstat = F}

    if(use.rankstat){
      #cutoff on pfp
      object = lapply(X = object, FUN = function(x, cutoff){
        #keep
        keep = x$pfp_pos < cutoff | x$pfp_neg < cutoff
        #update
        x = x[keep,,drop=F]
        #return
        return(x)
      }, cutoff = cutoff)
    } else {
      #cutoff on score
      object = lapply(X = object, FUN = function(x, cutoff){
        #keep
        keep = abs(x[,1]) > cutoff
        #update
        x = x[keep,,drop=F]
        #return
        return(x)
      }, cutoff = cutoff)
    }

    #return
    return(object)
  }
)


#'@rdname signature
#'
#'@export
methods::setMethod(
  f = "signature",
  signature = methods::signature(object = "Renoir", index = "missing", n = "missing", cutoff = "missing", measure = "character", set = "character"),
  definition = function(object, index, n, cutoff, measure, set = c("train", "test", "full"), ...){
    set = match.arg(set)
    #get marked list
    object = get_marks(object = object)
    #subset
    object = subset_list(object = object, set = set, measure = measure)
    #check
    if(length(object)>1){use.rankstat = T}else{use.rankstat = F}
    #get marks
    object = get_mark(object = object)
    if(isTRUE(!is.data.frame(object) && is.list(object))){
      #get names
      feats = rownames(object[[1]])
      #reshape
      object = do.call(what = Map, args = c(f = cbind, object))
      #set names
      object = lapply(X = object, FUN = function(x, feats){row.names(x)=feats; x}, feats = feats)
    }

    if(use.rankstat){
      #compute
      object = lapply(X = object, FUN = function(fi.df, ...){
        #set as matrix
        fi.matrix = as.matrix(fi.df)

        #get RP
        out = compute_rank_product(data = fi.matrix, ...)
      }, ... = ...)

    }

    #return
    return(object)
  }
)


#'@rdname signature
#'
#'@export
methods::setMethod(
  f = "signature",
  signature = methods::signature(object = "Renoir", index = "missing", n = "missing", cutoff = "numeric", measure = "missing", set = "missing"),
  definition = function(object, index, n, cutoff, measure, set = c("train", "test", "full"), ...){
    #get signature
    out = signature(object = object, ...)
    #cutoff on pfp
    out = lapply(X = out, FUN = function(x, cutoff){
      #keep
      keep = x[,'pfp_pos'] < cutoff | x[,'pfp_neg'] < cutoff
      #update
      x = x[keep,,drop=F]
      #return
      return(x)
    }, cutoff = cutoff)
    #return
    return(out)
  }
)

#Measures are merged via RankProd

#'@rdname signature
#'
#'@export
methods::setMethod(
  f = "signature",
  signature = methods::signature(object = "Renoir", index = "missing", n = "missing", cutoff = "missing", measure = "missing", set = "missing"),
  definition = function(object, index, n, cutoff, measure, set = c("train", "test", "full")){
    #get marked list
    object = get_marks(object = object)
    #get marks
    object = get_mark(object = object)
    #get names
    feats = rownames(object[[1]])
    #reshape
    object = do.call(what = Map, args = c(f = cbind, object))
    #set names
    object = lapply(X = object, FUN = function(x, feats){row.names(x)=feats; x}, feats = feats)
    #compute
    object = lapply(X = object, FUN = function(fi.df, ...){
      #set as matrix
      fi.matrix = as.matrix(fi.df)

      #get RP
      out = compute_rank_product(data = fi.matrix, ...)
    }, ... = ...)

    #return
    return(object)
  }
)

#If model was tuned, then the features of the model
#are filtered for the stability
#'@rdname signature
#'
#'@export
methods::setMethod(
  f = "signature",
  signature = methods::signature(object = "Renoir", index = "numeric", n = "numeric", cutoff = "numeric", measure = "missing", set = "missing"),
  definition = function(object, index, n, cutoff, grouped = F){
    #get evaluated list
    object = get_evaluation(object = object)
    #subset
    object = get_model(object = object, n = n, index = index)
    #features
    out = features(object = object, type = "nonzero")

    #check
    if(grouped){out = unique(unlist(out))}
    #return
    return(object)
  }
)


#'Compute Rank Product/Rank Sum
#'@param data a matrix vars x obs
#'@inheritParams RankProd::RankProducts
#'@param ... further arguments to \code{\link[RankProd]{RankProducts}}
#'@return a dataframe containing different elements
#'\describe{
#'   \item{code{pfp_}}{estimated percentage of false positive predictions for each feature
#'   positively and negatively correlated with the outcome}
#'   \item{code{p_val}}{estimated p-value for each feature
#'   positively and negatively correlated with the outcome}
#'   \item{code{Rank_Stat_}}{the rank-product/rank-sum statistics evaluated per each gene
#'   positively and negatively correlated with the outcome}
#'
#'}
#'@keywords internal
compute_rank_product <- function(data,
                                 cl = rep(1, times = ncol(data)),
                                 logged = T,
                                 gene.names = rownames(data),
                                 ...){

  #compute the rank product
  rank.prod <- RankProd::RankProducts(data = data,
                                      cl = cl,
                                      # origin = rep(1, length(colnames(data))),
                                      logged = logged,
                                      gene.names = gene.names,
                                      ...)

  #create an output table
  out <- cbind(rank.prod$pfp,
               rank.prod$pval,
               rank.prod$RPs)

  #setup column names
  colnames(out) <- c(paste0("pfp_",       colnames(rank.prod$pfp)),
                     paste0("p_val_",     colnames(rank.prod$pval)),
                     paste0("Rank_Stat_", colnames(rank.prod$RPs)))

  #update names
  colnames(out) = gsub(pattern = "class1 < class2", replacement = "neg", x = colnames(out), fixed = F)
  colnames(out) = gsub(pattern = "class1 > class2", replacement = "pos", x = colnames(out), fixed = F)

  #return
  return(out)

}
