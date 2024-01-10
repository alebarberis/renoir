#' @include classes_generics.R class_Logger.R class_Trainer.R class_Screener.R class_ScreenerList.R class_Resampler.R class_Tester.R glmnet.R class_Selector.R class_Scorer.R utils.R
NULL

#' Tuner Class
#'
#' @description
#' An S4 class providing the methods to perform the tuning of
#' hyperparameters and the training of the final model.
#'
#' The object consists of 6 slots
#' @slot id the name of the tuning method to use
#' @slot tuner function to use for tuning the hyperparameters of the model
#' @slot parameters list containing the parameters to fix for the chosen tuning method
#' @slot logger a \linkS4class{Logger}
#' @slot sampler a \linkS4class{Sampler} object, the sampling procedure to use for tuning
#' @slot looper a \linkS4class{Looper} object
#' @slot screener a \linkS4class{ScreenerList} object
# @slot scorer   a \linkS4class{Scorer} object
# @slot trainer a \linkS4class{Trainer}
# @slot tester a \linkS4class{Tester}
# @slot selector a \linkS4class{Selector}
# @slot hyperparameters list containing the hyperparameters to be tuned for the chosen learning method.
# A \code{screening} element having an integer vector as value will be used as hyperparameters for the screener.
#' @author Alessandro Barberis
methods::setClass(
  Class = "Tuner",
  slots = c(
    id              = "character",
    tuner           = "function",
    sampler         = "Sampler",
    looper          = "Looper",
    screener        = "ScreenerList",
    # scorer          = "Scorer",
    # trainer         = "Trainer",
    # tester          = "Tester",
    # selector        = "Selector",
    # hyperparameters = "list",
    parameters      = "list",
    logger          = "Logger"
  )
)

#' Constructor for the S4 Tuner object.
#'
#' @description
#' Constructor for the S4 \linkS4class{Tuner} object.
#'
# @param learning.method the learning method associated to this \linkS4class{Forecaster}.
# If learning method is one of the supported by renoir, the constructor will
# automatically select \code{prediction} and \code{selector} if they are missing.
#' @param id object identifier, the name of the tuning method to use.
#' If \code{id} is one of the supported by renoir, the constructor will
#' automatically select a built-in \code{tuner} function.
#' @param tuner (optional) function for tuning the hyperparameters of the model
#' If tuning method is one of the supported by renoir, the constructor will
#' automatically select a built-in \code{tuner} function.
#' If \code{tuner} is provided it must conform to the renoir common interface,
#' and must have the following formal arguments:
#' \describe{
   # \item{tuner}{a \linkS4class{Tuner} object, used to pass all the needed elements (i.e. sampler, looper, trainer, ...)}
#'    \item{sampler}{a \linkS4class{Sampler} object, the sampling procedure to use for tuning}
#'    \item{looper}{a \linkS4class{Looper} object}
#'    \item{screener}{a \linkS4class{ScreenerList} object, can contain just one \linkS4class{Screener}}
#'    \item{trainer}{a \linkS4class{Trainer} object}
#'    \item{tester}{a \linkS4class{Tester} object}
#'    \item{selector}{a \linkS4class{Selector} object}
#'    \item{logger}{a \linkS4class{Logger} object}
#'    \item{hyperparameters}{the input matrix, where rows are observations and columns are variables}
#'    \item{x}{the input matrix, where rows are observations and columns are variables}
#'    \item{y}{the response variable. Its number of rows must match the number of rows of \code{x}}
#'    \item{weights}{priors of the observations}
#'    \item{offset}{a priori known component to be included in the linear predictor during fitting}
#'    \item{resp.type}{the response type}
#'    \item{...}{additional arguments}
#' }
#' It must return a \linkS4class{Tuned} or \linkS4class{TunedList} object
#' @param sampler a \linkS4class{Sampler} object, the sampling procedure to use for tuning
#' @param looper a \linkS4class{Looper} object
#' @param screener a \linkS4class{Screener} object
# @param trainer a \linkS4class{Trainer} object
# @param tester a \linkS4class{Tester} object
# @param selector a \linkS4class{Selector} object
# @param scorer a \linkS4class{Scorer} object
#' @param logger a \linkS4class{Logger} object
# @param hyperparameters list containing the hyperparameters to be tuned
#' @param parameters list containing the parameters to fix for the chosen tuning method.
#' Please, note that the parameters shouldn't include the default renoir formals as they won't be considered.
#' (It is helpful to change default settings)
#'
#' @return An object of class \linkS4class{Tuner}.
#'
#' @author Alessandro Barberis
#' @export
#'
#'@rdname Tuner-class
Tuner <- function(
  id           = "grid.search",
  tuner,
  sampler = Sampler(
    method  = "cv",
    k       = 10L
  ),
  looper          = Looper(),
  screener        = ScreenerList(Screener(id = "ebayes")),
  # scorer          = Scorer(id = "mse"),
  # trainer         = Trainer(),
  # tester          = Tester(),
  # selector        = Selector(id = get_id(trainer)),
  # selector,
  # hyperparameters,
  logger          = Logger(),
  parameters
){

  if(id %in% supported_tuning_methods()){
    tuner = get_tuner_function(id)
  }

  #Check provided tuner function
  check_provided_tuner_function(tuner)


  # hyperparameters = check_tuning_hyperparameters(hyperparameters = hyperparameters, trainer = trainer)

  # check_type_measure(type.measure = get_id(get_scorer(tester)), resp.type = get_resp_type(object = trainer))

  # if(missing(selector)){
  #   selector = get_model_selector_function(learning.method = get_id(trainer))
  # }

  #Check parameters
  if(missing(parameters) || is.null(parameters) || length(parameters)==0){
    parameters = list()
  }

  methods::new(
    Class = "Tuner",
    id              = id,
    tuner           = tuner,
    sampler         = sampler,
    looper          = looper,
    screener        = screener,
    # trainer         = trainer,
    # tester          = tester,
    # selector        = selector,
    # scorer          = scorer,
    logger          = logger,
    # hyperparameters = hyperparameters,
    parameters      = parameters
  )
}

methods::setMethod(f = "get_id",              signature = "Tuner", definition = function(object){methods::slot(object = object, name = 'id')})
methods::setMethod(f = "get_tuner",           signature = "Tuner", definition = function(object){methods::slot(object = object, name = 'tuner')})
methods::setMethod(f = "get_sampler",         signature = "Tuner", definition = function(object){methods::slot(object = object, name = 'sampler')})
methods::setMethod(f = "get_looper",          signature = "Tuner", definition = function(object){methods::slot(object = object, name = 'looper')})
methods::setMethod(f = "get_screener",        signature = "Tuner", definition = function(object){methods::slot(object = object, name = 'screener')})
# methods::setMethod(f = "get_scorer",          signature = "Tuner", definition = function(object){methods::slot(object = object, name = 'scorer')})
methods::setMethod(f = "get_logger",          signature = "Tuner", definition = function(object){methods::slot(object = object, name = 'logger')})
methods::setMethod(f = "get_parameters", signature = "Tuner", definition = function(object){methods::slot(object = object, name = 'parameters')})

# methods::setMethod(f = "get_trainer",         signature = "Tuner", definition = function(object){methods::slot(object = object, name = 'trainer')})
# methods::setMethod(f = "get_tester",          signature = "Tuner", definition = function(object){methods::slot(object = object, name = 'tester')})
# methods::setMethod(f = "get_selector",        signature = "Tuner", definition = function(object){methods::slot(object = object, name = 'selector')})

# methods::setMethod(f = "get_hyperparameters", signature = "Tuner", definition = function(object){methods::slot(object = object, name = 'hyperparameters')})
# # methods::setMethod(f = "get_scorer",            signature = "Tuner", definition = function(object){methods::slot(object = object, name = 'scorer')})
# # methods::setMethod(f = "get_measure",           signature = "Tuner", definition = function(object){scorer = get_scorer(object); get_measure(scorer)})
# methods::setMethod(f = "get_measure",           signature = "Tuner", definition = function(object){scorer = get_scorer(get_tester(object)); get_id(scorer)})


# methods::setMethod(f = "set_resampler", signature = "Tuner", definition = function(object, value){methods::slot(object = object, name = 'resampler') = value; return(object)})
# methods::setMethod(f = "set_trainer",   signature = "Tuner", definition = function(object, value){methods::slot(object = object, name = 'trainer') = value; return(object)})
methods::setMethod(f = "set_screener",  signature = "Tuner", definition = function(object, value){methods::slot(object = object, name = 'screener') = value; return(object)})


methods::setMethod(
  f = "tune",
  signature = "Tuner",
  # definition = function(tuner, trainer, tester, selector, hyperparameters, x, y, weights, offset, resp.type, ...){
  # definition = function(tuner, sampler, looper, screener, trainer, forecaster, scorer, selector, hyperparameters, logger, x, y, weights, offset, resp.type, ...){
  definition = function(tuner, sampler, looper, screener, trainer, tester, selector, hyperparameters, logger, x, y, weights, offset, resp.type, ...){

    #--------------------------------------------------------------------------------------------#
    #Check input
    if(missing(sampler)){sampler   = get_sampler(tuner)}
    if(missing(looper)){looper     = get_looper(tuner)}
    if(missing(screener)){screener = get_screener(tuner)}
    # if(missing(scorer)){scorer     = get_scorer(tuner)}
    if(missing(logger)){logger     = get_logger(tuner)}

    #--------------------------------------------------------------------------------------------#
    #get parameters
    parameters = get_parameters(tuner)

    #rm
    # parameters[c("x", "y", "weights", "offset", "resp.type")] = NULL
    # parameters[c("x", "y", "weights", "offset", "resp.type", "sampler", "looper", "screener", "logger", "scorer", "trainer", "forecaster", "selector", "hyperparameters")] = NULL
    parameters[c("x", "y", "weights", "offset", "resp.type", "sampler", "looper", "screener", "logger", "trainer", "tester", "selector", "hyperparameters")] = NULL

    #check
    parameters = check_parameters(parameters = parameters, ...)

    #--------------------------------------------------------------------------------------------#
    #merge
    args = c(parameters, list(...))

    #--------------------------------------------------------------------------------------------#
    #tuner function
    tuner_fun = get_tuner(object = tuner);

    #--------------------------------------------------------------------------------------------#
    #tune
    # tuned = do.call(what = tuner_fun, args = c(list(tuner = tuner, x = x, y = y, weights = weights, offset = offset, resp.type = resp.type), args))
    tuned = do.call(
      what = tuner_fun,
      args = c(list(
        # tuner = tuner,
        sampler = sampler, looper = looper, screener = screener,
        trainer = trainer, tester = tester, selector = selector, hyperparameters = hyperparameters,
        logger = logger,
        x = x, y = y, weights = weights, offset = offset, resp.type = resp.type), args))

    #--------------------------------------------------------------------------------------------#
    #return
    return(tuned)

  }
)

supported_tuning_methods <- function(){
  out = c("grid.search")
  return(out)
}

get_tuner_function <- function(id){
  out = switch(
    id,
    grid.search = tune_via_grid_search
  )

  return(out)
}

check_provided_tuner_function <- function(f){

  if(missing(f)){
    stop("'tuner' is missing with no default.\n")
  } else {
    #needed formals
    # formals.def = c("tuner", "x", "y", "weights", "offset", "resp.type")
    # formals.def = c("tuner", "trainer", "tester", "selector", "hyperparameters", "x", "y", "weights", "offset", "resp.type")
    formals.def = c("sampler", "looper", "screener", "trainer", "tester", "selector", "logger", "hyperparameters", "x", "y", "weights", "offset", "resp.type")

    #get formals
    formals.fun = names(formals(fun = f))

    #check
    # if(any(!(formals.fun %in% formals.def))){
    if(any(!(formals.def %in% formals.fun))){

      stop(paste0("Provided tuner function without required formal arguments. Function interface must match renoir requirements. Try with:\n
      function(", paste0(formals.def, collapse = ", "),", ...){
        #Extract objects from tuner
        #sampler = get_sampler(tuner)
        #YOUR CODE

        #Features screening
        #YOUR CODE

        #Training
        #YOUR CODE

        #Testing
        #YOUR CODE

        #Selecting optimal configuration
        #YOUR CODE

        #Training with optimal configuration on full data
        #YOUR CODE

        #Set output as Tuned object
        #out = Tuned(YOUR CODE)

        #Return output
        return(out)
      }
      \n"))
    }
  }
}


#' Tune via grid search
#'
#' @description This function tunes the hyperparameters of a learning method via
#' a grid-search strategy.
#'
#' @param sampler a \linkS4class{Sampler} object, the sampling procedure to use for tuning
#' @param looper a \linkS4class{Looper} object
#' @param screener a \linkS4class{Screener} object
#' @param trainer a \linkS4class{Trainer} object
#' @param tester a \linkS4class{Tester} object
#' @param selector a \linkS4class{Selector} object
#' @param logger a \linkS4class{Logger} object
#' @param hyperparameters list containing the hyperparameters to be tuned
#' @param x the input matrix, where rows are observations and columns are variables.
#' @param y the response variable. Its number of rows must match the number of rows of \code{x}.
#' @param weights priors of the observations
#' @param offset used only for GLM methods, it is an a priori known component to be included in the linear predictor during fitting
#' @param resp.type the response type
#' @param screening (optional) integer, if provided a features screening is performed before fitting.
#' If an integer vector is provided then the number of features to select before fitting is
#' considered an hyperparameter for the screener and will be tuned
#' @param keep.fit logical, whether to keep the trained objects returned by each resampling fit.
#' @param rule character string, the model selection rule to apply
#' @param observations (optional) integer vector, providing the indices of observations to keep.
#' If missing the entire set of observations is considered.
#' @param marker a \linkS4class{Marker} object
#' @param recorder a \linkS4class{Recorder} object
#' @param ... further arguments to the train method
#'
#' @author Alessandro Barberis
#'
#' @keywords internal
tune_via_grid_search <- function(
  # tuner,
  sampler,
  looper = Looper(),
  screener,
  trainer,
  tester,
  selector,
  hyperparameters,
  logger,
  x,
  y,
  weights,
  offset,
  resp.type,
  screening = NULL,
  keep.fit = FALSE,
  rule = c("opt", "1se"),
  observations = NULL,
  marker,
  recorder,
  ...
){

  #--------------------------------------------------------------------------------------------#
  #method
  tuning.method = "grid.search"

  #--------------------------------------------------------------------------------------------#
  #Check input
  if(missing(sampler)){stop("'sampler' missing with no default.\n")}
  # if(missing(looper)){ stop("'looper' missing with no default.\n")}
  if(missing(screener)){stop("'screener' missing with no default.\n")}
  if(missing(trainer)){stop("'trainer' missing with no default.\n")}
  if(missing(tester)){stop("'tester' missing with no default.\n")}
  if(missing(selector)){stop("'selector' missing with no default.\n")}
  if(missing(hyperparameters)){stop("'hyperparameters' missing with no default.\n")}

  hyperparameters = check_tuning_hyperparameters(hyperparameters, trainer)

  if(missing(logger)){logger = get_logger(trainer)}
  logger = open_con(logger)

  # #--------------------------------------------------------------------------------------------#
  # #Check scorer
  # check_type_measure(type.measure = get_id(get_scorer(tester)), resp.type = resp.type)

  #--------------------------------------------------------------------------------------------#
  #Number of features/observations
  nfeats = ncol(x)
  nobs = ifelse(test = S4Vectors::isEmpty(observations), yes = nrow(x), no = length(observations))

  #--------------------------------------------------------------------------------------------#
  #check names
  if(is.null(colnames(x))){
    #set colnames
    colnames(x) = paste0("V", seq(nfeats))
  }

  #--------------------------------------------------------------------------------------------#
  #Number of features to keep after screening
  do.screening = FALSE

  #check
  if(missing(screening)){screening = hyperparameters$screening}
  screening = check_tune_formal_screening(screening)

  if(!S4Vectors::isEmpty(screening)){
    hyperparameters$screening = NULL
    #flag
    do.screening = TRUE
    #
    len.screening  = length(screening)
    #set max number of features to keep
    maxvars = as.integer(max(screening, na.rm = TRUE))
  }

  #--------------------------------------------------------------------------------------------#
  #Samples
  #check N
  if(S4Vectors::isEmpty(get_N(sampler))){sampler = set_N(sampler, nobs)}
  #check strata
  if(!S4Vectors::isEmpty(observations)){sampler = subset_observations(object = sampler, which = observations)}
  #Get samples
  samples = resample(object = sampler)
  #check
  if(!S4Vectors::isEmpty(observations)){
    #update indices
    samples = lapply(X = samples, FUN = subset_observations, object = observations)
  }
  #--------------------------------------------------------------------------------------------#
  #Screen
  if(do.screening){
    log_debug(object = logger, message = "Features screening", sep = "\n", add.level = TRUE, add.time = TRUE)
    screenedlist = get_resample_screenedlist(
      screener = screener,
      x = x,
      y = y,
      weights = weights,
      maxvars = maxvars,
      resp.type = resp.type,
      order = "increasing",
      looper = looper,
      logger = logger,
      observations = samples)

    #get index
    featureslist = lapply(screenedlist, get_index)

    #reshape
    featureslist = lapply(X = featureslist, FUN = function(features, n){lapply(X = n, FUN = function(maxvar, features){features[1:maxvar]}, features = features)}, n = screening)

  } else {
    screenedlist = NULL
    featureslist = NULL
  }

  #--------------------------------------------------------------------------------------------#
  #Train
  log_debug(object = logger, message = "Training the models", sep = "\n", add.level = TRUE, add.time = TRUE)
  trainedlist = get_resample_trainedlist(
    trainer         = trainer,
    x               = x,
    y               = y,
    weights         = weights,
    offset          = offset,
    resp.type       = resp.type,
    looper          = looper,
    logger          = logger,
    observations    = samples,
    features        = featureslist,
    hyperparameters = hyperparameters,
    .inorder        = FALSE,
    ...)

  #--------------------------------------------------------------------------------------------#
  #Get data
  indices = unlist(sapply(X = trainedlist, FUN = "[[", "indices"))
  trainedlist = lapply(X = trainedlist, FUN = "[[", "fitlist")

  #Get order
  ordered = order(indices, decreasing = FALSE)
  # ordered
  #Order trained
  trainedlist = trainedlist[ordered]

  #--------------------------------------------------------------------------------------------#
  #create indices for testing data
  if(S4Vectors::isEmpty(observations)){
    itest = lapply(X = samples, FUN = setdiff, x = seq(nobs))
  } else {
    itest = lapply(X = samples, FUN = setdiff, x = observations)
  }


  #--------------------------------------------------------------------------------------------#
  #reshape to have a list where each element corresponds to a different configuration
  trainedlist = do.call(what = Map, args = c(f = c, trainedlist))

  #--------------------------------------------------------------------------------------------#
  #stability
  if(!missing(recorder)){
    log_debug(object = logger, message = "Recording features presence...", sep = "", add.level = TRUE, add.time = TRUE)
    #Record presence of feature in trained models
    recordedl = lapply(X =  unlist(lapply(X = trainedlist, FUN = unlist, recursive = T)), FUN = record, recorder = recorder, features = colnames(x))
    #reshape to have one element per response
    recordedl = do.call(what = Map, args = c(f = cbind, recordedl))
    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    #stability
    log_debug(object = logger, message = "Computing features stability...", sep = "", add.level = TRUE, add.time = TRUE)
    stability = lapply(X = recordedl, FUN = rowMeans, na.rm = T)
    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    rm(recordedl)
  } else {
    stability = list()
  }

  #--------------------------------------------------------------------------------------------#
  #Force TrainedList
  trainedlist = lapply(X = trainedlist, FUN = TrainedList)

  #--------------------------------------------------------------------------------------------#
  #assess
  log_debug(object = logger, message = "Assessing the models on validation data", sep = "\n", add.level = TRUE, add.time = TRUE)
  testedlist = lapply(
    X = trainedlist,
    FUN = test,
    tester     = tester,
    indices    = itest,
    screened   = screenedlist,
    #prediction
    newx       = x,
    newoffset  = offset,
    #test
    newy       = y,
    weights    = weights,
    score.type = c("mean_score", "summary_score", "score", "score_and_summary")[2],
    # multi    = c("average", "sum"), grouped = TRUE,#passed by scorer
    #other
    looper     = looper,
    logger     = logger)

  #--------------------------------------------------------------------------------------------#
  #reshape to have a list where each element corresponds to a different configuration
  # testedlist1 = do.call(what = Map, args = c(f = c, testedlist))
  # testedlist = unlist(x = testedlist, recursive = F)#NO MORE NEEDED, NOW TESTER HAS SINGLE SCORER

  #--------------------------------------------------------------------------------------------#
  #find best hyperparameters configuration
  log_debug(object = logger, message = "Get index of hyperparameters configurations (via optimal accuracy and one standard error rule)", sep = "\n", add.level = TRUE, add.time = TRUE)

  #extract mean error estimate and standard error
  merr   = sapply(X = testedlist, FUN = "[[", 'm')
  sderr  = sapply(X = testedlist, FUN = "[[", 'sem')

  #get index of best trained object
  selected = select(
    selector = selector,
    models   = TrainedList(lapply(X = trainedlist, FUN = "[[", 1)),
    merr     = merr,
    sderr    = sderr,
    scorer   = get_scorer(tester)
  )

  log_debug(object = logger, message = paste("Index opt:", selected$opt, "; index 1se:", selected$`1se`), sep = "\n", add.level = TRUE, add.time = TRUE)

  #subset - extract config from trained model in 1st fold
  log_debug(object = logger, message = "Selecting hyperparameters configurations (via optimal accuracy and one standard error rule)", sep = "\n", add.level = TRUE, add.time = TRUE)
  # config.opt = get_config(object = trainedlist[[selected$opt]][[1]])
  # config.1se = get_config(object = trainedlist[[selected$`1se`]][[1]])
  trained.opt = trainedlist[[selected$opt]][[1]]
  trained.1se = trainedlist[[selected$`1se`]][[1]]

  #--------------------------------------------------------------------------------------------#
  # train final model on all available data
  #--------------------------------------------------------------------------------------------#

  if(do.screening){
    log_debug(object = logger, message = "Screening", sep = "\n", add.level = TRUE, add.time = TRUE)
    #screening (will select biggest maxvars by default)
    # screened = screen(object = screener)
    screened = screen(screener = screener, x = x, y = y, weights = weights, observations = observations, resp.type = resp.type, order = "increasing")
  }

  #--------------------------------------------------------------------------------------------#
  #CONFIG OPTIMAL ACCURACY
  log_debug(object = logger, message = "Configuration selected to optimise accuracy", sep = "\n", add.level = TRUE, add.time = TRUE)

  #Setup
  if(do.screening){
    #get maxnvar
    maxvars.opt = get_screened_nvar_from_config(object = trained.opt)
    #update screened
    screened.i = update_n(object = screened, value = maxvars.opt)
    #remove
    config = get_config(del_screened_nvar_from_config(object = trained.opt))
    #get features
    features = get_index(screened.i)[1:maxvars.opt]
  } else {
    config = get_config(trained.opt)
    screened.i = Screened()
    features = NULL
  }

  #Training
  log_debug(object = logger, message = paste("Training"), sep = "\n", add.level = TRUE, add.time = TRUE)
  trained = do.call(
    what = train,
    args = c(
      list(
        trainer   = trainer,
        x         = x,
        y         = y,
        weights   = weights,
        offset    = offset,
        resp.type = resp.type,
        features  = features,
        observations = observations
      ),
      config,
      list(...))
  )

  log_trace(object = logger, message = paste("Creating Tuned object..."), sep = "", add.level = TRUE, add.time = TRUE)
  # tuned.opt = Tuned(
  #   tuned             = trained,
  #   screened          = screened.i,
  #   mean.error        = merr[selected$opt],
  #   sd.error          = sderr[selected$opt],
  #   tuning.method     = get_id(tuner),
  #   resampling.method = get_method(sampler),
  #   type.measure      = get_measure(tester),
  #   xvars             = ncol(x),
  #   xobs              = nrow(x),
  #   index             = selected$opt,
  #   trained           = trainedlist,
  #   validated         = testedlist
  # )
  tuned.opt = Tuned(
    model     = trained,
    config    = "opt",
    mscore    = merr[selected$opt],
    sem       = sderr[selected$opt],
    tuning    = tuning.method,
    # sampling  = get_method(sampler),
    sampling  = get_name_sampling_strategy(object = sampler),
    measure   = get_measure(tester),
    screened  = screened.i,
    xvars     = ncol(x),
    xobs      = nrow(x),
    index     = selected$opt,
    trained   = trainedlist,
    validated = testedlist,
    stability = stability
  )
  log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  rm(trained.opt)
  #--------------------------------------------------------------------------------------------#
  #CONFIG 1SE RULE
  log_debug(object = logger, message = "Configuration selected via one standard error rule", sep = "\n", add.level = TRUE, add.time = TRUE)

  #check match
  if(selected$opt == selected$`1se`){
    log_debug(
      object = logger,
      message = "Configuration equivalent to optimal configuration: couldn't find a simpler model with mean accuracy within 1SE from optimal accuracy",
      sep = "\n", add.level = TRUE, add.time = TRUE)
    tuned.1se = tuned.opt
    tuned.1se = set_config(tuned.1se, "1se")
  } else {
    #Setup
    if(do.screening){
      #get maxnvar
      maxvars.1se = get_screened_nvar_from_config(object = trained.1se)
      #update screened
      screened.i = update_n(object = screened, value = maxvars.1se)
      #remove
      config = get_config(del_screened_nvar_from_config(object = trained.1se))
      #get features
      features = get_index(screened.i)[1:maxvars.1se]
    } else {
      config = get_config(trained.1se)
      screened.i = Screened()
      features = NULL
    }

    #Training
    log_debug(object = logger, message = paste("Training"), sep = "\n", add.level = TRUE, add.time = TRUE)
    trained = do.call(
      what = train,
      args = c(
        list(
          trainer   = trainer,
          x         = x,
          y         = y,
          weights   = weights,
          offset    = offset,
          resp.type = resp.type,
          features  = features,
          observations = observations
        ),
        config,
        list(...))
    )

    log_trace(object = logger, message = paste("Creating Tuned object..."), sep = "", add.level = TRUE, add.time = TRUE)
    # tuned.1se = Tuned(
    #   tuned             = trained,
    #   screened          = screened.i,
    #   mean.error        = merr[selected$`1se`],
    #   sd.error          = sderr[selected$`1se`],
    #   tuning.method     = get_id(tuner),
    #   resampling.method = get_method(sampler),
    #   type.measure      = get_measure(tester),
    #   xvars             = ncol(x),
    #   xobs              = nrow(x),
    #   index             = selected$`1se`,
    #   trained           = trainedlist,
    #   validated         = testedlist
    # )
    tuned.1se = Tuned(
      model     = trained,
      config    = "1se",
      mscore    = merr[selected$`1se`],
      sem       = sderr[selected$`1se`],
      tuning    = tuning.method,
      # sampling  = get_method(sampler),
      sampling  = get_name_sampling_strategy(object = sampler),
      measure   = get_measure(tester),
      screened  = screened.i,
      xvars     = ncol(x),
      xobs      = nrow(x),
      index     = selected$`1se`,
      trained   = trainedlist,
      validated = testedlist,
      stability = stability
    )
    log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  }
  rm(trained.1se)

  #--------------------------------------------------------------------------------------------#
  log_trace(object = logger, message = paste("Creating output list..."), sep = "", add.level = TRUE, add.time = TRUE)
  out = list('opt' = tuned.opt, '1se' = tuned.1se)
  log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------------------------------------#
  log_trace(object = logger, message = paste("Creating output object..."), sep = "", add.level = TRUE, add.time = TRUE)
  out = TunedList(out)
  log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------------------------------------#
  #close
  close_con(logger)

  #--------------------------------------------------------------------------------------------#
  return(out)
}


get_name_config_slot <- function(config = c("opt", "1se")){
  out = switch(
    config,
    'opt' = 'config.opt',
    '1se' = 'config.1se'
  )
  return(out)
}

get_config_log_msg <- function(config = c("opt", "1se")){

  config = match.arg(config)

  out = switch(
    config,
    'opt' = "Configuration selected to optimise accuracy",
    '1se' = "Configuration selected via one standard error rule"
  )

  return(out)
}

#'Check tuning type measure
#'@description Check if the selected accuracy measure is supported for tuning.
#'@param type.measure the accuracy measure
#'@param resp.type the response type
#'@keywords internal
check_tuning_type_measure = function (
  type.measure = "mse",
  resp.type = "gaussian")
{

  type.measure = type.measure[1]

  type.measure = check_type_measure(type.measure = type.measure, resp.type = resp.type)

  #supported measures
  supported.measures = c("squared_error", "deviance", "class", "auc", "absolute_error", "ape", "sle")

  #check if supported
  is.supported = type.measure %in% supported.measures

  if(!is.supported){
    logMsg = paste("The selected accuracy measure is not supported. Please, select one of the following:\n",supported.measures,"\n")
    stop(logMsg)
  }

  return(type.measure)
}


renoir_tuner_default_hyperparameters_glmnet <- function(
  learning.method = c("lasso", "ridge", "elnet", "elasticnet",
                      "relaxed_elasticnet", "relaxed_lasso", "relaxed_ridge", "relaxed_elnet"),
  name){

  learning.method = match.arg(learning.method)

  #default
  lambda = 10^seq(3, -2, length=100)
  gamma  = c(0, 0.25, 0.5, 0.75, 1)

  alpha = switch(
    learning.method,
    # glmnet = seq(0,1,length=11),
    lasso  = 1,
    ridge  = 0,
    elnet  = 0.5,
    elasticnet = 0.5,
    # relaxed_glmnet = seq(0,1,length=11),
    relaxed_lasso  = 1,
    relaxed_ridge  = 0,
    relaxed_elnet  = 0.5,
    relaxed_elasticnet = 0.5)

  #set out list
  out = list(alpha = alpha, lambda = lambda, gamma = gamma)

  #check if relax
  if(!(learning.method %in% glmnet_learning_method_names(all = F, relaxed = T))){
    out[['gamma']] = NULL
  }

  #if name is provided
  if(!missing(name) && !is.null(name)){
    #get names
    lnames = names(out)
    #match
    name = match(x = name, table = lnames)
    #check
    if(!is.na(name)){
      out = out[[name]]
    }
  }

  return(out)
}

renoir_tuner_default_hyperparameters_randomForest <- function(name){

  #n
  out = list(ntree = c(10, 50, 100, 250, 500))

  #if name is provided
  if(!missing(name) && !is.null(name)){
    #get names
    lnames = names(out)
    #match
    name = match(x = name, table = lnames)
    #check
    if(!is.na(name)){
      out = out[[name]]
    }
  }

  return(out)
}

renoir_tuner_default_hyperparameters_gbm <- function(name){

  #n
  out = list(ntree = c(10, 50, 100, 250, 500), eta = c(0.1, 0.01))

  #if name is provided
  if(!missing(name) && !is.null(name)){
    #get names
    lnames = names(out)
    #match
    name = match(x = name, table = lnames)
    #check
    if(!is.na(name)){
      out = out[[name]]
    }
  }

  return(out)
}

renoir_tuner_default_hyperparameters_xgboost <- function(name){

  #n
  out = list(
    nrounds = c(50, 100, 500),
    eta = c(0.3, 0.1),
    lambda = seq(from = 1, to = 0.1, length.out = 3),
    alpha = seq(from = 0, to = 1, length.out = 3)
  )

  #if name is provided
  if(!missing(name) && !is.null(name)){
    #get names
    lnames = names(out)
    #match
    name = match(x = name, table = lnames)
    #check
    if(!is.na(name)){
      out = out[[name]]
    }
  }

  return(out)
}

renoir_tuner_default_hyperparameters_svm <- function(name, learning.method){

  #n
  out = list(
    cost   = 2^seq(from = -5, to = 15, length.out = 11),
    gamma  = 2^seq(from = -15, to = 3, length.out = 10),
    degree = seq(from = 1, to = 3, length.out = 3),
    # nu     = 10^seq(from = -5, to = 0, length.out = 6)
    nu     = seq(from = 0.1, to = 0.9, length.out = 5)
  )

  out = out[renoir_trainer_default_hyperparameters(learning.method)]

  #if name is provided
  if(!missing(name) && !is.null(name)){
    #get names
    lnames = names(out)
    #match
    name = match(x = name, table = lnames)
    #check
    if(!is.na(name)){
      out = out[[name]]
    }
  }

  return(out)
}

renoir_tuner_default_hyperparameters_gknn <- function(name, learning.method){

  #n
  out = list(
    k = seq(from = 1, to = 9, length.out = 5)
  )

  #if name is provided
  if(!missing(name) && !is.null(name)){
    #get names
    lnames = names(out)
    #match
    name = match(x = name, table = lnames)
    #check
    if(!is.na(name)){
      out = out[[name]]
    }
  }

  return(out)
}


renoir_tuner_default_hyperparameters <- function(learning.method, name){
  # #get trainer
  # trainer = get_trainer(tuner)
  # #learning method
  # learning.method = get_learning_method(trainer)
  #check if one of supported
  if(learning.method %in% renoir_learning_methods()){
    out = switch(
      learning.method,
      # glmnet         = renoir_tuner_default_hyperparameters_glmnet(learning.method = "glmnet", name),
      lasso          = renoir_tuner_default_hyperparameters_glmnet(learning.method = "lasso", name),
      ridge          = renoir_tuner_default_hyperparameters_glmnet(learning.method = "ridge", name),
      elnet          = renoir_tuner_default_hyperparameters_glmnet(learning.method = "elnet", name),
      elasticnet     = renoir_tuner_default_hyperparameters_glmnet(learning.method = "elasticnet", name),
      # relaxed_glmnet = renoir_tuner_default_hyperparameters_glmnet(learning.method = "relaxed_glmnet", name),
      relaxed_lasso  = renoir_tuner_default_hyperparameters_glmnet(learning.method = "relaxed_lasso", name),
      relaxed_ridge  = renoir_tuner_default_hyperparameters_glmnet(learning.method = "relaxed_ridge", name),
      relaxed_elnet  = renoir_tuner_default_hyperparameters_glmnet(learning.method = "relaxed_elnet", name),
      relaxed_elasticnet  = renoir_tuner_default_hyperparameters_glmnet(learning.method = "relaxed_elasticnet", name),
      randomForest   = renoir_tuner_default_hyperparameters_randomForest(name),
      gbm            = renoir_tuner_default_hyperparameters_gbm(name),
      xgbtree        = renoir_tuner_default_hyperparameters_xgboost(name),
      xgblinear      = renoir_tuner_default_hyperparameters_xgboost(name),
      linear_SVM         = renoir_tuner_default_hyperparameters_svm(learning.method = learning.method, name = name),
      polynomial_SVM     = renoir_tuner_default_hyperparameters_svm(learning.method = learning.method, name = name),
      radial_SVM         = renoir_tuner_default_hyperparameters_svm(learning.method = learning.method, name = name),
      sigmoid_SVM        = renoir_tuner_default_hyperparameters_svm(learning.method = learning.method, name = name),
      linear_NuSVM       = renoir_tuner_default_hyperparameters_svm(learning.method = learning.method, name = name),
      polynomial_NuSVM   = renoir_tuner_default_hyperparameters_svm(learning.method = learning.method, name = name),
      radial_NuSVM       = renoir_tuner_default_hyperparameters_svm(learning.method = learning.method, name = name),
      sigmoid_NuSVM      = renoir_tuner_default_hyperparameters_svm(learning.method = learning.method, name = name),
      gknn               = renoir_tuner_default_hyperparameters_gknn(learning.method = learning.method, name = name),
      nsc                = renoir_tuner_default_hyperparameters_nsc(learning.method = learning.method, name = name)
    )
  } else {
    #raise a warning
    warning("\nNo hyperparameters list for the user-provided learning method.\n")
    out = list()
  }
  return(out)
}


update_tuning_hyperparameters <- function(hyperparameters, tuner){

}


check_tuning_hyperparameters <- function(hyperparameters, trainer){
# check_tuning_hyperparameters <- function(hyperparameters, trainer, screener){

  if(missing(trainer)){stop("'trainer' must be provided")}

  #learning method
  learning.method = get_id(trainer)

  if(missing(hyperparameters) || is.null(hyperparameters) || length(hyperparameters)==0){
    #get method
    hyperparameters = renoir_tuner_default_hyperparameters(learning.method)
  } else {

    #check screening
    # hyperparameters = check_tuning_hyperparameters_screener(hyperparameters = hyperparameters, screener = screener)
    hyperparameters = check_tuning_hyperparameters_screener(hyperparameters = hyperparameters)

    #check if supported
    if(isTRUE(learning.method %in% renoir_learning_methods())){
      if(isTRUE(learning.method %in% glmnet_learning_method_names(all = T))){
        hyperparameters = check_tuning_hyperparameters_glmnet(hyperparameters, trainer)
      } else {
        hyperparameters = switch(
          learning.method,
          randomForest = check_tuning_hyperparameters_randomForest(hyperparameters, trainer),
          hyperparameters
        )
      }
    }
  }

  return(hyperparameters)
}


check_tuning_hyperparameters_screener <- function(hyperparameters){
  #check if screening is provided
  screening = hyperparameters$screening;

  if(!is.null(screening) && !S4Vectors::isEmpty(screening)){
    if(!all(screening > 0)){
      warning("Negative values not allowed in screening hyperparameter.\n")

      #subset
      screening = screening[screening>0]

      if(!S4Vectors::isEmpty(screening)){
        hyperparameters$screening = screening
      } else {
        warning("Provided screening hyperparameter not valid. Hyperparameter forced to default value (no screening).\n")
        hyperparameters$screening = integer()
      }
    }
  } else {
    hyperparameters$screening = NULL
  }

  return(hyperparameters)
}

check_tune_formal_screening <- function(screening){
  if(!S4Vectors::isEmpty(screening)){
    if(!all(screening > 0)){
      warning("Negative values not allowed in screening hyperparameter.\n")
      #subset
      screening = screening[screening>0]
      #check
      if(S4Vectors::isEmpty(screening)){
        warning("Provided screening hyperparameter not valid: forced to default value (no screening).\n")
      }
    }
  }

  return(screening)
}

# check_tuning_hyperparameters_screener <- function(hyperparameters, screener){
#   #check if screening is provided
#   screening = hyperparameters$screening;
#   maxvars   = get_maxvars(object = screener)
#
#   if(!is.null(screening) && !S4Vectors::isEmpty(screening)){
#     if(!all(screening > 0)){
#       if(!S4Vectors::isEmpty(maxvars)){
#         hyperparameters$screening = maxvars
#       } else {
#         warning("Provided screening hyperparameter not valid. Hyperparameter forced to default value (no screening).\n")
#         hyperparameters$screening = integer()
#       }
#     }
#   } else if(!S4Vectors::isEmpty(maxvars)){
#     hyperparameters$screening = maxvars
#   } else {
#     hyperparameters$screening = NULL
#   }
#
#   return(hyperparameters)
# }

check_tuning_hyperparameters_glmnet <- function(hyperparameters, trainer){

  if(missing(trainer)){stop("'trainer' must be provided")}

  #learning method
  learning.method = get_id(trainer)

  #get default
  hdef = renoir_tuner_default_hyperparameters(learning.method)
  hnames = names(hyperparameters)

  #check if names common with default
  if(any(!(hnames %in% names(hdef)))){
    warning("\nProvided hyperparameter names different from default for selected learning method.\n")
  }

  #check if any default parameter is missing
  if(any(!(names(hdef) %in% hnames))){
    alpha = hyperparameters$alpha;
    alpha.def = renoir_tuner_default_hyperparameters(learning.method = learning.method, name = "alpha")

    if(is.null(alpha)){
      hyperparameters$alpha = alpha.def
    } else {
      if(!identical(alpha, alpha.def)){
        if(learning.method %in% c("lasso", "ridge", "relaxed_lasso", "relaxed_ridge")){
          warning("Provided hyperparameter not matching learning method. Hyperparameter forced to default value.\n")
          hyperparameters$alpha = alpha.def
        } else if(learning.method %in% c("elasticnet", "relaxed_elasticnet")) {
          if(!(alpha>=0 && alpha<=1)){
            warning("Provided hyperparameter not matching learning method. Hyperparameter forced to default value.\n")
            hyperparameters$alpha = alpha.def
          }
        } else {
          if(!(alpha>=0 && alpha<=1)){
            warning("Provided hyperparameter not matching learning method. Hyperparameter forced to default value.\n")
            hyperparameters$alpha = alpha.def
          }
        }
      }
    }

    lambda = hyperparameters$lambda
    lambda.def = renoir_tuner_default_hyperparameters(learning.method = learning.method, name = "lambda")

    if(is.null(lambda)){
      hyperparameters$lambda = lambda.def
    }

    if(learning.method %in% glmnet_learning_method_names(all = F, relaxed = T)){
      gammav = hyperparameters$gamma
      gamma.def = renoir_tuner_default_hyperparameters(learning.method = learning.method, name = "gamma")

      if(is.null(gammav)){
        hyperparameters$gamma = gamma.def
      } else {
        if(!(gammav>=0 && gammav<=1)){
          warning("Provided hyperparameter not matching learning method. Hyperparameter forced to default value.\n")
          hyperparameters$gamma = gamma.def
        }
        if (max(gammav) < 1) {
          hyperparameters$gamma = c(gammav, 1)
        }
      }
    }

  }

  return(hyperparameters)
}

check_tuning_hyperparameters_randomForest <- function(hyperparameters, trainer){

  if(missing(trainer)){stop("'trainer' must be provided")}

  #learning method
  learning.method = get_id(trainer)

  #get default
  hdef = renoir_tuner_default_hyperparameters(learning.method)
  hnames = names(hyperparameters)

  #check if names common with default
  if(any(!(hnames %in% names(hdef)))){
    warning("\nProvided hyperparameter names different from default for selected learning method.\n")

    ntree = hyperparameters$ntree;
    ntree.def = renoir_tuner_default_hyperparameters(learning.method = learning.method, name = "ntree")

    if(is.null(ntree)){
      hyperparameters$ntree = ntree.def
    } else {
      if(!(ntree>0)){
        warning("Provided hyperparameter not matching learning method. Hyperparameter forced to default value.\n")
        hyperparameters$ntree = ntree.def
      }
    }

  }

  return(hyperparameters)
}


create_hyperparameters_grid <- function(hyperparameters, learning.method){

  #create grid
  if(isTRUE(learning.method %in% glmnet_learning_method_names(all = T))){
    grid = create_hyperparameters_grid_glmnet(hyperparameters = hyperparameters, learning.method = learning.method)
  } else {
    #get all combination of hyperparameters to test (i.e. configurations)
    grid = base::expand.grid(hyperparameters, stringsAsFactors = F)
  }

  return(grid)
}

create_hyperparameters_grid_glmnet <- function(hyperparameters, learning.method){
  #lambda
  lambda = hyperparameters$lambda
  gamma  = hyperparameters$gamma

  #update
  hyperparameters[c('lambda', 'gamma')] = NULL

  #create grid
  #get all combination of hyperparameters to test (i.e. configurations)
  grid = base::expand.grid(hyperparameters, stringsAsFactors = F)

  #add
  if(!is.null(lambda)){
    grid$lambda = rep(list(lambda), nrow(grid))
  }

  #check if relaxed
  if((learning.method %in% glmnet_learning_method_names(all = F, relaxed = T)) && !is.null(gamma)){
    grid$gamma  = rep(list(gamma), nrow(grid))
  }

  return(grid)
}

