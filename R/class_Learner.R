#' @include classes_generics.R class_Logger.R class_Tuner.R class_Trainer.R class_Tester.R class_Forecaster.R class_ScorerList.R class_Selector.R class_Sampler.R class_Recorder.R class_Marker.R
#class_Resampler.R  class_Learned.R class_LearnedList.R class_Looper.R
NULL


#' Learner Class
#'
#' @description An S4 class representing a learner.
#'
#' The object consists of 8 slots
# @slot resampler a \linkS4class{Resampler} object containing the sampling procedure to use for assessing the
# tuned models
#' @slot tuner a \linkS4class{Tuner} object
#' @slot trainer a \linkS4class{Trainer} object
# @slot tester a \linkS4class{Tester} object
#' @slot forecaster a \linkS4class{Forecaster} object
#' @slot scorer a \linkS4class{ScorerList} object
#' @slot selector a \linkS4class{Selector} object
# @slot looper a \linkS4class{Looper}
#' @slot recorder a \linkS4class{Recorder} object
#' @slot marker a \linkS4class{Marker} object
#' @slot logger a \linkS4class{Logger} object
#'
#' @author Alessandro Barberis
methods::setClass(
  Class = "Learner",
  slots = c(
    # resampler = "Resampler",
    tuner     = "Tuner",
    trainer   = "Trainer",
    # tester    = "Tester",
    forecaster= "Forecaster",
    scorer    = "ScorerList",
    selector  = "Selector",
    recorder  = "Recorder",
    marker    = "Marker",
    # looper    = "Looper",
    logger    = "Logger"
  )
)

#' Learner Class Constructor
#'
#' @description Constructor for the S4 \linkS4class{Learner} object.
#'
#' @param tuner a \linkS4class{Tuner} object
#' @param trainer a \linkS4class{Trainer} object
#' @param forecaster a \linkS4class{Forecaster} object
#' @param scorer a \linkS4class{ScorerList} or a \linkS4class{Scorer} object
#' @param selector a \linkS4class{Selector} object
#' @param recorder a \linkS4class{Recorder} object
#' @param marker a \linkS4class{Marker} object
#' @param logger a \linkS4class{Logger} object
#'
#'@return a \linkS4class{Learner} object
#'
#'@export
#'
#'@author Alessandro Barberis
#'
#'@rdname Learner-class
methods::setMethod(
  f = "Learner",
  signature = methods::signature(id = "missing"),
  definition = function(
  # resampler = Resampler(),
  tuner     = Tuner(),
  trainer,
  # tester,
  forecaster,
  scorer     = Scorer(id = "mse"),
  selector,
  recorder,
  marker,
  # looper    = Looper(),
  logger    = Logger()
){

  # if(missing(tester)){tester = get_tester(tuner)}

  #Check scorer
  if(is.Scorer(scorer)){scorer = ScorerList(scorer)}

  methods::new(
    Class = "Learner",
    # resampler = resampler,
    tuner     = tuner,
    trainer   = trainer,
    # tester    = tester,
    forecaster= forecaster,
    scorer    = scorer,
    selector  = selector,
    recorder  = recorder,
    marker    = marker,
    # looper    = looper,
    logger    = logger
  )
})

#' Learner Class Constructor
#'
#' @description Constructor for the S4 \linkS4class{Learner} object.
#'
#'@param id learning method
#'@param tuning the tuning method
#'@param screening the screening method
#'@param sampling the sampling strategy
#'@inheritParams Sampler
#'@param score the accuracy measure to use during tuning
#'@param resp.type the response type
#'@param looper a \linkS4class{Looper}
#'@param logger a \linkS4class{Logger}
#'
#'@return a \linkS4class{Learner} object
#'
#'@export
#'
#'@author Alessandro Barberis
#'
#'@rdname Learner-class
methods::setMethod(
  f = "Learner",
  signature = methods::signature(id = "character"),
  definition = function(
    #Trainer
    id,
    #Tuner
    tuning    = "grid.search",
    #Screener
    screening,
    #Sampler
    sampling  = c("cv", "random", "bootstrap"),
    strata    = NULL,
    balance   = FALSE,
    k         = 10L,
    #Scorer
    score     = "mse",
    resp.type,
    looper    = Looper(),
    #Logger
    logger    = Logger()
){


  if(missing(resp.type)){stop("Please, provide the 'resp.type'.")}
  #--------------------------------------------------------------------------------------------#
  sampling = match.arg(sampling)

  #--------------------------------------------------------------------------------------------#
  #check id
  if(id %in% supported_learning_methods()){
    #Create objects
    trainer    = Trainer(id = id, logger = logger)
    forecaster = Forecaster(id = id)#, logger = logger)
    selector   = Selector(id = id, logger = logger)
    recorder   = Recorder(id = id, logger = logger)
    marker     = Marker(id = id, logger = logger)

  } else {
    stop("'id' must be one of the supported learning methods. Please run 'supported_learning_methods()' to list the available methods.\n")
  }

  #--------------------------------------------------------------------------------------------#
  #Screening
  if(!missing(screening)){
    if(all(screening %in% supported_screening_methods())){
      screener = list()
      for(iscreen in screening){
        screener[[iscreen]] = Screener(id = iscreen, logger = logger)
      }
      screener = ScreenerList(screener)
    } else {
      stop("'screening' should contain one or more supported screening methods. Please run 'supported_screening_methods()' to list the available methods.\n")
    }
  } else {
    screener = ScreenerList(Screener(id = "ebayes"))
  }

  #Set tuner
  if(tuning %in% supported_tuning_methods()){
    #Create objects
    tuner = Tuner(
      id       = tuning,
      sampler  = Sampler(
        method  = sampling,
        strata  = strata,
        balance = balance,
        k       = k
      ),
      screener = screener,
      looper   = looper,
      logger   = logger
    )

  } else {
    stop("'tuning' must be one of the supported tuning methods. Please run 'supported_tuning_methods()' to list the available methods.\n")
  }

  #--------------------------------------------------------------------------------------------#
  #Scorer
  if(score %in% supported_scoring_methods()){
    #Create objects
    scorer = Scorer(
      id        = score,
      resp.type = resp.type,
      logger    = logger
    )

    scorer = ScorerList(scorer)
  } else {
    stop("'score' must be one of the supported scoring methods. Please run 'supported_scoring_methods()' to list the available methods.\n")
  }

  #--------------------------------------------------------------------------------------------#
  #Create
  methods::new(
    Class = "Learner",
    tuner     = tuner,
    trainer   = trainer,
    forecaster= forecaster,
    scorer    = scorer,
    selector  = selector,
    recorder  = recorder,
    marker    = marker,
    logger    = logger
  )
})


#'@keywords internal
is_valid_learner_object <- function(object) {
  errors <- character()

  trainerid = get_id(get_trainer(object))

  # if (is.na(match(x = get_id(get_forecaster(get_tester(object))), table = trainerid))) {
  #   msg <- paste("'forecaster' in 'tester' not matching provided 'trainer' id. ", sep = "")
  #   errors <- c(errors, msg)
  # }

  if (is.na(match(x = get_id(get_forecaster(object)), table = trainerid))) {
    msg <- paste("'forecaster' not matching provided 'trainer' id. ", sep = "")
    errors <- c(errors, msg)
  }

  #Get prediction type for accuracy measure
  measures = get_id(get_scorer(object))
  pred.types = sapply(X = measures, FUN = get_prediction_type, object = get_forecaster(object))
  if(anyNA(pred.types)){
    measures = measures[which(is.na(pred.types))]
    msg <- paste("'scorer' (",paste0(measures, collapse = ", "),") not supported by provided 'forecaster' id. Please check the 'forecaster' or change the 'scorer' ", sep = "")
    errors <- c(errors, msg)
  }

  if (is.na(match(x = get_id(get_selector(object)), table = trainerid))) {
    msg <- paste("'selector' not matching provided 'trainer' id. ", sep = "")
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}

#'@rdname Learner-class
is.Learner <- function(object){
  out = !is.na(match("Learner", table = class(object)))
  return(out)
}

methods::setValidity(Class = "Learner", method = is_valid_learner_object)

methods::setMethod(f = "get_tuner",     signature = "Learner", definition = function(object){methods::slot(object = object, name = 'tuner')})
methods::setMethod(f = "get_trainer",   signature = "Learner", definition = function(object){methods::slot(object = object, name = 'trainer')})
methods::setMethod(f = "get_tester",    signature = "Learner", definition = function(object){methods::slot(object = object, name = 'tester')})
methods::setMethod(f = "get_forecaster",signature = "Learner", definition = function(object){methods::slot(object = object, name = 'forecaster')})
methods::setMethod(f = "get_scorer",    signature = "Learner", definition = function(object){methods::slot(object = object, name = 'scorer')})
methods::setMethod(f = "get_selector",  signature = "Learner", definition = function(object){methods::slot(object = object, name = 'selector')})
methods::setMethod(f = "get_recorder",  signature = "Learner", definition = function(object){methods::slot(object = object, name = 'recorder')})
methods::setMethod(f = "get_marker",    signature = "Learner", definition = function(object){methods::slot(object = object, name = 'marker')})
methods::setMethod(f = "get_logger",    signature = "Learner", definition = function(object){methods::slot(object = object, name = 'logger')})

methods::setMethod(f = "get_id",        signature = "Learner", definition = function(object){get_id(get_trainer(object))})


#' Learn
#'
#' @description Function used to run a learning process.
#' The function invokes particular methods which depend on the
#' presence of the second argument. If \code{hyperparameters}
#' is provided then a tuning is performed, while a training
#' is run otherwise.
#'
#' @param learner an object of class \linkS4class{Learner}
#' @param hyperparameters (optional) list containing the hyperparameters to tune
#' @param ... additional arguments affecting the learning procedure
#'
#' @return The form of the returned object depends on the type
#' of learning. See the documentation of the particular method for
#' further details.
#'
#' @rdname learn
#'
#'@seealso
#'\code{\link{tune}},
#'\code{\link{train}}
#'
#'@export
#'
#'@author Alessandro Barberis
methods::setMethod(
  f = "learn",
  signature = methods::signature(learner = "Learner"),
  definition = function(learner, hyperparameters = NULL, ...){
    #get trainer
    trainer = get_trainer(learner)

    #check argument
    if(isTRUE(missing(hyperparameters) || S4Vectors::isEmpty(hyperparameters))){
      n = NULL
    } else {
      #check hyperparameters space
      hp.grid = base::expand.grid(hyperparameters, stringsAsFactors = F)
      #number of configurations
      n = nrow(hp.grid)
    }

    #check
    if(isTRUE(!is.null(n) && n>1)){
      #tune
      out = tune(
        tuner    = get_tuner(learner),
        trainer  = get_trainer(learner),
        # tester   = get_tester(learner),
        tester   = Tester(forecaster = get_forecaster(learner), scorer = get_scorer(learner)[[1]]),
        selector = get_selector(learner),
        hyperparameters = hyperparameters,
        recorder        = get_recorder(learner),
        ...)
    } else {
      #train
      out = do.call(what = train, args = c(list(trainer = get_trainer(learner)), list(...), hyperparameters))
    }

    #return
    return(out)
})


#'@keywords internal
get_sample_learnedlist <- function(learner, logger, observations, ..., iloop){

  #--------------------------------------------------------------------------------------------#
  #Set logger
  if(missing(logger)){logger = get_logger(learner)}
  logger = open_con(logger)

  #--------------------------------------------------------------------------------------------#
  log_trace(object = logger, message = paste(iloop, "Learning"), sep = "\n", add.level = TRUE, add.time = TRUE)
  learned = learn(learner = learner, observations = observations[[iloop]], ...)

  #--------------------------------------------------------------------------------------------#
  #close
  close_con(logger)

  #--------------------------------------------------------------------------------------------#
  #store
  return(list(indices = iloop, learned = learned))
}

#'@keywords internal
check_sampler_for_training <- function(object){
  N = get_N(object)
  n = get_n(object)
  k = get_k(object)
  if(identical(get_method(object), "random")){
    if(n>=N){stop("Sample size 'n' should be smaller than population size 'N'. Try with 'n = N - 1'.\n")}
    if(k>1){stop("Number of repeats 'k' should be set to 1.\n")}
  } else if(identical(get_method(object), "bootstrap")){
    if(n>=N){stop("Sample size 'n' should be smaller than population size 'N'. Try with 'n = N - 1'.\n")}
  }
}

#'@keywords internal
check_sampler_for_single_sample <- function(object){
  N = get_N(object)
  n = get_n(object)
  k = get_k(object)
  if(identical(get_method(object), "random")){
    if(n>=N){stop("Sample size 'n' should be smaller than population size 'N'. Try with 'n = N - 1'.\n")}
    if(k>1){stop("Number of repeats 'k' should be set to 1.\n")}
  } else if(identical(get_method(object), "bootstrap")){
    if(n>=N){stop("Sample size 'n' should be smaller than population size 'N'. Try with 'n = N - 1'.\n")}
    if(k>1){stop("Number of repeats 'k' should be set to 1.\n")}
  }
}
