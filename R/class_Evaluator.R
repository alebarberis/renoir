#' @include classes_generics.R class_Logger.R class_ScorerList.R class_Looper.R class_Sampler.R class_LearnerList.R class_Learner.R
NULL

#' Evaluator Class
#'
#' @description
#' An S4 class providing the methods to perform an evaluation of a learning method.
#'
#' The object consists of 3 slots
# @slot resampler a \linkS4class{Resampler} object containing the sampling procedure to use for assessing the
# tuned models
#' @slot sampler a \linkS4class{Sampler} object
#' @slot looper a \linkS4class{Looper} object
#' @slot scorer a \linkS4class{ScorerList} object
#' @slot logger a \linkS4class{Logger} object
#'
#' @author Alessandro Barberis
#'
#' @export
methods::setClass(
  Class = "Evaluator",
  slots = c(
    # id              = "character",
    sampler         = "Sampler",
    looper          = "Looper",
    scorer          = "ScorerList",
    # trainer         = "Trainer",
    # tester          = "Tester",
    # selector        = "Selector",
    # hyperparameters = "list",
    # parameters      = "list",
    logger          = "Logger"
  )
)


# Evaluator <- function(
#   sampler = Sampler(method = "random", k = 1L),
#   cores,
#   score,
#   logger = Logger()
# ){
#
#   #Check scorer
#   if(is.Scorer(scorer)){scorer = ScorerList(scorer)}
#
#   #Create
#   methods::new(
#     Class = "Evaluator",
#     sampler = sampler,
#     looper  = looper ,
#     scorer  = scorer ,
#     logger  = logger
#   )
# }


#' Evaluator Class Constructor
#'
#' Constructor for the S4 \linkS4class{Evaluator} object.
#'
#' @param sampler a \linkS4class{Sampler}
#' @param looper a \linkS4class{Looper}
#' @param scorer a \linkS4class{ScorerList} or a \linkS4class{Scorer} object
#' @param logger a \linkS4class{Logger}
#'
#' @return An object of class \linkS4class{Evaluator}.
#'
#' @author Alessandro Barberis
#' @export
#'
#'@rdname Evaluator-class
Evaluator <- function(
  sampler = Sampler(method = "random", k = 1L),
  looper  = Looper(),
  scorer  = ScorerList(Scorer(id = "mse")),
  logger  = Logger()
){

  #Check scorer
  if(is.Scorer(scorer)){scorer = ScorerList(scorer)}

  #Create
  methods::new(
    Class = "Evaluator",
    sampler = sampler,
    looper  = looper ,
    scorer  = scorer ,
    logger  = logger
  )
}

methods::setMethod(f = "get_sampler", signature = "Evaluator", definition = function(object){methods::slot(object = object, name = 'sampler')})
methods::setMethod(f = "get_looper",  signature = "Evaluator", definition = function(object){methods::slot(object = object, name = 'looper')})
methods::setMethod(f = "get_scorer",  signature = "Evaluator", definition = function(object){methods::slot(object = object, name = 'scorer')})
methods::setMethod(f = "get_logger",  signature = "Evaluator", definition = function(object){methods::slot(object = object, name = 'logger')})

methods::setMethod(f = "set_sampler", signature = "Evaluator", definition = function(object, value){methods::slot(object = object, name = 'sampler') = value; return(object)})


#'Evaluate Learning Method
#'
#'@param learner an object of class \linkS4class{Learner}
#'@param evaluator an object of class \linkS4class{Evaluator}
#'@param logger an object of class \linkS4class{Logger}
#'@param x the input matrix, where rows are observations and columns are variables.
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}.
#'@param weights (optional) vector of observation weights
#'@param offset vector containing an offset, used for linear models. Default is \code{NULL}.
#'@param resp.type the response type
#'@param observations (optional) index of samples to use for training.
#'If missing, observations will be sampled by using the \linkS4class{Sampler}
#'in \linkS4class{Evaluator}. This is an helper argument, and shouldn't be used
#'directly. Information of the sampling procedure in the output \linkS4class{Evaluated}
#'object are still retrieved from \linkS4class{Sampler} in \linkS4class{Evaluator} even if
#'\code{observation} is provided.
#If missing, 70% of samples will be used for training
#'@param filename (optional) name without extension for the output file
#'@param outdir path to the output directory
#'@param rm.call logical, whether to remove the call from the models. Helpful if object size is expected to be big
#'@param rm.fit logical, whether to remove the model fits used for tuning the hyperparameters . Helpful if object size is expected to be big
#'object can be huge.
#'@param ... further arguments to `learn` function
#'
#'@rdname evaluate
methods::setMethod(
  f = "evaluate",
  # signature = methods::signature(models = "missing", learner = "Learner", hyperparameters = "ANY", sampler = "Sampler"),
  signature = methods::signature(models = "missing", learner = "Learner", evaluator = "Evaluator", npoints = "missing"),
  definition = function(
    learner,
    evaluator,
    # hyperparameters,
    x,
    y,
    weights,
    offset,
    resp.type,
    observations,
    # train.size,
    logger,
    outdir,
    filename,
    rm.call = T,
    rm.fit = F,
    ...){


    measures = get_id(get_scorer(evaluator))
    pred.types = sapply(X = measures, FUN = get_prediction_type, object = get_forecaster(learner))
    if(anyNA(pred.types)){
      measures = measures[which(is.na(pred.types))]
      msg <- paste("'scorer' (",paste0(measures, collapse = ", "),") not supported by provided 'forecaster'. Please check the 'learner' or change the 'evaluator' ", sep = "")
      # errors <- c(errors, msg)
      stop(msg)
    }

    #--------------------------------------------------------------------------------------------#
    if(missing(logger)){logger = get_logger(evaluator)}
    logger = open_con(logger)
    #--------------------------------------------------------------------------------------------#
    #Check input
    sampler = get_sampler(evaluator)
    looper  = get_looper(evaluator)
    #--------------------------------------------------------------------------------------------#
    #check
    saveRes = check_output_dir(outdir);
    if(saveRes && missing(filename)){
      warning("Output directory was provided but 'filename' is missing without default. Data won't be saved.\n")
      saveRes = FALSE
    }

    #--------------------------------------------------------------------------------------------#
    #Number of features/observations
    nfeats = ncol(x)
    nobs = nrow(x)

    #--------------------------------------------------------------------------------------------#
    # forecaster = get_forecaster(learner)
    # scorer     = get_scorer(learner)
    # selector   = get_selector(learner)

    #--------------------------------------------------------------------------------------------#
    #Samples
    if(missing(observations)){
      #check N
      if(S4Vectors::isEmpty(get_N(sampler))){sampler = set_N(sampler, nobs)}
      #check
      is.valid.sampler = is_valid_sampler_object(sampler)
      if(!isTRUE(is.valid.sampler)){stop(paste("Sampler is not valid.\n",is.valid.sampler,"\n"))}

      #Get samples
      observations = resample(object = sampler)
    }

    #--------------------------------------------------------------------------------------------#
    #indices
    obs   = seq(nobs)
    feats = seq(nfeats)

    #--------------------------------------------------------------------------------------------#
    #Learn
    log_debug(object = logger, message = "Learning", sep = "\n", add.level = TRUE, add.time = TRUE)
    learnedlist = loop(
      looper = looper,
      n.iter = length(observations),
      .inorder = F,
      fun = get_sample_learnedlist,
      learner = learner,
      observations = observations,
      x = x,
      y = y,
      weights = weights,
      offset = offset,
      resp.type = resp.type,
      # hyperparameters = hyperparameters,
      ...
    )

    #--------------------------------------------------------------------------------------------#
    #Get data
    indices = unlist(sapply(X = learnedlist, FUN = "[[", "indices"))
    learnedlist = lapply(X = learnedlist, FUN = "[[", "learned")

    #Get order
    ordered = order(indices, decreasing = FALSE)
    # ordered
    #Order trained
    learnedlist = learnedlist[ordered]

    #--------------------------------------------------------------------------------------------#
    #Check data
    if(is.Trained(learnedlist[[1]])){
      log_debug(object = logger, message = "Evaluating trained models", sep = "\n", add.level = TRUE, add.time = TRUE)

      #Force TrainedList
      models = TrainedList(learnedlist)

      #evaluate
      out = evaluate(
        models = models,
        learner = learner,
        evaluator = evaluator,
        # hyperparameters,
        # #Sample
        # sampler = sampler,
        # looper = looper,
        #Test
        # forecaster = get_forecaster(learner),
        # scorer = get_scorer(learner),
        # tester,
        x = x,
        y = y,
        weights = weights,
        offset = offset,
        # resp.type,
        observations = observations,
        # train.size,
        logger = logger,
        # indices = itrain,
        screened = NULL,
        config = character(),
        resp.type = resp.type
      )

      #Clean trained object to reduce space in memory
      out = clean(object = out, rm.call = rm.call, rm.fit = rm.fit)

    } else if(is.TunedList(learnedlist[[1]])){
      log_debug(object = logger, message = "Evaluating tuned models", sep = "\n", add.level = TRUE, add.time = TRUE)

      #reshape to have a list where each element corresponds to a different configuration
      models = do.call(what = Map, args = c(f = c, learnedlist))

      #get trained
      models  = lapply(X = models, FUN = TunedList)

      #evaluate
      out = lapply(
        X = models,
        FUN = evaluate,
        # evaluate(
        # models = models,
        learner = learner,
        evaluator = evaluator,
        # hyperparameters,
        # #Sample
        # sampler = sampler,
        # looper = looper,
        #Test
        # forecaster = get_forecaster(learner),
        # scorer = get_scorer(learner),
        # tester,
        x = x,
        y = y,
        weights = weights,
        offset = offset,
        # resp.type,
        observations = observations,
        # train.size,
        logger = logger,
        resp.type = resp.type
        # indices = itrain,
        # screened = NULL,
        # config = character()
      )

      #remove names
      names(out) = NULL

      #Clean tuned object to reduce space in memory
      out = lapply(X = out, FUN = clean, rm.call = rm.call, rm.fit = rm.fit)

    } else {
      stop("Something is wrong with the trained models: their class is not 'Trained' nor 'TunedList'. Please check your data.\n")
    }

    #--------------------------------------------------------------------------------------------#
    #save
    if(saveRes){
      log_debug(object = logger, message = "Saving the results.", sep = "\n", add.level = TRUE, add.time = TRUE)

      #Save locally
      saveRDS(object = out, file = file.path(outdir, paste0(filename, ".rds")));

      #Clean trained object to reduce space in memory
      # out = clean(object = out, rm.call = rm.call, rm.fit = rm.fit)
    }

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  })


#'@param models an object of class \linkS4class{TrainedList}
#'@rdname evaluate
methods::setMethod(
  f = "evaluate",
  #signature = methods::signature(models = "TrainedList", learner = "Learner", hyperparameters = "ANY", sampler = "Sampler"),
  signature = methods::signature(models = "TrainedList", learner = "Learner", evaluator = "Evaluator", npoints = "missing"),
  definition = function(
    models,
    learner,
    evaluator,
    #Sample
    # sampler,
    # looper = Looper(),
    #Test
    x,
    y,
    weights,
    offset,
    resp.type,
    observations = NULL,
    # train.size,
    logger,
    # indices,
    screened = NULL,
    config = character(),
    ...){

    #--------------------------------------------------------------------------------------------#
    if(missing(logger)){logger = get_logger(evaluator)}
    logger = open_con(logger)
    scorer = get_scorer(evaluator)
    #--------------------------------------------------------------------------------------------#
    #Number of features/observations
    nfeats = ncol(x)
    nobs = nrow(x)

    #--------------------------------------------------------------------------------------------#
    #indices
    obs   = seq(nobs)
    feats = seq(nfeats)

    #--------------------------------------------------------------------------------------------#
    #stability
    log_debug(object = logger, message = "Recording features presence...", sep = "", add.level = TRUE, add.time = TRUE)
    #Record presence of feature in trained models
    # recordedl = lapply(X = unlist(models), FUN = record, recorder = get_recorder(learner), features = colnames(x))
    recordedl = lapply(X = unlist(models), FUN = record, recorder = get_recorder(learner), features = colnames(x, do.NULL = F, prefix = "V"))#create column names if missing
    #reshape to have one element per response
    recordedl = do.call(what = Map, args = c(f = cbind, recordedl))
    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    #stability
    log_debug(object = logger, message = "Computing features stability...", sep = "", add.level = TRUE, add.time = TRUE)
    stability = lapply(X = recordedl, FUN = rowMeans, na.rm = T)
    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    rm(recordedl)

    #--------------------------------------------------------------------------------------------#
    #create indices for testing data
    itest = lapply(X = observations, FUN = setdiff, x = seq(nobs))

    indices = list(train = observations, test = itest, full = lapply(X = rep(nobs, length(observations)), FUN = seq))

    #--------------------------------------------------------------------------------------------#
    #reshape to have a list where each element corresponds to a different configuration
    # models = do.call(what = Map, args = c(f = c, models))

    #--------------------------------------------------------------------------------------------#
    #Force TrainedList
    # models = lapply(X = trainedlist, FUN = TrainedList)

    #--------------------------------------------------------------------------------------------#
    #--------------------------------------------------------------------------------------------#
    out = list()
    for(i in seq(length(indices))){

      nm = names(indices)[i]

      log_debug(object = logger, message = paste("Assessing the models on",nm, "data"), sep = "\n", add.level = TRUE, add.time = TRUE)
      testedlist = test(
        models = models,
        # tester     = tester,
        forecaster = get_forecaster(learner),
        scorer     = scorer,
        indices    = indices[[i]],
        screened   = screened,
        #prediction
        newx       = x,
        newoffset  = offset,
        #test
        newy       = y,
        weights    = weights,
        score.type = c("mean_score", "summary_score", "score", "score_and_summary")[4],
        min.obs    = 0,
        # multi    = c("average", "sum"), grouped = TRUE,#passed by scorer
        #other
        looper     = get_looper(evaluator),
        logger     = logger)

      log_debug(object = logger, message = paste("Selecting best models"), sep = "\n", add.level = TRUE, add.time = TRUE)
      outlist = list()
      for(j in seq(length(scorer))){
        selected = select(
          selector = get_selector(learner),
          models   = models,
          merr     = testedlist[[j]]$s,
          sderr    = rep(x = testedlist[[j]]$sem, times = length(testedlist[[j]]$s)),
          scorer   = scorer[[j]]
        )

        outlist[[j]] = Tested(
          set     = nm,
          measure = get_id(scorer[[j]]),
          score   = testedlist[[j]]$s,
          mscore  = testedlist[[j]]$m,
          sem     = testedlist[[j]]$sem,
          ci      = testedlist[[j]]$ci,
          opt     = selected$opt,
          `1se`   = selected$`1se`
        )
      }


      log_trace(object = logger, message = paste("Storing"), sep = "\n", add.level = TRUE, add.time = TRUE)
      # out[[nm]] = TestedList(outlist)
      out = c(out, outlist)

    }

    #--------------------------------------------------------------------------------------------#
    #assess
    log_trace(object = logger, message = paste("Creating output object..."), sep = "", add.level = TRUE, add.time = TRUE)
    out = Evaluated(
      # id       = get_learning_method(learnedlist[[1]]),
      id           = get_id(get_trainer(learner)),
      config       = config,
      response     = resp.type,
      nout         = get_num_of_responses(y = y, resp.type = resp.type),
      sampling     = get_method(get_sampler(evaluator)),
      # size         = get_train_set_size(sampler),
      N            = get_N(get_sampler(evaluator)),
      n            = get_train_set_size(get_sampler(evaluator)),
      k            = get_k(get_sampler(evaluator)),
      observations = observations,
      models       = unlist(models),
      stability    = stability,
      performance  = TestedList(out)
    )
    log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  })

#'@param models an object of class \linkS4class{TunedList}
#'@rdname evaluate
methods::setMethod(
  f = "evaluate",
  signature = methods::signature(models = "TunedList", learner = "Learner", evaluator = "Evaluator", npoints = "missing"),
  definition = function(
    models,
    learner,
    evaluator,
    ...){


    # #--------------------------------------------------------------------------------------------#
    # if(missing(logger)){logger = get_logger(learner)}
    # logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #get config
    configs = unique(get_config(models))

    #Check config
    config = ifelse( test = (length(configs) == 1), yes = configs, no = "mixed")

    #--------------------------------------------------------------------------------------------#
    #get trained
    trainedlist  = TrainedList(get_model(models))
    #get screened
    screenedlist = get_screened(models)

    #--------------------------------------------------------------------------------------------#
    #evaluate
    out = evaluate(evaluator = evaluator, models = trainedlist, screened = screenedlist, config = config, learner = learner, ...)

    #--------------------------------------------------------------------------------------------#
    #update
    out = set_models(out, unlist(models))

    #--------------------------------------------------------------------------------------------#
    #close
    # close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  })


#'Evaluate Learning Method
#'@param ngrid (optional) grid of sample sizes to consider
#'@param nmin minimum sample size (i.e. number of samples to use for training)
#'@param npoints number of sample sizes to consider (i.e. number of training set sizes in the grid)
#'@param rm.call logical, whether to remove the call from the models. Helpful if object size is expected to be big
#'@param rm.fit logical, whether to remove the model fits used for tuning the hyperparameters . Helpful if object size is expected to be big
#'object can be huge.
#'
#'@rdname evaluate
methods::setMethod(
  f = "evaluate",
  # signature = methods::signature(models = "missing", learner = "Learner", hyperparameters = "ANY", sampler = "Sampler"),
  # signature = methods::signature(evaluator = "Evaluator", models = "missing", learner = "Learner", npoints = "numeric"),
  signature = methods::signature(models = "missing", learner = "ANY", evaluator = "Evaluator", npoints = "numeric"),
  definition = function(
    learner,
    evaluator,
    #training set size grid
    npoints = 3,
    ngrid,
    nmin = round(nrow(x)/2),
    x,
    looper = Looper(),
    logger,
    #output
    outdir   = NULL,
    filename = "renoir",
    restore = T,
    #space
    rm.call = FALSE,
    rm.fit  = FALSE,
    #further args
    ...){

    #--------------------------------------------------------------------------------------------#
    if(missing(logger)){logger = get_logger(evaluator)}
    logger = open_con(logger)
    #--------------------------------------------------------------------------------------------#
    #check
    saveRes = check_output_dir(outdir);

    #--------------------------------------------------------------------------------------------#
    #check
    if(missing(ngrid)){
      #create grid
      log_info(object = logger, message = paste("Creating grid of sample sizes..."), sep = "", add.level = TRUE, add.time = TRUE)
      ngrid = get_grid(object = get_sampler(evaluator), nmin = nmin, npoints = npoints)
      log_info(object = logger, message = paste("DONE:", paste0(ngrid, collapse = ", ")), sep = "\n", add.level = F, add.time = F)
    }

    #--------------------------------------------------------------------------------------------#
    #Loop over grid
    outlist = loop(
      looper = looper,
      n.iter = length(ngrid),
      .inorder = TRUE,
      fun = get_sample_evaluated,
      logger    = logger,
      learner   = learner,
      evaluator = evaluator,
      ngrid      = ngrid,
      outdir     = outdir,
      filename   = filename,
      restore    = restore,
      x          = x,
      rm.call    = rm.call,
      rm.fit     = rm.fit,
      ...
      )

    #--------------------------------------------------------------------------------------------#
    #If output directory was provided, the objects for each training set size were
    #stored to reduce occupied memory size while computing.

    #Create the output object
    if(saveRes){

      log_info(object = logger, message = paste("Loading data..."), sep = "\n", add.level = TRUE, add.time = TRUE)
      outlist = loop(
        looper = looper,
        n.iter = length(ngrid),
        .inorder = TRUE,
        fun = function(iloop, ngrid, outdir, filename, rm.call = T, rm.fit = T, ilogger){
          ilogger = open_con(ilogger)

          #Set training set size
          isize = ngrid[iloop];
          log_trace(object = ilogger, message = paste("Training set size", isize, "..."), sep = "", add.level = TRUE, add.time = TRUE)
          #Get the file path
          filename = create_filename(filename, isize)
          filepath = file.path(outdir, paste0(filename, ".rds"))
          #Read the file
          out = readRDS(file = filepath);
          log_trace(object = ilogger, message = paste("DONE"), sep = "\n", add.level = F, add.time = F)

          # #Clean trained object to reduce space in memory
          # if(rm.fit || rm.call){
          #   log_trace(object = ilogger, message = paste("Cleaning object to save space in memory..."), sep = "", add.level = TRUE, add.time = TRUE)
          #   out = clean(object = out, rm.fit = rm.fit, rm.call = rm.call)
          #   log_trace(object = ilogger, message = paste("DONE"), sep = "\n", add.level = F, add.time = F)
          # }

          close_con(ilogger)
          #return
          return(out)
        }, ngrid = ngrid, outdir = outdir, filename = filename, rm.call = rm.call, rm.fit = rm.fit, ilogger = logger)


    }
    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    return(outlist)
})


#'@keywords internal
get_sample_evaluated = function(iloop, logger, learner, evaluator, ngrid, outdir, filename, restore = T, ...){
  #--------------------------------------------------------------------------------------------#
  #Set logger
  if(missing(logger)){logger = get_logger(evaluator)}
  logger = open_con(logger)

  #--------------------------------------------------------------------------------------------#
  #check outdir
  hasOutdir = check_output_dir(outdir);

  #--------------------------------------------------------------------------------------------#
  #set default
  out = list()

  #--------------------------------------------------------------------------------------------#
  #get size
  isize = ngrid[iloop]
  #log
  log_info(object = logger, message = paste(iloop, "Considering size:", isize), sep = "\n", add.level = TRUE, add.time = TRUE)

  #--------------------------------------------------------------------------------------------#
  #Create the filename using our internal rule
  filename = create_filename(filename, isize)
  if(hasOutdir) {filepath = file.path(outdir, paste0(filename, ".rds"))}else{filepath=NULL}

  #If 'restore' flag is active, check if result file is in outdir,
  #skip analysis in this case
  if(restore && !is.null(filepath) && file.exists(filepath) && file.size(filepath) > 0){
    log_info(object = logger, message = "Analysis SKIPPED for this training set size: a results file found in the output directory.", sep = "\n", add.level = TRUE, add.time = TRUE)
  } else {

    #update sampler
    ievaluator = set_sampler(object = evaluator, value = set_size(get_sampler(evaluator), isize))

    #learn
    out = evaluate(
      learner   = learner,
      evaluator = ievaluator,
      # x,
      # y,
      # weights,
      # offset,
      # resp.type,
      # # observations = NULL,
      # # train.size,
      logger   = logger,
      outdir   = outdir,
      filename = filename,
      ...)
  }

  #If output directory is provided, the object returned from training and testing was
  #stored. To improve memory efficiency we adopted the following strategy:
  #a) if object was stored on disk, don't keep in memory for the moment but read
  # all the results in the end
  #b) if output directory wasn't provided, keep object in memory (an alternative could be to create
  #a temporary directory)
  if(hasOutdir){
    out = list()
  }

  #--------------------------------------------------------------------------------------------#
  #close
  close_con(logger)

  #--------------------------------------------------------------------------------------------#
  return(out)
}

#'Evaluate Learning Methods
#'@param looper a \linkS4class{Looper} to loop over the learning methods to evaluate
#'
#'@rdname evaluate
methods::setMethod(
  f = "evaluate",
  # signature = methods::signature(models = "missing", learner = "Learner", hyperparameters = "ANY", sampler = "Sampler"),
  signature = methods::signature(models = "missing", learner = "LearnerList", evaluator = "Evaluator", npoints = "missing"),
  definition = function(
    learner,
    evaluator,
    # hyperparameters,
    x,
    # y,
    # weights,
    # offset,
    # resp.type,
    # observations = NULL,
    # train.size,
    looper = Looper(),
    logger,
    # outdir,
    # filename,
    ...){


    measures = get_id(get_scorer(evaluator))
    pred.types = sapply(X = ForecasterList(get_forecaster(learner)), FUN = get_prediction_type, measures)
    # pred.types = sapply(X = measures, FUN = get_prediction_type, object = ForecasterList(get_forecaster(learner)))
    if(anyNA(pred.types)){
      #check each learner
      pred.types = lapply(X = ForecasterList(get_forecaster(learner)), FUN = get_prediction_type, measures)
      #NA
      isNA = sapply(X = pred.types, FUN = anyNA)
      #measures
      measures = measures[unique(unlist(lapply(X = lapply(X = pred.types, FUN = is.na), FUN = which)))]
      msg <- paste("At least one 'scorer' (",paste0(measures, collapse = ", "),
                   ") not supported by at least one forecaster in 'learner' (",
                   paste0(names(isNA)[isNA], collapse = ", "),
                   "). Please check the 'learner' or change the 'evaluator' ", sep = "")
      errors <- c(errors, msg)
    }

    #--------------------------------------------------------------------------------------------#
    if(missing(logger)){logger = get_logger(evaluator)}
    logger = open_con(logger)
    #--------------------------------------------------------------------------------------------#
    #Check input
    sampler = get_sampler(evaluator)
    # looper  = get_looper(evaluator)
    #--------------------------------------------------------------------------------------------#
    #check
    # saveRes = check_output_dir(outdir);
    # if(saveRes && missing(filename)){
    #   warning("Output directory was provided but 'filename' is missing without default. Data won't be saved.\n")
    #   saveRes = FALSE
    # }

    #--------------------------------------------------------------------------------------------#
    #Number of features/observations
    nfeats = ncol(x)
    nobs = nrow(x)

    #--------------------------------------------------------------------------------------------#
    #Samples
    #check N
    if(S4Vectors::isEmpty(get_N(sampler))){sampler = set_N(sampler, nobs)}
    #check
    is.valid.sampler = is_valid_sampler_object(sampler)
    if(!isTRUE(is.valid.sampler)){stop(paste("Sampler is not valid.\n",is.valid.sampler,"\n"))}

    #Get samples
    itrain = resample(object = sampler)

    #--------------------------------------------------------------------------------------------#
    #indices
    obs   = seq(nobs)
    feats = seq(nfeats)

    #--------------------------------------------------------------------------------------------#
    #Learn
    log_debug(object = logger, message = paste("Evaluating", length(learner), "learning methods"), sep = "\n", add.level = TRUE, add.time = TRUE)
    out = loop(
      looper = looper,
      n.iter = length(learner),
      .inorder = F,
      fun = get_ievaluated,
      learners = learner,
      evaluator = evaluator,
      observations = itrain,
      x = x,
      # y = y,
      # weights = weights,
      # offset = offset,
      # resp.type = resp.type,
      # hyperparameters = hyperparameters,
      ...
    )

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  })


#Loop over learners
#'@keywords internal
get_ievaluated <- function(iloop, logger, learners, evaluator, ...){

  #--------------------------------------------------------------------------------------------#
  if(missing(logger)){logger = get_logger(evaluator)}
  logger = open_con(logger)

  #--------------------------------------------------------------------------------------------#
  log_trace(object = logger, message = paste("Evaluating learning method", iloop,"of", paste0(length(learners), ":"),
                                             get_id(learners[[iloop]])), sep = "\n", add.level = TRUE, add.time = TRUE)
  out = evaluate(
    learner   = learners[[iloop]],
    evaluator = evaluator,
    logger = logger,
    ...
  )

  #--------------------------------------------------------------------------------------------#
  #close
  close_con(logger)

  #--------------------------------------------------------------------------------------------#
  return(out)
}


#@param observations indices indicating a subset of original data
#@param samples list of integer, indicating the subset of \code{observations} used for training
#@param auto.select logical, whether auto select best model
#'@param models an object of class \linkS4class{TrainedList}
#'
#'@rdname evaluate
methods::setMethod(
  f = "evaluate",
  #signature = methods::signature(models = "TrainedList", learner = "Learner", hyperparameters = "ANY", sampler = "Sampler"),
  signature = methods::signature(models = "TrainedList", learner = "missing", evaluator = "Evaluator", npoints = "missing"),
  definition = function(
    models,
    learner,
    evaluator,
    recorder,

    #Sample
    # sampler,
    # looper = Looper(),
    #Test
    x,
    y,
    weights,
    offset,
    resp.type,
    samples      = NULL,
    observations = NULL,
    # train.size,
    logger,
    # indices,
    screened = NULL,
    config = character(),

    #Evaluate
    set = c("test", "train", "full"),
    min.obs    = 0,
    auto.select = F,
    # grade = F,
    ...){

    #--------------------------------------------------------------------------------------------#
    if(missing(logger)){logger = get_logger(evaluator)}
    logger = open_con(logger)
    scorer = get_scorer(evaluator)
    #--------------------------------------------------------------------------------------------#
    #Number of features/observations
    nfeats = ncol(x)
    nobs = nrow(x)

    #--------------------------------------------------------------------------------------------#
    #indices
    if(S4Vectors::isEmpty(observations)){observations = seq(nobs)}
    feats = seq(nfeats)

    #--------------------------------------------------------------------------------------------#
    #stability
    log_debug(object = logger, message = "Recording features presence...", sep = "", add.level = TRUE, add.time = TRUE)
    #Record presence of feature in trained models
    # recordedl = lapply(X = unlist(models), FUN = record, recorder = get_recorder(learner), features = colnames(x))
    recordedl = lapply(X = unlist(models), FUN = record, recorder = get_recorder(learner), features = colnames(x, do.NULL = F, prefix = "V"))#create column names if missing
    #reshape to have one element per response
    recordedl = do.call(what = Map, args = c(f = cbind, recordedl))
    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    #stability
    log_debug(object = logger, message = "Computing features stability...", sep = "", add.level = TRUE, add.time = TRUE)
    stability = lapply(X = recordedl, FUN = rowMeans, na.rm = T)
    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    rm(recordedl)

    #--------------------------------------------------------------------------------------------#
    #create indices for testing data
    indices = list()
    if("train" %in% set){indices = c(indices, list(train = samples))}
    if("test" %in% set){ indices = c(indices, list(test = lapply(X = samples, FUN = setdiff, x = observations)))}
    if("full" %in% set){ indices = c(indices, list(full = lapply(X = seq(length(samples)), FUN = function(x){return(observations)})))}

    #--------------------------------------------------------------------------------------------#
    #reshape to have a list where each element corresponds to a different configuration
    # models = do.call(what = Map, args = c(f = c, models))

    #--------------------------------------------------------------------------------------------#
    #Force TrainedList
    # models = lapply(X = trainedlist, FUN = TrainedList)

    #--------------------------------------------------------------------------------------------#
    #--------------------------------------------------------------------------------------------#
    out = list()
    for(i in seq(length(indices))){

      nm = names(indices)[i]

      log_debug(object = logger, message = paste("Assessing the models on",nm, "data"), sep = "\n", add.level = TRUE, add.time = TRUE)
      testedlist = test(
        models = models,
        # tester     = tester,
        forecaster = get_forecaster(learner),
        scorer     = scorer,
        indices    = indices[[i]],
        screened   = screened,
        #prediction
        newx       = x,
        newoffset  = offset,
        #test
        newy       = y,
        weights    = weights,
        score.type = c("mean_score", "summary_score", "score", "score_and_summary")[4],
        min.obs    = min.obs,
        # multi    = c("average", "sum"), grouped = TRUE,#passed by scorer
        #other
        looper     = get_looper(evaluator),
        logger     = logger)

      if(auto.select){log_debug(object = logger, message = paste("Selecting best models"), sep = "\n", add.level = TRUE, add.time = TRUE)}
      outlist = list()
      for(j in seq(length(scorer))){
        if(auto.select) {
          selected = select(
            selector = get_selector(learner),
            models   = models,
            merr     = testedlist[[j]]$s,
            sderr    = rep(x = testedlist[[j]]$sem, times = length(testedlist[[j]]$s)),
            scorer   = scorer[[j]]
          )
        } else{selected = NULL}


        outlist[[j]] = Tested(
          set     = nm,
          measure = get_id(scorer[[j]]),
          score   = testedlist[[j]]$s,
          mscore  = testedlist[[j]]$m,
          sem     = testedlist[[j]]$sem,
          ci      = testedlist[[j]]$ci,
          opt     = selected$opt,
          `1se`   = selected$`1se`
        )
      }


      log_trace(object = logger, message = paste("Storing"), sep = "\n", add.level = TRUE, add.time = TRUE)
      # out[[nm]] = TestedList(outlist)
      out = c(out, outlist)

    }

    #--------------------------------------------------------------------------------------------#
    #assess
    log_trace(object = logger, message = paste("Creating output object..."), sep = "", add.level = TRUE, add.time = TRUE)
    out = Evaluated(
      # id       = get_learning_method(learnedlist[[1]]),
      id           = get_id(get_trainer(learner)),
      config       = config,
      response     = resp.type,
      nout         = get_num_of_responses(y = y, resp.type = resp.type),
      sampling     = get_method(get_sampler(evaluator)),
      N            = get_N(get_sampler(evaluator)),
      n            = get_train_set_size(get_sampler(evaluator)),
      k            = get_k(get_sampler(evaluator)),
      observations = observations,
      models       = unlist(models),
      stability    = stability,
      performance  = TestedList(out)
    )
    log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  })
