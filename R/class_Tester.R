#' @include classes_generics.R class_Logger.R class_Looper.R class_Scorer.R class_ScorerList.R class_TunedList.R class_TrainedList.R class_ForecasterList.R
NULL

#' Tester Class
#' An S4 class providing the methods to test the trained models on provided data.
#'
#' The object consists of 6 slots
#' @slot forecaster a \linkS4class{Forecaster}
#' @slot scorer a \linkS4class{Scorer}
#' @slot looper a \linkS4class{Looper}
#' @slot logger a \linkS4class{Logger}
#' @author Alessandro Barberis
methods::setClass(
  Class = "Tester",
  slots = c(
    forecaster = "Forecaster",
    scorer     = "Scorer",
    # selector = "function",
    # looper   = "Looper",
    logger   = "Logger"
  )
)

#' Constructor for the S4 Tester object.
#'
#' Constructor for the S4 \linkS4class{Tester} object.
#'
#' @param forecaster a \linkS4class{ForecasterList}
#' @param scorer a \linkS4class{ScorerList}
#' @param looper a \linkS4class{Looper}
#' @param logger a \linkS4class{Logger}
Tester <- function(
  forecaster,
  scorer,
  # selector,
  # looper   = Looper(),
  logger   = Logger()
){

  # if(missing(prediction_fun)){
  #   prediction = get_prediction_function()
  # } else {
  #   user_prediction_fun = prediction_fun
  #
  #   prediction_fun = function(learning.method, ...){
  #     if(learning.method %in% supported_learning_methods()){
  #       out = prediction_function(...)
  #     } else {
  #       out = user_prediction_fun(...)
  #     }
  #     return(out)
  #   }
  # }

  #check forecaster supports scorer
  check_prediction_type(forecaster = forecaster, scorer = scorer)

  #create object
  methods::new(
    Class = "Tester",
    forecaster = forecaster,
    scorer     = scorer,
    # selector = selector,
    # looper     = Looper(),
    logger     = Logger()
  )
}

methods::setMethod(f = "get_forecaster", signature = "Tester", definition = function(object){methods::slot(object = object, name = 'forecaster')})
methods::setMethod(f = "get_scorer",     signature = "Tester", definition = function(object){methods::slot(object = object, name = 'scorer')})
# methods::setMethod(f = "get_looper",     signature = "Tester", definition = function(object){methods::slot(object = object, name = 'looper')})
methods::setMethod(f = "get_logger",     signature = "Tester", definition = function(object){methods::slot(object = object, name = 'logger')})
methods::setMethod(f = "get_measure",    signature = "Tester", definition = function(object){get_id(get_scorer(object))})


# methods::setMethod(f = "get_prediction", signature = "Tester", definition = function(object){methods::slot(object = object, name = 'prediction')})

is_valid_tester_object <- function(object) {
  errors <- character()

  pred.type = get_prediction_type(object = get_forecaster(object), type.measure = get_id(get_scorer(object)))

  if (is.null(pred.type) || is.na(pred.type)) {
    msg <- paste("Prediction type selected by 'forecaster' for 'scorer'", get_id(get_scorer(object)),
                 "is NULL. Please check the 'forecaster' or change the 'scorer'", sep = "")
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}

methods::setValidity(Class = "Tester", method = is_valid_tester_object)


check_prediction_type <- function(forecaster, scorer){
  #Get prediction type for accuracy measure
  pred.type = get_prediction_type(object = forecaster, type.measure = get_id(scorer))

  #check
  if(is.null(pred.type)){
    stop("Prediction type needed by 'scorer' to compute the accuracy measure is not provided by 'forecaster'. Please check the 'forecaster' or change the 'scorer'.\n")
  }

}

methods::setMethod(
  f = "test",
  signature = methods::signature(models = "TrainedList", tester = "Tester"),
  definition = function(models, tester, ...){
    #Check models list
    isok = all( unique(unlist(get_learning_method(models))) %in% unique(unlist(get_id(get_forecaster(tester)))) )

    if(!isok){
      missingid = setdiff(x = unique(unlist(get_learning_method(models))), y = unique(unlist(get_id(get_forecaster(tester)))))

      stop(paste("Missing Forecaster for the following learning methods found in the 'models' list:", paste0(missingid, collapse = ", "), "\n"))
    } else {

      out = lapply(X = models, FUN = test, tester = tester, ...)

    }

    #return
    return(out)
})

methods::setMethod(
  f = "test",
  signature = methods::signature(models = "Trained", tester = "Tester", forecaster = "missing", scorer = "missing", indices = "missing"),
  definition = function(
    models,
    tester,
    logger,
    ...){

    #--------------------------------------------------------------------------------------------#
    #logger
    if(missing(logger)){logger = get_logger(tester)}

    #--------------------------------------------------------------------------------------------#
    #scorer
    scorer = get_scorer(tester)

    #--------------------------------------------------------------------------------------------#
    #forecaster
    forecaster = get_forecaster(tester)

    #--------------------------------------------------------------------------------------------#
    #test
    out = test(models = models, forecaster = forecaster, scorer = scorer, logger = logger, ...)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)


methods::setMethod(
  f = "test",
  signature = methods::signature(models = "Trained", tester = "missing", forecaster = "Forecaster", scorer = "Scorer", indices = "missing"),
  definition = function(
    models,
    forecaster,
    scorer,
    newx,
    newoffset,
    # type,
    newy,
    weights = NULL,
    score.type = c("mean_score", "summary_score", "score", "score_and_summary"),
    min.obs = 3,
    logger,
    ...){

    #--------------------------------------------------------------------------------------------#
    #Check forecaster is matching trained
    isok = all(get_learning_method(models) %in% get_id(forecaster))
    if(!isok){
      stop(paste("Forecaster is not matching learning method of 'models'.\nFound:", get_id(forecaster), ", expected:",get_learning_method(models),"\n"))
    }

    #--------------------------------------------------------------------------------------------#
    #logger
    if(missing(logger)){logger = get_logger(tester)}
    #open connection
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #match
    score.type = match.arg(score.type)

    #--------------------------------------------------------------------------------------------#
    #extract all scores
    measure = unlist(get_id(scorer))

    #--------------------------------------------------------------------------------------------#
    #for each required score select the needed prediction type
    log_trace(object = logger, message = paste("Check prediction type"), sep = "\n", add.level = TRUE, add.time = TRUE)
    pred.type = get_prediction_type(object = forecaster, type.measure = measure)

    #--------------------------------------------------------------------------------------------#
    #predict
    log_info(object = logger, message = paste("Computing prediction..."), sep = "", add.level = TRUE, add.time = TRUE)
    pred = forecast(
      forecaster = forecaster,
      models = models,
      newx = newx,
      type = pred.type,
      newoffset = newoffset,
      ...)
    log_info(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #-------------------------------------------------------------------------------------------#
    #Compute score estimate and standard error
    log_info(object = logger, message = paste("Computing accuracy"), sep = "\n", add.level = TRUE, add.time = TRUE)

    out = switch(
      score.type,
      'summary_score'     = summary_score(scorer = scorer, true = newy, pred = pred, weights = weights, min.obs = min.obs),
      'mean_score'        = mean_score(   scorer = scorer, true = newy, pred = pred, weights = weights, min.obs = min.obs),
      'score'             = score(        scorer = scorer, true = newy, pred = pred, weights = weights, min.obs = min.obs),
      'score_and_summary' = summary_score(scorer = scorer, true = newy, pred = pred, weights = weights, min.obs = min.obs, return.raw = TRUE)
    )

    #--------------------------------------------------------------------------------------------#
    #close connection
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)


methods::setMethod(
  f = "test",
  signature = methods::signature(models = "Trained", tester = "missing", forecaster = "Forecaster", scorer = "ANY", indices = "numeric"),
  definition = function(
    models,
    forecaster,
    scorer,
    newx,
    newoffset,
    # type,
    newy,
    weights = NULL,
    indices,
    ...){

    #subset
    newx      = subset_observations(object = newx, which = indices)
    newoffset = subset_observations(object = newoffset, which = indices)
    newy      = subset_observations(object = newy, which = indices)
    weights   = subset_observations(object = weights, which = indices)

    out = test(models = models, forecaster = forecaster, scorer = scorer, newx = newx, newoffset = newoffset, newy = newy, weights = weights,
               ...)

    return(out)
  }
)

methods::setMethod(
  f = "test",
  signature = methods::signature(models = "Trained", tester = "missing", forecaster = "Forecaster", scorer = "ScorerList", indices = "missing"),
  definition = function(
    models,
    forecaster,
    scorer,
    newx,
    newoffset,
    # type,
    newy,
    weights = NULL,
    score.type = c("mean_score", "summary_score", "score", "score_and_summary"),
    min.obs = 3,
    logger = Logger(verbose = FALSE),
    ...){

    #--------------------------------------------------------------------------------------------#
    #Check forecaster is matching trained
    isok = all(get_learning_method(models) %in% get_id(forecaster))
    if(!isok){
      stop(paste("Forecaster is not matching learning method of 'models'.\nFound:", get_id(forecaster), ", expected:",get_learning_method(models),"\n"))
    }

    #--------------------------------------------------------------------------------------------#
    #open connection
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #match
    # multi = match.arg(multi)
    score.type = match.arg(score.type)

    #--------------------------------------------------------------------------------------------#
    #extract all scores
    measures = unlist(get_id(scorer))

    #--------------------------------------------------------------------------------------------#
    #for each required score select the needed prediction type
    log_trace(object = logger, message = paste("Check prediction type"), sep = "\n", add.level = TRUE, add.time = TRUE)
    pred.types = list()
    # measure = measures[1]
    for(measure in measures){
      pred.types[[measure]] = stats::setNames(
        object = get_prediction_type(object = forecaster, type.measure = measure, learning.method = get_learning_method(models)),
        nm = NULL)
    }

    #--------------------------------------------------------------------------------------------#
    #check dupli
    is.dupli = duplicated(pred.types)
    #rm
    pred.types.no.dupli = pred.types[!is.dupli]

    #--------------------------------------------------------------------------------------------#
    #predict
    log_info(object = logger, message = paste("Computing prediction..."), sep = "", add.level = TRUE, add.time = TRUE)
    predlist = list()
    i=1
    for(i in seq(length(pred.types.no.dupli))){
      predlist[[i]] = forecast(
        forecaster = forecaster,
        models = models,
        newx = newx,
        type = pred.types.no.dupli[[i]],
        newoffset = newoffset,
        ...)
    }
    log_info(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#
    #reshape and order by measure
    ipred = match(x = pred.types, table = pred.types.no.dupli)

    #-------------------------------------------------------------------------------------------#
    #Compute score estimate and standard error
    log_info(object = logger, message = paste("Computing accuracy"), sep = "\n", add.level = TRUE, add.time = TRUE)

    out = list()
    for(i in seq(length(predlist))){
      out = c(
        out,
        switch(
          score.type,
          'summary_score'     = summary_score(scorer = scorer[which(ipred == i)], true = newy, pred = predlist[[i]], weights = weights, min.obs = min.obs),
          'mean_score'        = mean_score(   scorer = scorer[which(ipred == i)], true = newy, pred = predlist[[i]], weights = weights, min.obs = min.obs),
          'score'             = score(        scorer = scorer[which(ipred == i)], true = newy, pred = predlist[[i]], weights = weights, min.obs = min.obs),
          'score_and_summary' = summary_score(scorer = scorer[which(ipred == i)], true = newy, pred = predlist[[i]], weights = weights, min.obs = min.obs, return.raw = TRUE)
        )
      )
    }

    #--------------------------------------------------------------------------------------------#
    #close connection
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

methods::setMethod(
  f = "test",
  signature = methods::signature(models = "TrainedList", tester = "missing", forecaster = "Forecaster", scorer = "ScorerList", indices = "missing"),
  definition = function(
    models,
    forecaster,
    scorer,
    logger,
    ...){

    #--------------------------------------------------------------------------------------------#
    #Check trained list
    isok = all( unique(unlist(get_learning_method(models))) %in% unique(unlist(get_id(forecaster))) )

    if(!isok){
      missingid = setdiff(x = unique(unlist(get_learning_method(models))), y = unique(unlist(get_id(forecaster))))
      stop(paste("Learning methods found in the 'models' list are different from 'forecaster':", paste0(missingid, collapse = ", "), "\n"))
    } else {
      out = lapply(X = models, FUN = test, forecaster = forecaster, scorer = scorer, logger = logger, ...)
    }

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

methods::setMethod(
  f = "test",
  signature = methods::signature(models = "TrainedList", tester = "missing", forecaster = "Forecaster", scorer = "Scorer", indices = "missing"),
  definition = function(
    models,
    forecaster,
    scorer,
    logger,
    ...){

    #--------------------------------------------------------------------------------------------#
    #Check trained list
    isok = all( unique(unlist(get_learning_method(models))) %in% unique(unlist(get_id(forecaster))) )

    if(!isok){
      missingid = setdiff(x = unique(unlist(get_learning_method(models))), y = unique(unlist(get_id(forecaster))))
      stop(paste("Learning methods found in the 'models' list are different from 'forecaster':", paste0(missingid, collapse = ", "), "\n"))
    } else {
      out = lapply(X = models, FUN = test, forecaster = forecaster, scorer = scorer, logger = logger, ...)
    }

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

#'Test
#'@description Tests the trained model on the provided data
#'@param tester a \linkS4class{Tester} object
#'@param models a list of \linkS4class{TrainedList} objects
#'@param indices list of indices used to subset x, y, weights, offset when training the model
#'@param screened list of screened features
#'@param newx the predictors matrix
#'@param newoffset offset
#@param type prediction type
#'@param newy the response
#'@param weights observations weights (i.e. response prior)
#'@param looper a \linkS4class{Looper} object
#'@param logger a \linkS4class{Logger} object
#'@param score.type the score type to return, one of
#'\describe{
#'   \item{\code{score}}{the accuracy measure}
#'   \item{\code{mean_score}}{estimate of the mean error obtained from the samples}
#'   \item{\code{summary_score}}{estimate of the mean error and its standard error}
#'}
#'@param ... further arguments to scorer function
#'@return list of 2 elements containing a score estimate and its standard error
methods::setMethod(
  f = "test",
  signature = methods::signature(models = "TrainedList", tester = "Tester", forecaster = "missing", scorer = "missing", indices = "list"),
  definition = function(
    #trained
    models,
    #tester
    tester,
    #resample
    indices,
    ...){

    #--------------------------------------------------------------------------------------------#
    #scorer
    scorer = get_scorer(tester)

    #--------------------------------------------------------------------------------------------#
    #forecaster
    forecaster = get_forecaster(tester)

    #--------------------------------------------------------------------------------------------#
    #test
    out = test(models = models, forecaster = forecaster, scorer = scorer, indices = indices, ...)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)





methods::setMethod(
  f = "test",
  signature = methods::signature(models = "TrainedList", tester = "missing", forecaster = "Forecaster", scorer = "Scorer", indices = "list"),
  definition = function(
    #trained
    models,
    #tester
    # tester,
    forecaster,
    scorer,
    #resample
    indices,
    screened = NULL,
    #prediction
    newx,
    newoffset = NULL,
    # type,
    #test
    newy,
    weights = NULL,
    score.type = c("mean_score", "summary_score", "score", "score_and_summary"),
    min.obs = 3,
    # multi = c("average", "sum"), grouped = TRUE,#passed by scorer
    #other
    looper = Looper(),
    logger,

    check.input = FALSE,
    ...){


    #--------------------------------------------------------------------------------------------#

    if(check.input){
      #Check trained list
      isok = all( unique(unlist(get_learning_method(models))) %in% unique(unlist(get_id(forecaster))) )

      if(!isok){
        missingid = setdiff(x = unique(unlist(get_learning_method(models))), y = unique(unlist(get_id(forecaster))))

        stop(paste("Missing Forecaster for the following learning methods found in the 'models' list:", paste0(missingid, collapse = ", "), "\n"))
      }


      #Check length
      isok = length(models) == length(indices)
      if(!isok){stop("Length of 'models' different from length of 'indices'.\n")}
    }

    #--------------------------------------------------------------------------------------------#
    #logger
    if(missing(logger)){logger = get_logger(scorer)}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #match
    # multi = match.arg(multi)
    score.type = match.arg(score.type)

    #--------------------------------------------------------------------------------------------#
    #extract all scores
    measure = get_id(scorer)

    #--------------------------------------------------------------------------------------------#
    #for each required score select the needed prediction type
    log_trace(object = logger, message = paste("Check prediction type..."), sep = "", add.level = TRUE, add.time = TRUE)
    pred.type = get_prediction_type(object = forecaster, type.measure = measure)
    log_trace(object = logger, message = paste("DONE"), sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#
    #loop
    predlist = get_resample_predlist(
      trainedlist = models,
      newx        = newx,
      type        = pred.type,
      newoffset   = newoffset,
      looper      = looper,
      logger      = logger,
      indices     = indices,
      screened    = screened,
      forecaster  = forecaster
      ,...
    )

    #--------------------------------------------------------------------------------------------#
    #subset y
    newy    = lapply(X = indices, FUN = subset_observations_def, x = newy)
    weights = lapply(X = indices, FUN = subset_observations_def, x = weights)

    #--------------------------------------------------------------------------------------------#
    #Compute score estimate and standard error
    log_trace(object = logger, message = paste("Computing accuracy"), sep = "\n", add.level = TRUE, add.time = TRUE)

    out = switch(
      score.type,
      'summary_score'    = summary_score(scorer = scorer, true = newy, pred = predlist, weights = weights, min.obs = min.obs, logger = logger),
      'mean_score'       = mean_score(   scorer = scorer, true = newy, pred = predlist, weights = weights, min.obs = min.obs, logger = logger),
      'score'            = score(        scorer = scorer, true = newy, pred = predlist, weights = weights, min.obs = min.obs, logger = logger),
      'score_and_summary'= summary_score(scorer = scorer, true = newy, pred = predlist, weights = weights, min.obs = min.obs, logger = logger, return.raw = TRUE)
    )

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)

methods::setMethod(
  f = "test",
  signature = methods::signature(models = "TrainedList", tester = "missing", forecaster = "Forecaster", scorer = "ScorerList", indices = "list"),
  definition = function(
    #trained
    models,
    #tester
    # tester,
    forecaster,
    scorer,
    #resample
    indices,
    screened = NULL,
    #prediction
    newx,
    newoffset = NULL,
    # type,
    #test
    newy,
    weights = NULL,
    score.type = c("mean_score", "summary_score", "score", "score_and_summary"),
    min.obs = 3,
    # multi = c("average", "sum"), grouped = TRUE,#passed by scorer
    #other
    looper,
    logger,
    ...){

    #Check trained list
    isok = all( unique(unlist(get_learning_method(models))) %in% unique(unlist(get_id(forecaster))) )

    if(!isok){
      missingid = setdiff(x = unique(unlist(get_learning_method(models))), y = unique(unlist(get_id(forecaster))))

      stop(paste("Missing Forecaster for the following learning methods found in the 'models' list:", paste0(missingid, collapse = ", "), "\n"))
    }

    #--------------------------------------------------------------------------------------------#
    #Check length
    isok = length(models) == length(indices)
    if(!isok){stop("Length of 'models' different from length of 'indices'.\n")}

    #--------------------------------------------------------------------------------------------#
    #logger
    if(missing(logger)){logger = get_logger(scorer)}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #match
    # multi = match.arg(multi)
    score.type = match.arg(score.type)

    #--------------------------------------------------------------------------------------------#
    #extract learning methods
    learning.methods = unlist(get_learning_method(models))

    #--------------------------------------------------------------------------------------------#
    #extract all scores
    measures = unlist(get_id(scorer))

    #--------------------------------------------------------------------------------------------#
    #for each required score select the needed prediction type
    log_trace(object = logger, message = paste("Check prediction type..."), sep = "", add.level = TRUE, add.time = TRUE)
    pred.types = list()
    # measure = measures[1]
    for(measure in measures){
      # pred.types[[measure]] = get_prediction_type(object = forecaster, type.measure = measure, learning.method = learning.methods)#OLD, used for ForecasterList
      pred.types[[measure]] = stats::setNames(object = get_prediction_type(object = forecaster, type.measure = measure), nm = NULL)
    }

    #--------------------------------------------------------------------------------------------#
    #check dupli
    is.dupli = duplicated(pred.types)
    #rm
    pred.types.no.dupli = pred.types[!is.dupli]
    log_trace(object = logger, message = paste("DONE:",length(pred.types.no.dupli),"types needed"), sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#
    #loop
    predlist = list()
    i=1
    for(i in seq(length(pred.types.no.dupli))){
      predlist[[i]] = get_resample_predlist(
        trainedlist = models,
        newx        = newx,
        type        = pred.types.no.dupli[[i]],
        newoffset   = newoffset,
        looper      = looper,
        logger      = logger,
        indices     = indices,
        screened    = screened,
        forecaster  = forecaster
        ,...
      )
    }

    #--------------------------------------------------------------------------------------------#
    #reshape and order by measure
    ipred = match(x = pred.types, table = pred.types.no.dupli)

    #--------------------------------------------------------------------------------------------#
    #subset y
    newy    = lapply(X = indices, FUN = subset_observations_def, x = newy)
    weights = lapply(X = indices, FUN = subset_observations_def, x = weights)

    #--------------------------------------------------------------------------------------------#
    #Compute score estimate and standard error
    log_trace(object = logger, message = paste("Computing accuracy"), sep = "\n", add.level = TRUE, add.time = TRUE)

    out = list()
    for(i in seq(length(predlist))){
      out = c(
        out,
        switch(
          score.type,
          'summary_score'    = summary_score(scorer = scorer[which(ipred == i)], true = newy, pred = predlist[[i]], weights = weights, min.obs = min.obs),
          'mean_score'       = mean_score(   scorer = scorer[which(ipred == i)], true = newy, pred = predlist[[i]], weights = weights, min.obs = min.obs),
          'score'            = score(        scorer = scorer[which(ipred == i)], true = newy, pred = predlist[[i]], weights = weights, min.obs = min.obs),
          'score_and_summary'= summary_score(scorer = scorer[which(ipred == i)], true = newy, pred = predlist[[i]], weights = weights, min.obs = min.obs, return.raw = TRUE)
        )
      )
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
  f = "test",
  signature = methods::signature(models = "ANY", tester = "Tester", forecaster = "missing", scorer = "missing"),
  definition = function(
    models,
    #tester
    tester,
    ...){

    #--------------------------------------------------------------------------------------------#
    #scorer
    scorer = get_scorer(tester)

    #--------------------------------------------------------------------------------------------#
    #forecaster
    forecaster = get_forecaster(tester)

    #--------------------------------------------------------------------------------------------#
    #test
    out = test(models = models, forecaster = forecaster, scorer = scorer, ...)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)


methods::setMethod(
  f = "test",
  signature = methods::signature(models = "TunedList", tester = "missing", forecaster = "Forecaster", scorer = "Scorer", indices = "list"),
  definition = function(
    #trained
    models,
    #tester
    # tester,
    forecaster,
    scorer,
    #resample
    indices,
    screened = NULL,
    #prediction
    newx,
    newoffset = NULL,
    # type,
    #test
    newy,
    weights = NULL,
    score.type = c("mean_score", "summary_score", "score", "score_and_summary"),
    min.obs = 3,
    # multi = c("average", "sum"), grouped = TRUE,#passed by scorer
    #other
    looper = Looper(),
    logger,
    ...){

    #--------------------------------------------------------------------------------------------#
    #match
    # multi = match.arg(multi)
    score.type = match.arg(score.type)

    #--------------------------------------------------------------------------------------------#
    #logger
    if(missing(logger)){logger = get_logger(scorer)}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #screened
    if(missing(screened)){screened = get_screened(models)}

    #--------------------------------------------------------------------------------------------#
    #subset y
    newy    = lapply(X = indices, FUN = subset_observations_def, x = newy)
    weights = lapply(X = indices, FUN = subset_observations_def, x = weights)

    #--------------------------------------------------------------------------------------------#
    #extract trained
    trained = TrainedList(get_model(models))

    #--------------------------------------------------------------------------------------------#
    #extract learning methods
    learning.methods = unlist(get_learning_method(trained))

    #--------------------------------------------------------------------------------------------#
    #extract all scores
    measure = get_id(scorer)

    #--------------------------------------------------------------------------------------------#
    #for each required score select the needed prediction type
    log_trace(object = logger, message = paste("Check prediction type"), sep = "\n", add.level = TRUE, add.time = TRUE)
    pred.type = get_prediction_type(object = forecaster, type.measure = measure)

    #--------------------------------------------------------------------------------------------#
    #loop
    predlist = get_resample_predlist(
      trainedlist = trained,
      newx        = newx,
      type        = pred.type,
      newoffset   = newoffset,
      looper      = looper,
      logger      = logger,
      indices     = indices,
      screened    = screened,
      forecaster  = forecaster
      ,...
    )

    #--------------------------------------------------------------------------------------------#
    #Compute score estimate and standard error
    log_trace(object = logger, message = paste("Computing accuracy"), sep = "\n", add.level = TRUE, add.time = TRUE)
    out = switch(
      score.type,
      'summary_score'    = summary_score(scorer = scorer, true = newy, pred = predlist, weights = weights, min.obs = min.obs, logger = logger),
      'mean_score'       = mean_score(   scorer = scorer, true = newy, pred = predlist, weights = weights, min.obs = min.obs, logger = logger),
      'score'            = score(        scorer = scorer, true = newy, pred = predlist, weights = weights, min.obs = min.obs, logger = logger),
      'score_and_summary'= summary_score(scorer = scorer, true = newy, pred = predlist, weights = weights, min.obs = min.obs, logger = logger, return.raw = TRUE)
    )

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)


methods::setMethod(
  f = "test",
  signature = methods::signature(models = "TunedList", tester = "missing", forecaster = "Forecaster", scorer = "ScorerList", indices = "list"),
  definition = function(
    #trained
    models,
    #tester
    # tester,
    forecaster,
    scorer,
    #resample
    indices,
    screened = NULL,
    #prediction
    newx,
    newoffset = NULL,
    # type,
    #test
    newy,
    weights = NULL,
    score.type = c("mean_score", "summary_score", "score", "score_and_summary"),
    min.obs = 3,
    # multi = c("average", "sum"), grouped = TRUE,#passed by scorer
    #other
    looper = Looper(),
    logger,
    ...){

    #--------------------------------------------------------------------------------------------#
    #match
    # multi = match.arg(multi)
    score.type = match.arg(score.type)

    #--------------------------------------------------------------------------------------------#
    #logger
    if(missing(logger)){logger = get_logger(scorer)}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #screened
    if(missing(screened)){screened = get_screened(models)}

    #--------------------------------------------------------------------------------------------#
    #subset y
    newy    = lapply(X = indices, FUN = subset_observations_def, x = newy)
    weights = lapply(X = indices, FUN = subset_observations_def, x = weights)

    #--------------------------------------------------------------------------------------------#
    #extract trained
    trained = TrainedList(get_model(models))

    #--------------------------------------------------------------------------------------------#
    #extract learning methods
    learning.methods = unlist(get_learning_method(trained))

    #--------------------------------------------------------------------------------------------#
    #extract all scores
    measures = unlist(get_id(scorer))

    #--------------------------------------------------------------------------------------------#
    #for each required score select the needed prediction type
    log_trace(object = logger, message = paste("Check prediction type"), sep = "\n", add.level = TRUE, add.time = TRUE)
    pred.types = list()
    for(measure in measures){
      # pred.types[[measure]] = get_prediction_type(object = forecaster, type.measure = measure)
      pred.types[[measure]] = stats::setNames(object = get_prediction_type(object = forecaster, type.measure = measure), nm = NULL)
    }

    #--------------------------------------------------------------------------------------------#
    #check dupli
    is.dupli = duplicated(pred.types)
    #rm
    pred.types.no.dupli = pred.types[!is.dupli]

    #--------------------------------------------------------------------------------------------#
    #loop
    predlist = list()
    for(i in seq(length(pred.types.no.dupli))){
      predlist[[i]] = get_resample_predlist(
        trainedlist = trained,
        newx        = newx,
        type        = pred.types.no.dupli[[i]],
        newoffset   = newoffset,
        looper      = looper,
        logger      = logger,
        indices     = indices,
        screened    = screened,
        forecaster  = forecaster
        ,...
      )
    }

    #--------------------------------------------------------------------------------------------#
    #reshape and order by measure
    ipred = match(x = pred.types, table = pred.types.no.dupli)

    #--------------------------------------------------------------------------------------------#
    #Compute score estimate and standard error
    log_trace(object = logger, message = paste("Computing accuracy"), sep = "\n", add.level = TRUE, add.time = TRUE)

    out = list()
    for(i in seq(length(predlist))){
      out = c(
        out,
        switch(
          score.type,
          'summary_score' = summary_score(scorer = scorer[which(ipred == i)], true = newy, pred = predlist[[i]], weights = weights, min.obs = min.obs),
          'mean_score'    = mean_score(   scorer = scorer[which(ipred == i)], true = newy, pred = predlist[[i]], weights = weights, min.obs = min.obs),
          'score'         = score(        scorer = scorer[which(ipred == i)], true = newy, pred = predlist[[i]], weights = weights, min.obs = min.obs),
          'score_and_summary'= summary_score(scorer = scorer[which(ipred == i)], true = newy, pred = predlist[[i]], weights = weights, min.obs = min.obs, return.raw = TRUE)
        )
      )
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
  f = "test",
  signature = methods::signature(models = "list", tester = "missing", forecaster = "Forecaster", scorer = "Scorer", indices = "list"),
  definition = function(
    #trained
    models,
    #tester
    # tester,
    forecaster,
    scorer,
    #resample
    indices,
    screened = NULL,
    #prediction
    newx,
    newoffset = NULL,
    # type,
    #test
    newy,
    weights = NULL,
    score.type = c("mean_score", "summary_score", "score", "score_and_summary"),
    min.obs = 3,
    # multi = c("average", "sum"), grouped = TRUE,#passed by scorer
    #other
    looper = Looper(),
    logger,
    ...){

    #--------------------------------------------------------------------------------------------#
    #match
    # multi = match.arg(multi)
    score.type = match.arg(score.type)

    #--------------------------------------------------------------------------------------------#
    #logger
    if(missing(logger)){logger = get_logger(scorer)}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #screened
    if(missing(screened)){screened = lapply(X = models, FUN = function(obj){if("Trained" %in% class(obj)){NULL}else{get_screened(obj)}})}

    #--------------------------------------------------------------------------------------------#
    #subset y
    newy    = lapply(X = indices, FUN = subset_observations_def, x = newy)
    weights = lapply(X = indices, FUN = subset_observations_def, x = weights)

    #--------------------------------------------------------------------------------------------#
    #extract trained
    # trained = TrainedList(get_tuned(trained))

    #--------------------------------------------------------------------------------------------#
    #extract learning methods
    learning.methods = unlist(lapply(X = models, FUN = get_learning_method))

    #--------------------------------------------------------------------------------------------#
    #extract all scores
    measure = get_id(scorer)

    #--------------------------------------------------------------------------------------------#
    #for each required score select the needed prediction type
    log_trace(object = logger, message = paste("Check prediction type"), sep = "\n", add.level = TRUE, add.time = TRUE)
    pred.type = get_prediction_type(object = forecaster, type.measure = measure)

    #--------------------------------------------------------------------------------------------#
    #loop
    predlist = get_resample_predlist(
      trainedlist = models,
      newx        = newx,
      type        = pred.type,
      newoffset   = newoffset,
      looper      = looper,
      logger      = logger,
      indices     = indices,
      screened    = screened,
      forecaster  = forecaster
      ,...
    )

    #--------------------------------------------------------------------------------------------#
    #Compute score estimate and standard error
    log_trace(object = logger, message = paste("Computing accuracy"), sep = "\n", add.level = TRUE, add.time = TRUE)
    out = switch(
      score.type,
      'summary_score'    = summary_score(scorer = scorer, true = newy, pred = predlist, weights = weights, min.obs = min.obs, logger = logger),
      'mean_score'       = mean_score(   scorer = scorer, true = newy, pred = predlist, weights = weights, min.obs = min.obs, logger = logger),
      'score'            = score(        scorer = scorer, true = newy, pred = predlist, weights = weights, min.obs = min.obs, logger = logger),
      'score_and_summary'= summary_score(scorer = scorer, true = newy, pred = predlist, weights = weights, min.obs = min.obs, logger = logger, return.raw = TRUE)
    )

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #return
    return(out)
  }
)

