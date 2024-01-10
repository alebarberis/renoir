#' @include classes_generics.R class_Logger.R class_Sampler.R class_Looper.R
NULL

#' Resampler Class
#' An S4 class providing the re-sampling methods.
#'
#' The object consists of 2 slots
#' @slot sampler a \linkS4class{Sampler}
#' @slot logger a \linkS4class{Logger}
# @slot cores the number of cores to use. If \code{cores > 1} a parallel environment is setup.
#' @slot looper a \linkS4class{Looper}
#' @author Alessandro Barberis
methods::setClass(
  Class = "Resampler",
  slots = c(
    sampler = "Sampler",
    logger  = "Logger",
    # cores   = "integer"
    looper = "Looper"
  )
)

#' Resampler Class Constructor
#'
#' @rdname Resampler-class
Resampler <- function(
  sampler = Sampler(),
  logger  = Logger(),
  # cores   = 1
  looper  = Looper()
){

  methods::new(
    Class = "Resampler",
    sampler = sampler,
    logger  = logger,
    # cores   = cores
    looper  = looper
  )
}

methods::setMethod(f = "get_sampler",  signature = "Resampler", definition = function(object){methods::slot(object = object, name = 'sampler')})
methods::setMethod(f = "get_logger",   signature = "Resampler", definition = function(object){methods::slot(object = object, name = 'logger')})
# methods::setMethod(f = "get_cores",    signature = "Resampler", definition = function(object){methods::slot(object = object, name = 'cores')})
methods::setMethod(f = "get_looper",    signature = "Resampler", definition = function(object){methods::slot(object = object, name = 'looper')})

methods::setMethod(f = "get_method",   signature = "Resampler", definition = function(object){ sampler = get_sampler(object); get_method(sampler)})
methods::setMethod(f = "get_N",        signature = "Resampler", definition = function(object){ sampler = get_sampler(object); get_N(sampler)})
methods::setMethod(f = "get_n",        signature = "Resampler", definition = function(object){ sampler = get_sampler(object); get_n(sampler)})
methods::setMethod(f = "get_k",        signature = "Resampler", definition = function(object){ sampler = get_sampler(object); get_k(sampler)})

methods::setMethod(f = "set_sampler", signature = "Resampler", definition = function(object, value){methods::slot(object = object, name = 'sampler') = value; return(object)})

methods::setMethod(f = "set_N", signature = "Resampler", definition = function(object, value){
  #get sampler
  sampler = get_sampler(object);
  #update N
  sampler = set_N(object = sampler, value)
  #update resampler
  object = set_sampler(object = object, value = sampler);
  #return
  return(object)
  }
)

methods::setMethod(f = "set_n", signature = "Resampler", definition = function(object, value){
  #get sampler
  sampler = get_sampler(object);
  #update N
  sampler = set_n(object = sampler, value)
  #update resampler
  object = set_sampler(object = object, value = sampler);
  #return
  return(object)
})

methods::setMethod(f = "set_size", signature = "Resampler", definition = function(object, value){
  #get sampler
  sampler = get_sampler(object);
  #update size
  sampler = set_size(object = sampler, value)
  #update resampler
  object = set_sampler(object = object, value = sampler);
  #return
  return(object)
})

methods::setMethod(
  f = "subset_observations",
  signature = "Resampler",
  definition = function(object, which){
    #get sampler
    sampler = get_sampler(object);
    #subset
    sampler = subset_observations(object = sampler, which = which)
    #update
    object = set_sampler(object = object, value = sampler)
    #return
    return(object)
  }
)



#'Resample
#'@description Takes repeated samples with or without replacement from the population and feed them to function \code{fun}
#'@param fun A function to be executed on re-sampled data. It should contain an \code{indices} parameter
#'@param do.parallel logical, whether to try to parallelise the train-and-test procedure
#'@param cores number of cores to use for parallel execution
#'@param ... further arguments to \code{fun}
#'@inheritParams foreach::foreach
#'@return A \code{list} containing the output of function \code{fun}
#'@author Alessandro Barberis
#'
#'@rdname resample
methods::setMethod(
  f = "resample",
  signature = methods::signature(object = "Resampler"),
  definition = function(object, .inorder = TRUE, fun, ...){


    #-----------------------------------------------------------------------------------#
    #logger
    logger = get_logger(object)
    # logger = open_con(logger)
    verbose = get_verbose(logger)

    #-----------------------------------------------------------------------------------#
    #sampler
    sampler = get_sampler(object)

    #-----------------------------------------------------------------------------------#
    #looper
    looper = get_looper(object)

    #-----------------------------------------------------------------------------------#

    samples = resample(object = sampler)

    repeats = length(samples)
    #-----------------------------------------------------------------------------------#

    outlist = loop(
      looper = looper,
      n.iter = repeats,
      # cores = cores,
      .inorder = .inorder,
      fun = function(iloop, samples, FUN, ...){
        do.call(what = FUN, args = c(list(indices = samples[[iloop]]), list(...)))
        }, FUN = fun, samples = samples, ...)

    #-----------------------------------------------------------------------------------#

    close_con(logger)

    #-----------------------------------------------------------------------------------#
    return(outlist)
  }
)


methods::setMethod(
  f = "get_train_set_size",
  signature = methods::signature(object = "Resampler"),
  definition = function(object){
    #get sampler
    sampler = get_sampler(resampler)
    #get size
    out = get_train_set_size(object = sampler)
    #return
    return(out)
  }
)


# methods::setMethod(
#   f = "resample",
#   signature = methods::signature(object = "Resampler"),
#   definition = function(object, cores = 1, .inorder = TRUE, fun, ...){
#
#
#     #-----------------------------------------------------------------------------------#
#     logger = get_logger(object)
#     logger = open_con(logger)
#     #-----------------------------------------------------------------------------------#
#     verbose = get_verbose(logger)
#
#     #-----------------------------------------------------------------------------------#
#
#     sampler = get_sampler(object)
#
#     if(missing(cores)){
#       cores = get_cores(object)
#     }
#
#     #-----------------------------------------------------------------------------------#
#     parallel = ifelse(test = cores > 1, yes = TRUE, no = FALSE)
#
#     samples = resample(object = sampler)
#
#     repeats = length(samples)
#     #-----------------------------------------------------------------------------------#
#     if(parallel){
#       #Setup parallel
#       log_info(object = logger, message = "Detecting the number of available cores: ", sep = "", add.level = TRUE, add.time = TRUE)
#
#       numCores.max = parallel::detectCores();
#
#       log_info(object = logger, message = numCores.max, sep = "\n", add.level = F, add.time = F)
#
#
#       if(!is.na(numCores.max) && numCores.max>1){
#         log_info(object = logger, message = "Setting up parallel environment...", sep = "", add.level = TRUE, add.time = TRUE)
#
#         if(cores >= numCores.max) {cores = numCores.max - 1}
#
#         cl <- parallel::makeCluster(spec = cores)
#         doParallel::registerDoParallel(cl);
#
#         log_info(object = logger, message = paste("DONE:", cores, "cores selected"), sep = "\n", add.level = F, add.time = F)
#       } else {
#         parallel     = FALSE;
#         warning("The number of detected cores is 1 or not available. Execution set to sequential.")
#       }
#     }
#
#     #-----------------------------------------------------------------------------------#
#     # create list to store results
#     outlist = as.list(seq(repeats));
#
#     #-----------------------------------------------------------------------------------#
#     #execute
#
#     if(parallel){
#
#
#       `%dopar%` <- foreach::`%dopar%`;
#       tryCatch(expr = {
#
#         outlist = foreach::foreach(i = seq(repeats),
#                                    .packages = c(get_name_package()),
#                                    .inorder = .inorder,
#                                    .verbose = verbose) %dopar%
#           {
#             #Fit the model
#             res = do.call(what = fun, args = c(list(indices = samples[[i]]), list(...)))
#
#             #----------------------------------------------------------------------#
#
#             #Open connection to log file
#             logger.task = open_con(logger);
#
#             log_info(object = logger.task, message = paste0("[",i, "] : DONE"), sep = "\n", add.level = T, add.time = T)
#             log_info(object = logger.task, message = get_log_line("long.line.1"), sep = "\n", add.level = F, add.time = F)
#
#             #Close connection to log file
#             close_con(logger.task);
#             #----------------------------------------------------------------------#
#
#             return(res);
#           }
#
#       }, finally = {
#         #Stop cluster
#         log_info(object = logger, message = "Shut down workers and stop parallel socket cluster...", sep = "", add.level = T, add.time = T)
#
#         parallel::stopCluster(cl = cl);
#
#         log_info(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
#       })
#
#     } else {
#       for(i in 1L:repeats){
#         log_info(object = logger, message = paste("Iteration", i), sep = "\n", add.level = T, add.time = T)
#
#         #Fit the model
#         outlist[[i]] = do.call(what = fun, args = c(list(indices = samples[[i]]), list(...)))
#       }#END FOR(time in 1L:repeats)
#     }
#     #-----------------------------------------------------------------------------------#
#
#     close_con(logger)
#
#     #-----------------------------------------------------------------------------------#
#     return(outlist)
#   }
# )


