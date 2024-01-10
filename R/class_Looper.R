#' @include classes_generics.R class_Logger.R
NULL

#' Looper Class
#' An S4 class providing methods for loop constructs.
#'
#' The object consists of 2 slots
#' @slot logger a \linkS4class{Logger}
#' @slot cores the number of cores to use. If \code{cores > 1} a parallel environment is setup.
#' @author Alessandro Barberis
methods::setClass(
  Class = "Looper",
  slots = c(
    logger  = "Logger",
    cores   = "integer"
  )
)

#' Looper Class Constructor
#' @export
#' @rdname Looper-class
Looper <- function(
  logger  = Logger(),
  cores   = 1L
){
  methods::new(
    Class = "Looper",
    logger  = logger,
    cores   = cores
  )
}

methods::setMethod(f = "get_logger",   signature = "Looper", definition = function(object){methods::slot(object = object, name = 'logger')})
methods::setMethod(f = "get_cores",    signature = "Looper", definition = function(object){methods::slot(object = object, name = 'cores')})



#'Loop
#'@description Common interface to run a for loop sequentially or in parallel.
#'The core of the loop is defined by the function \code{fun}.
#'@param looper a \linkS4class{Looper}
#'@param n.iter number of iterations of the loop
#'@param fun A function to be executed inside the loop. It should contain an \code{iloop} parameter.
#@param cores number of cores to use for parallel execution.
#'@param ... further arguments to \code{fun}
#'@inheritParams foreach::foreach
#'
#'@keywords internal
methods::setMethod(
  f = "loop",
  signature = methods::signature(looper = "Looper"),
  definition = function(looper, n.iter, .inorder = TRUE, fun, ...){

    #-----------------------------------------------------------------------------------#
    #logger
    logger = get_logger(looper)
    logger = open_con(logger)
    verbose = get_verbose(logger)

    #-----------------------------------------------------------------------------------#
    #cores
    cores = get_cores(looper)

    #-----------------------------------------------------------------------------------#
    parallel = ifelse(test = cores > 1, yes = TRUE, no = FALSE)

    #-----------------------------------------------------------------------------------#
    if(parallel){
      #Setup parallel
      log_info(object = logger, message = "Detecting the number of available cores: ", sep = "", add.level = TRUE, add.time = TRUE)

      numCores.max = parallel::detectCores();

      log_info(object = logger, message = numCores.max, sep = "\n", add.level = F, add.time = F)


      if(!is.na(numCores.max) && numCores.max>1){
        log_info(object = logger, message = "Setting up parallel environment...", sep = "", add.level = TRUE, add.time = TRUE)

        if(cores >= numCores.max) {cores = numCores.max - 1}

        cl <- parallel::makeCluster(spec = cores)
        doParallel::registerDoParallel(cl);

        log_info(object = logger, message = paste("DONE:", cores, "cores selected"), sep = "\n", add.level = F, add.time = F)
      } else {
        parallel     = FALSE;
        warning("The number of detected cores is 1 or not available. Execution set to sequential.")
      }
    }

    #-----------------------------------------------------------------------------------#
    # create list to store results
    outlist = as.list(seq(n.iter));

    #-----------------------------------------------------------------------------------#
    #execute

    if(parallel){


      `%dopar%` <- foreach::`%dopar%`;
      tryCatch(expr = {

        outlist = foreach::foreach(i = seq(n.iter),
                                   .packages = c(get_name_package()),
                                   .inorder = .inorder,
                                   .verbose = verbose) %dopar%
          {
            #Fit the model
            res = do.call(what = fun, args = c(list(iloop = i), list(...)))

            #----------------------------------------------------------------------#

            #Open connection to log file
            logger.task = open_con(logger);

            log_info(object = logger.task, message = paste0("[",i, "] : DONE"), sep = "\n", add.level = T, add.time = T)
            log_info(object = logger.task, message = get_log_line("long.line.1"), sep = "\n", add.level = F, add.time = F)

            #Close connection to log file
            close_con(logger.task);
            #----------------------------------------------------------------------#

            return(res);
          }

      }, finally = {
        #Stop cluster
        log_info(object = logger, message = "Shut down workers and stop parallel socket cluster...", sep = "", add.level = T, add.time = T)

        parallel::stopCluster(cl = cl);

        log_info(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
      })

    } else {
      for(i in 1L:n.iter){
        # log_info(object = logger, message = paste("Iteration", i), sep = "\n", add.level = T, add.time = T)
        log_debug(object = logger, message = paste("Iteration", i), sep = "\n", add.level = T, add.time = T)

        #Fit the model
        outlist[[i]] = do.call(what = fun, args = c(list(iloop = i), list(...)))
      }#END FOR(time in 1L:repeats)
    }
    #-----------------------------------------------------------------------------------#

    close_con(logger)

    #-----------------------------------------------------------------------------------#
    return(outlist)
  }
)

#'Loop
#'@description Common interface to run a for loop sequentially or in parallel.
#'The core of the loop is defined by the function \code{fun}.
#'@param n.iter number of iterations of the loop
#'@param logger a \linkS4class{Logger}
#'@param fun A function to be executed inside the loop. It should contain an \code{iloop} parameter.
#'@param cores number of cores to use for parallel execution.
#'@param ... further arguments to \code{fun}
#'@inheritParams foreach::foreach
forloop = function(n.iter, logger, cores = 1, .inorder = TRUE, fun, ...){

  #-----------------------------------------------------------------------------------#
  if(missing(logger)) {logger = Logger()}
  logger = open_con(logger)
  #-----------------------------------------------------------------------------------#
  verbose = get_verbose(logger)

  #-----------------------------------------------------------------------------------#
  parallel = ifelse(test = cores > 1, yes = TRUE, no = FALSE)

  #-----------------------------------------------------------------------------------#
  if(parallel){
    #Setup parallel
    log_info(object = logger, message = "Detecting the number of available cores: ", sep = "", add.level = TRUE, add.time = TRUE)

    numCores.max = parallel::detectCores();

    log_info(object = logger, message = numCores.max, sep = "\n", add.level = F, add.time = F)


    if(!is.na(numCores.max) && numCores.max>1){
      log_info(object = logger, message = "Setting up parallel environment...", sep = "", add.level = TRUE, add.time = TRUE)

      if(cores >= numCores.max) {cores = numCores.max - 1}

      cl <- parallel::makeCluster(spec = cores)
      doParallel::registerDoParallel(cl);

      log_info(object = logger, message = paste("DONE:", cores, "cores selected"), sep = "\n", add.level = F, add.time = F)
    } else {
      parallel     = FALSE;
      warning("The number of detected cores is 1 or not available. Execution set to sequential.")
    }
  }

  #-----------------------------------------------------------------------------------#
  # create list to store results
  outlist = as.list(seq(n.iter));

  #-----------------------------------------------------------------------------------#
  #execute

  if(parallel){


    `%dopar%` <- foreach::`%dopar%`;
    tryCatch(expr = {

      outlist = foreach::foreach(i = seq(n.iter),
                                 .packages = c(get_name_package()),
                                 .inorder = .inorder,
                                 .verbose = verbose) %dopar%
        {
          #Fit the model
          res = do.call(what = fun, args = c(list(iloop = i), list(...)))

          #----------------------------------------------------------------------#

          #Open connection to log file
          logger.task = open_con(logger);

          log_info(object = logger.task, message = paste0("[",i, "] : DONE"), sep = "\n", add.level = T, add.time = T)
          log_info(object = logger.task, message = get_log_line("long.line.1"), sep = "\n", add.level = F, add.time = F)

          #Close connection to log file
          close_con(logger.task);
          #----------------------------------------------------------------------#

          return(res);
        }

    }, finally = {
      #Stop cluster
      log_info(object = logger, message = "Shut down workers and stop parallel socket cluster...", sep = "", add.level = T, add.time = T)

      parallel::stopCluster(cl = cl);

      log_info(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    })

  } else {
    for(i in 1L:n.iter){
      log_info(object = logger, message = paste("Iteration", i), sep = "\n", add.level = T, add.time = T)

      #Fit the model
      outlist[[i]] = do.call(what = fun, args = c(list(iloop = i), list(...)))
    }#END FOR(time in 1L:repeats)
  }
  #-----------------------------------------------------------------------------------#

  close_con(logger)

  #-----------------------------------------------------------------------------------#
  return(outlist)
}
