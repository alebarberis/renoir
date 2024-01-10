#' @include classes_generics.R class_Logger.R class_Trained.R class_TrainedList.R
NULL

#' Trainer Class
#' An S4 class representing a learning method.
#'
#' The object consists of 4 slots
#' @slot id the learning method name
#' @slot trainer trainer function.
#' It must have the following formal arguments:
#' \describe{
#'    \item{x}{the input matrix, where rows are observations and columns are variables.}
#'    \item{y}{the response variable. Its number of rows must match the number of rows of \code{x}.}
#'    \item{weights}{priors of the observations}
#'    \item{offset}{used only for GLM methods, it is an a priori known component to be included in the linear predictor during fitting}
#'    \item{resp.type}{the response type}
#'    \item{observations}{indices of observations to keep}
#'    \item{features}{indices of predictors to keep}
#'    \item{...}{additional arguments}
#' }
#' @slot parameters list containing the parameters for the chosen learning method
#' @slot logger a \linkS4class{Logger}
#'
#' @author Alessandro Barberis
methods::setClass(
  Class = "Trainer",
  slots = c(
    id         = "character",
    trainer    = "function",
    parameters = "list",
    logger     = "Logger"
  )
)

#' Constructor for the S4 Trainer object.
#'
#' Constructor for the S4 \linkS4class{Trainer} object.
#'
#' @param id the learning method name associated to this \linkS4class{Trainer}.
#' If learning method is one of the supported by renoir, the constructor will
#' automatically select a \code{trainer}. See \code{supported_learning_methods()}
#' for the supported methods.
#' @param trainer (optional) function to train a model.
#' Used if \code{id} is not one of the supported by renoir.
#' If \code{trainer} is provided it must conform to the renoir common interface,
#' and must have the following formal arguments:
#' \describe{
#'    \item{x}{the input matrix, where rows are observations and columns are variables.}
#'    \item{y}{the response variable. Its number of rows must match the number of rows of \code{x}.}
#'    \item{weights}{priors of the observations}
#'    \item{offset}{used only for GLM methods, it is an a priori known component to be included in the linear predictor during fitting}
#'    \item{resp.type}{the response type}
#'    \item{observations}{indices of observations to keep}
#'    \item{features}{indices of predictors to keep}
#'    \item{...}{additional arguments}
#' }
#' @param parameters list containing the parameters for the chosen learning method
#' @param logger a \linkS4class{Logger}
#'
#' @return An object of class \linkS4class{Trainer}.
#'
#' @export
#'
#' @author Alessandro Barberis
#'@rdname Trainer-class
Trainer <- function(
  id,
  trainer,
  parameters,
  logger   = Logger()
){

  if(missing(trainer) && (id %in% supported_learning_methods())){
    trainer = get_trainer_function(id)
  } else if(!missing(trainer)){
    if(id %in% supported_learning_methods()){
      stop(paste("'id' not valid, please change name. It must be different from the built-in renoir method names:\n",paste(renoir_learning_methods(), collapse = ", "),"\n"))
    }
  } else {
    stop("Please provide a valid 'id' and/or 'trainer'.\n")
  }

  #Check provided prediction function
  check_provided_trainer_function(trainer)

  #Check parameters
  if(missing(parameters) || is.null(parameters) || length(parameters)==0){
    parameters = list()
  }

  methods::new(
    Class = "Trainer",
    trainer    = trainer,
    id         = id,
    parameters = parameters,
    logger     = logger
  )
}

methods::setMethod(f = "get_learning_method", signature = "Trainer", definition = function(object){methods::slot(object = object, name = 'id')})
methods::setMethod(f = "get_id",         signature = "Trainer", definition = function(object){methods::slot(object = object, name = 'id')})
methods::setMethod(f = "get_parameters", signature = "Trainer", definition = function(object){methods::slot(object = object, name = 'parameters')})
methods::setMethod(f = "get_trainer",    signature = "Trainer", definition = function(object){methods::slot(object = object, name = 'trainer')})
methods::setMethod(f = "get_logger",     signature = "Trainer", definition = function(object){methods::slot(object = object, name = 'logger')})

#'Train the model
#'
#'@description Common interface to train a model. The learning method is provided in \code{trainer}
#'
#'@param trainer a \linkS4class{Trainer} object providing the learning method
#'@param x the input matrix, where rows are observations and columns are variables.
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}.
#'@param weights priors of the observations
#'@param offset used only for GLM methods, it is an a priori known component to be included in the linear predictor during fitting
#'@param resp.type the response type
#'@param observations (optional) indices of observations to keep
#'@param features (optional) indices of predictors to keep
#'@param ... additional arguments to training method
#'
#'@return a \linkS4class{Trained} object or a \linkS4class{TrainedList} object if learning method
#'runs multiple configurations (i.e. multiple sets of hyper-parameters).
#'
#'@author Alessandro Barberis
#'
#'
#'@rdname train
methods::setMethod(
  f = "train",
  signature = methods::signature(trainer ="Trainer"),
  definition = function(
    trainer,
    x,
    y,
    weights = NULL,
    offset  = NULL,
    resp.type,
    observations = NULL,
    features = NULL,
    ...){

    #get parameters
    parameters = get_parameters(trainer)

    #check
    parameters = check_parameters(parameters, ...)

    #merge
    args = c(parameters, list(...))

    #----------------------------------------------------------------------#
    #get function
    trainer_fun = get_trainer(trainer)

    #----------------------------------------------------------------------#
    # clean
    rm(parameters)

    #train
    tryCatch(expr = {
      out = do.call(
        what = trainer_fun,
        args = c(list(
          x       = x,
          y       = y,
          weights = weights,
          offset  = offset,
          resp.type    = resp.type,
          observations = observations,
          features     = features
          ), args)
      )

      # out = trainer_fun(
      #   x = x,
      #   y = y,
      #   weights= weights,
      #   offset = offset,
      #   resp.type = resp.type,
      #   observations = observations,
      #   features = features,
      #   unlist(parameters),
      #   ...)
    },
    error = function(cnd){
      #Get the current process id
      pid = Sys.getpid();
      #Set a process index
      pidi = 1;
      #Setup a dump dir path
      dp = file.path(getwd(), "dump", paste(pid, pidi, sep="_"));
      while(dir.exists(dp)){
        pidi = pidi + 1;
        dp = file.path(getwd(), "dump", paste(pid, pidi, sep="_"));
      }
      dir.create(path = dp, recursive = T);
      #Setup a file name
      # paste(Sys.info()[['nodename']], Sys.getpid(), sep='-')
      dfn = paste0("train_last_dump_",pidi)
      # #add extension
      # dfn = paste0(dfn,".rda")
      # #Setup a file path
      # dfp = file.path(dp, dfn);
      # #args
      # # args = c(
      # #   x = x,
      # #   y = y,
      # #   weights= weights,
      # #   offset = offset,
      # #   resp.type = resp.type,
      # #   observations = observations,
      # #   features = features,
      # #   parameters, list(...))
      # # args = c(as.list(environment()), list(...))
      # dump.data = c(args, list(
      #   x       = x,
      #   y       = y,
      #   weights = weights,
      #   offset  = offset,
      #   resp.type    = resp.type,
      #   observations = observations,
      #   features     = features
      # ),
      # list(error = cnd, call = match.call()))
      #
      # #Store the call
      # save(dump.data, file = dfp)
      # #Return a meaningful message
      # message("An error occurred during training. A dump of data was saved ", dfp, appendLF = T)

      #get wd
      wd = getwd()
      #set
      setwd(dir = dp)
      #store
      utils::dump.frames(dumpto = dfn, to.file = T)
      #Return a meaningful message
      message("An error occurred during training. A dump of data was saved ", file.path(dp, paste0(dfn, ".rda")), appendLF = T)
      #set
      setwd(dir = wd)
      #Stop execution and raise the error
      stop(cnd);
    })

    #--------------------------------------------------------------------------------------------#
    #Update configuration with subset information
    if(!missing(features) && !is.null(features)){
      out = add_screened_nvar_to_config(object = out, value = length(features))
    }

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)

#'Train the model
#'
#'@inheritParams train,Trainer,ANY-method
#'@inherit train,Trainer,ANY-method return
#'
#'@keywords internal
#'
#'@rdname train
methods::setMethod(
  f = "train",
  signature = methods::signature(trainer ="Trainer", features = "missing"),
  definition = function(trainer, features, ...){train(trainer = trainer, features = NULL, ...)}
)

#'Train the model
#'
#'@inheritParams train,Trainer,ANY-method
#'@return Results are merged into one \linkS4class{TrainedList} object
#'@keywords internal
#'
#'@rdname train
methods::setMethod(
  f = "train",
  signature = methods::signature(trainer ="Trainer", features = "list"),
  definition = function(
    trainer,
    x,
    y,
    weights = NULL,
    offset  = NULL,
    resp.type,
    observations = NULL,
    features,
    logger = Logger(verbose = FALSE),
    ...){

    #--------------------------------------------------------------------------------------------#
    #Set logger
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    #Output object
    log_trace(object = logger, message = "Train a model for each set of features", sep = "\n", add.level = TRUE, add.time = TRUE)
    fitlist = lapply(X = features, FUN = train, trainer = trainer, x = x, y = y, weights = weights, offset = offset, resp.type = resp.type, observations = observations, ...)

    #--------------------------------------------------------------------------------------------#
    #Merge into a single TrainedList object
    log_trace(object = logger, message = "Combine results", sep = "\n", add.level = TRUE, add.time = TRUE)
    fitlist = do.call(c, args = fitlist)

    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    return(fitlist)
  }
)

get_sample_trainedlist <- function(trainer, logger, hyperparameters, observations, features = NULL, ..., iloop){

  #--------------------------------------------------------------------------------------------#
  #Set logger
  if(missing(logger)){logger = get_logger(trainer)}
  logger = open_con(logger)

  #--------------------------------------------------------------------------------------------#
  learning.method = get_id(trainer)

  #--------------------------------------------------------------------------------------------#
  #out
  fitlist = TrainedList()

  #--------------------------------------------------------------------------------------------#
  #Hyperparameter space
  hyperparameter.space = create_hyperparameters_grid(hyperparameters = hyperparameters, learning.method = learning.method)

  #get number of configurations
  n.config = nrow(hyperparameter.space)

  #--------------------------------------------------------------------------------------------#
  log_debug(object = logger, message = paste(iloop, "Training"), sep = "\n", add.level = TRUE, add.time = TRUE)

  if(n.config > 0){
    #Loop over configurations
    for(ic in seq(n.config)){

      #get config to test
      config = as.list(hyperparameter.space[ic,,drop=F])

      #get config to test
      log_trace(object = logger, message = paste("Configuration", ic), sep = "\n", add.level = TRUE, add.time = TRUE)
      trained.i = do.call(
        what = train,
        args = c(list(
          trainer = trainer,
          features = features[[iloop]],
          observations = observations[[iloop]]
        ), config, list(...)))

      #store
      fitlist = c(fitlist, trained.i)

      #clean
      rm(trained.i)

    }
  } else {
    #If no hyperparameter is passed, use default.
    trained.i = do.call(
        what = train,
        args = c(
          list(trainer = trainer,
               features = features[[iloop]],
               observations = observations[[iloop]]),
          list(...)))

    #store
    fitlist = c(fitlist, trained.i)

    #clean
    rm(trained.i)
  }

  #--------------------------------------------------------------------------------------------#
  #close
  close_con(logger)

  #--------------------------------------------------------------------------------------------#
  #store
  return(list(indices = iloop, fitlist = fitlist))
}

#'@keywords internal
get_resample_trainedlist = function(trainer, ..., looper, observations, .inorder = FALSE){

  out = loop(
    looper = looper,
    n.iter = length(observations),
    .inorder = .inorder,
    fun = get_sample_trainedlist,
    trainer = trainer, observations = observations, ...)

  return(out)
}

get_trainer_function <-function(learning.method) {
  trainer = switch(
    learning.method,
    # glmnet         = renoir_glmnet,
    lasso              = renoir_lasso,
    ridge              = renoir_ridge,
    elnet              = renoir_elnet,
    elasticnet         = renoir_elnet,
    # relaxed_glmnet = renoir_relaxed_glmnet,
    relaxed_lasso      = renoir_relaxed_lasso,
    relaxed_ridge      = renoir_relaxed_ridge,
    relaxed_elnet      = renoir_relaxed_elnet,
    relaxed_elasticnet = renoir_relaxed_elnet,
    randomForest       = renoir_random_forest,
    gbm                = renoir_gbm,
    xgbtree            = renoir_xgbtree,
    xgblinear          = renoir_xgblinear,
    linear_SVM         = renoir_linear_SVM       ,
    polynomial_SVM     = renoir_polynomial_SVM   ,
    radial_SVM         = renoir_radial_SVM       ,
    sigmoid_SVM        = renoir_sigmoid_SVM      ,
    linear_NuSVM       = renoir_linear_NuSVM     ,
    polynomial_NuSVM   = renoir_polynomial_NuSVM ,
    radial_NuSVM       = renoir_radial_NuSVM     ,
    sigmoid_NuSVM      = renoir_sigmoid_NuSVM    ,
    gknn               = renoir_gknn             ,
    nsc                = renoir_nearest_shrunken_centroid
  )
  return(trainer)
}

check_provided_trainer_function <- function(trainer){

  if(missing(trainer)){
    stop("'trainer' is missing with no default.\n")
  } else {
    #needed formals
    formals.def = c("x", "y", "weights", "offset", "resp.type", "observations", "features")

    #get formals
    formals.fun = names(formals(fun = trainer))

    #check
    # if(any(!(formals.fun %in% formals.def))){
    if(any(!(formals.def %in% formals.fun))){

      stop("Provided trainer function without required formal arguments.Function interface must match renoir requirements. Try with:\n
         function(x, y, weights, offset, resp.type, observations, features, ...){
            #Use provided data to train the model
            # out = YOUR CODE

            #Set a Trained object as output
            # out = Trained(fit = OUTPUT_OF_TRAINING, learning.method = NAME_OF_SELECTED_LEARNING_METHOD, config = HYPERPARAMETERS_LIST)

            #Return trained model
            return(out)
         }
         \n")
    }
  }
}

#'Generalized k-Nearest Neighbors Classification or Regression
#'
#'@param x the input matrix, where rows are observations and columns are variables
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}
#'@param weights priors of the observations
#'@param offset used only for GLM methods, it is an a priori known component to be included in the linear predictor during fitting
#'@param resp.type the response type
#'@param observations indices of observations to keep
#'@param features indices of predictors to keep
#'@param ... further arguments to \code{\link[e1071]{gknn}}
#'@param k number of neighbours considered
#'
#'@return An object of class \linkS4class{Trained}
#'
#'@author Alessandro Barberis
#'
#'@export
renoir_gknn <- function(
  x,
  y = NULL,
  weights = NULL,
  offset = NULL,
  resp.type,
  observations = NULL,
  features = NULL,
  clean = FALSE,
  keep.call = TRUE,
  ...,
  k = 1
){

  #--------------------------------------------------------------------------------------------#
  #Subset observations
  x       = subset_observations(object = x, which = observations)
  y       = subset_observations(object = y, which = observations)
  # offset  = subset_observations(object = offset, which = observations)
  weights = subset_observations(object = weights, which = observations)

  #subset features
  x = subset_features(object = x, which = features)

  #--------------------------------------------------------------------------------------------#
  #set fun
  fun = e1071:::gknn.default;#e1071::gknn;

  #--------------------------------------------------------------------------------------------#
  #check y
  if(isTRUE((identical(resp.type, "binomial") || identical(resp.type, "multinomial")))){
    #check if matrix
    if(isTRUE(is.null(dim(y)) && !is.factor(y))){
      y = factor(y)

    } else if(isTRUE(!is.null(dim(y)))){
      y = dummy.matrix.to.factor(y)
    }

    #store classes
    # classes = levels(y)

    #update y
    # y = as.numeric(y) - 1#function expect y in [0, 1, ..., nclasses - 1]
  } #else {classes = NULL}

  #--------------------------------------------------------------------------------------------#
  #get formals
  args.def = names(formals(fun = fun))

  #get provided
  args = c(
    list(
      x = x,
      y = y,
      k = k
    ),
    list(...)
  );


  #check passed arguments
  if(any(!(names(args) %in% args.def))){
    # warning("Provided argument not found in formals\n")
    warning(paste("Provided argument not found in formals:",
                  paste(setdiff(x = names(args), y = args.def), collapse = ", "),
                  "\n"))
  }

  #--------------------------------------------------------------------------------------------#
  #train
  out = do.call(what = fun, args = args)

  #--------------------------------------------------------------------------------------------#
  #clean
  if(isFALSE(keep.call)){
    out$call = NULL
  }

  if(isTRUE(clean)){
    # out$data = NULL
  }

  #--------------------------------------------------------------------------------------------#
  #update
  # out$classes = classes

  #--------------------------------------------------------------------------------------------#
  #Set Trained object
  out = Trained(
    fit = out,
    learning.method = "gknn",
    config = list(k = k),
    nfeatures = get_nrecords(records = record_gknn(out))
  )

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}

#'Support Vector Machine
#'
#'@param x the input matrix, where rows are observations and columns are variables
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}
#'@param weights priors of the observations
#'@param offset used only for GLM methods, it is an a priori known component to be included in the linear predictor during fitting
#'@param resp.type the response type
#'@param observations indices of observations to keep
#'@param features indices of predictors to keep
#'@param ... further arguments to \code{\link[e1071]{svm}}
#'@param cost cost of costraint violation (must be >0)
#'@param nu re-parametrization of \code{cost} which controls the
#'number of support vectors and the margin errors.
#'It must be in range (0, 1]. Note that for classification nu must be
#'nu * length(y)/2 <= min(table(y))
#'@inheritParams e1071::svm
#'
#'@return An object of class \linkS4class{Trained}
#'
#'@author Alessandro Barberis
#'
#'@export
renoir_svm <- function(
  x,
  y = NULL,
  weights = NULL,
  offset = NULL,
  resp.type,
  observations = NULL,
  features = NULL,
  clean = FALSE,
  keep.call = TRUE,
  ...,
  use.nusvm = FALSE,
  kernel = "radial",
  degree = 3,
  gamma = NULL,
  coef0 = 0,
  cost = 1,
  nu = 0.5,
  epsilon = 0.1,
  fitted = FALSE,
  # distribution,
  verbose = F
){

  #--------------------------------------------------------------------------------------------#
  #Subset observations
  x       = subset_observations(object = x, which = observations)
  y       = subset_observations(object = y, which = observations)
  # offset  = subset_observations(object = offset, which = observations)
  weights = subset_observations(object = weights, which = observations)

  #subset features
  x = subset_features(object = x, which = features)

  #--------------------------------------------------------------------------------------------#
  #set fun
  fun = e1071:::svm.default;#e1071::svm;

  #--------------------------------------------------------------------------------------------#
  #objective
  svm.type = switch(
    resp.type,
    'gaussian'    = if(isFALSE(use.nusvm)){'eps-regression'}else{'nu-regression'},
    'binomial'    = if(isFALSE(use.nusvm)){'C-classification'}else{'nu-classification'}
  )

  #--------------------------------------------------------------------------------------------#
  #check gamma
  if(isTRUE(missing(gamma) || is.null(gamma))){
    gamma = if (is.vector(x)) 1 else 1 / ncol(x)
  }

  #--------------------------------------------------------------------------------------------#
  #check y
  if(isTRUE((identical(resp.type, "binomial") || identical(resp.type, "multinomial")))){
    #check if matrix
    if(isTRUE(is.null(dim(y)) && !is.factor(y))){
      y = factor(y)

    } else if(isTRUE(!is.null(dim(y)))){
      y = dummy.matrix.to.factor(y)
    }

    #store classes
    # classes = levels(y)

    #update y
    # y = as.numeric(y) - 1#function expect y in [0, 1, ..., nclasses - 1]
  } #else {classes = NULL}

  #--------------------------------------------------------------------------------------------#
  #get formals
  args.def = names(formals(fun = fun))

  #get provided
  args = c(
    list(
      x = x,
      y = y,
      # offset = offset,
      type = svm.type,
      kernel = kernel,
      class.weights = weights,
      fitted = fitted,
      degree = degree,
      gamma = gamma,
      coef0 = coef0,
      cost = cost,
      nu = nu,
      epsilon = epsilon
    ),
    list(...)
  );


  #update
  args$probability = TRUE

  #check passed arguments
  if(any(!(names(args) %in% args.def))){
    # warning("Provided argument not found in formals\n")
    warning(paste("Provided argument not found in formals:",
                  paste(setdiff(x = names(args), y = args.def), collapse = ", "),
                  "\n"))
  }

  #--------------------------------------------------------------------------------------------#
  #train
  out = do.call(what = fun, args = args)

  #--------------------------------------------------------------------------------------------#
  #clean
  if(isFALSE(keep.call)){
    out$call = NULL
  }

  if(isTRUE(clean)){
    # out$data = NULL
  }

  #--------------------------------------------------------------------------------------------#
  #update
  # out$classes = classes

  #--------------------------------------------------------------------------------------------#
  #Set Trained object
  out = Trained(
    fit = out,
    learning.method = "svm",
    config = list(cost = cost, gamma = gamma, degree = degree, nu = nu),
    nfeatures = get_nrecords(records = record_svm(out))
  )

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}

#Linear SVM
renoir_linear_SVM <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){
  #get args
  args = c(as.list(environment()), list(...));

  #fix kernel
  args$kernel = 'linear'
  args$use.nusvm = FALSE

  #call fun
  out = do.call(what = renoir_svm, args = args)

  #update object
  out = set_learning_method(object = out, value = "linear_SVM")
  out = set_config(object = out, value = get_config(out)[renoir_trainer_default_hyperparameters("linear_SVM")])

  #return
  return(out)
}

#Polynomial SVM
renoir_polynomial_SVM <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){
  #get args
  args = c(as.list(environment()), list(...));

  #fix kernel
  args$kernel = 'polynomial'
  args$use.nusvm = FALSE

  #call fun
  out = do.call(what = renoir_svm, args = args)

  #update object
  out = set_learning_method(object = out, value = "polynomial_SVM")
  out = set_config(object = out, value = get_config(out)[renoir_trainer_default_hyperparameters("polynomial_SVM")])


  #return
  return(out)
}

#Radial SVM
renoir_radial_SVM <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){
  #get args
  args = c(as.list(environment()), list(...));

  #fix kernel
  args$kernel = 'radial'
  args$use.nusvm = FALSE

  #call fun
  out = do.call(what = renoir_svm, args = args)

  #update object
  out = set_learning_method(object = out, value = "radial_SVM")
  out = set_config(object = out, value = get_config(out)[renoir_trainer_default_hyperparameters("radial_SVM")])

  #return
  return(out)
}

#Sigmoid SVM
renoir_sigmoid_SVM <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){
  #get args
  args = c(as.list(environment()), list(...));

  #fix kernel
  args$kernel = 'sigmoid'
  args$use.nusvm = FALSE

  #call fun
  out = do.call(what = renoir_svm, args = args)

  #update object
  out = set_learning_method(object = out, value = "sigmoid_SVM")
  out = set_config(object = out, value = get_config(out)[renoir_trainer_default_hyperparameters("sigmoid_SVM")])


  #return
  return(out)
}

#Linear SVM
renoir_linear_NuSVM <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){
  #get args
  args = c(as.list(environment()), list(...));

  #fix kernel
  args$kernel = 'linear'
  args$use.nusvm = T

  #call fun
  out = do.call(what = renoir_svm, args = args)

  #update object
  out = set_learning_method(object = out, value = "linear_NuSVM")
  out = set_config(object = out, value = get_config(out)[renoir_trainer_default_hyperparameters("linear_NuSVM")])

  #return
  return(out)
}

#Polynomial SVM
renoir_polynomial_NuSVM <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){
  #get args
  args = c(as.list(environment()), list(...));

  #fix kernel
  args$kernel = 'polynomial'
  args$use.nusvm = T

  #call fun
  out = do.call(what = renoir_svm, args = args)

  #update object
  out = set_learning_method(object = out, value = "polynomial_NuSVM")
  out = set_config(object = out, value = get_config(out)[renoir_trainer_default_hyperparameters("polynomial_NuSVM")])

  #return
  return(out)
}

#Radial SVM
renoir_radial_NuSVM <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){
  #get args
  args = c(as.list(environment()), list(...));

  #fix kernel
  args$kernel = 'radial'
  args$use.nusvm = T

  #call fun
  out = do.call(what = renoir_svm, args = args)

  #update object
  out = set_learning_method(object = out, value = "radial_NuSVM")
  out = set_config(object = out, value = get_config(out)[renoir_trainer_default_hyperparameters("radial_NuSVM")])

  #return
  return(out)
}

#Sigmoid SVM
renoir_sigmoid_NuSVM <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){
  #get args
  args = c(as.list(environment()), list(...));

  #fix kernel
  args$kernel = 'sigmoid'
  args$use.nusvm = T

  #call fun
  out = do.call(what = renoir_svm, args = args)

  #update object
  out = set_learning_method(object = out, value = "sigmoid_NuSVM")
  out = set_config(object = out, value = get_config(out)[renoir_trainer_default_hyperparameters("sigmoid_NuSVM")])

  #return
  return(out)
}


#'Generalized Boosted Regression Modeling (GBM)
#'
#'@param x the input matrix, where rows are observations and columns are variables
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}
#'@param weights priors of the observations
#'@param offset used only for GLM methods, it is an a priori known component to be included in the linear predictor during fitting
#'@param resp.type the response type
#'@param observations indices of observations to keep
#'@param features indices of predictors to keep
#'@param ... further arguments to \code{\link[gbm]{gbm.fit}}
#'@param distribution either a character string specifying the name of the distribution to use or
#'a list with a component name specifying the distribution and any additional parameters needed.
#'If not specified, it is set by \code{resp.type}
#'@param ntree the total number of trees to fit. This is equivalent to the
#'number of iterations and the number of basis functions in the additive expansion.
#'@param eta The shrinkage parameter applied to each tree in the expansion.
#'Also known as the learning rate or step-size reduction; 0.001 to 0.1 usually work,
#'but a smaller learning rate typically requires more trees. Default is 0.1.
#'@param keep.data logical, whether or not to keep the data and an index of the data stored with the object.
#'@inheritParams gbm::gbm.fit
#'
#'@return An object of class \linkS4class{Trained}
#'
#'@author Alessandro Barberis
#'
#'@export
renoir_gbm <- function(
  x,
  y = NULL,
  weights = NULL,
  offset = NULL,
  resp.type,
  observations = NULL,
  features = NULL,
  clean = FALSE,
  keep.call = TRUE,
  ...,
  eta, ntree,
  keep.data = FALSE,
  # distribution,
  verbose = F
){

  #--------------------------------------------------------------------------------------------#
  #Subset observations
  x       = subset_observations(object = x, which = observations)
  y       = subset_observations(object = y, which = observations)
  offset  = subset_observations(object = offset, which = observations)
  weights = subset_observations(object = weights, which = observations)

  #subset features
  x = subset_features(object = x, which = features)

  #--------------------------------------------------------------------------------------------#
  #set fun
  fun = gbm::gbm.fit;

  #--------------------------------------------------------------------------------------------#
  #objective
  # if(isTRUE(missing(distribution) || is.null(distribution))){
    distribution = switch(
      resp.type,
      'gaussian'    = 'gaussian',
      'binomial'    = 'bernoulli',
      'cox'         = 'coxph',
      'poisson'     = 'poisson'
    )
  # }

  #--------------------------------------------------------------------------------------------#
  #check y
  if(isTRUE((identical(resp.type, "binomial") || identical(resp.type, "multinomial")))){
    #check if matrix
    if(isTRUE(is.null(dim(y)) && !is.factor(y))){
      y = factor(y)

    } else if(isTRUE(!is.null(dim(y)))){
      y = dummy.matrix.to.factor(y)
    }

    #store classes
    classes = levels(y)

    #update y
    y = as.numeric(y) - 1#function expect y in [0, 1, ..., nclasses - 1]
  } else {classes = NULL}

  #--------------------------------------------------------------------------------------------#
  #get formals
  args.def = names(formals(fun = fun))

  #get provided
  args = c(
    list(
      x = x,
      y = y,
      offset = offset,
      w = weights,
      keep.data = keep.data,
      distribution = distribution,
      verbose = verbose),
    list(...)
  );

  #update
  args$n.trees   = ntree
  args$shrinkage = eta
  if(isTRUE(is.null(args$var.names))){args$var.names = colnames(x)}

  #check passed arguments
  if(any(!(names(args) %in% args.def))){
    # warning("Provided argument not found in formals\n")
    warning(paste("Provided argument not found in formals:",
            paste(setdiff(x = names(args), y = args.def), collapse = ", "),
            "\n"))
  }

  #--------------------------------------------------------------------------------------------#
  #train
  out = do.call(what = fun, args = args)

  #--------------------------------------------------------------------------------------------#
  #clean
  if(isFALSE(keep.call)){
    out$call = NULL
  }

  if(isTRUE(clean)){
    out$data = NULL
  }

  #--------------------------------------------------------------------------------------------#
  #update
  out$classes = classes

  #--------------------------------------------------------------------------------------------#
  #Set Trained object
  out = Trained(
    fit = out,
    learning.method = "gbm",
    config = list(eta = eta, ntree = ntree),
    nfeatures = get_nrecords(records = record_gbm(out))
  )

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}


#Extreme Gradient Boosting
#@param alpha L1 regularization term on weights
#@param lambda L2 regularization term on weights
#@param eta control the learning rate (range is 0 < eta < 1)
#@param ... further arguments to \code{\link{xgb.train}} \code{params}
renoir_xgboost = function(
  x,
  y = NULL,
  weights = NULL,
  offset = NULL,
  resp.type,
  observations = NULL,
  features = NULL,
  clean = FALSE,
  keep.call = TRUE,
  ...,
  nthread = 1L,
  alpha = 0,
  lambda = 0,
  nrounds = 1000L,
  eta = 0.3,
  verbose = 0
  ){

  #--------------------------------------------------------------------------------------------#
  #Subset observations
  x       = subset_observations(object = x, which = observations)
  y       = subset_observations(object = y, which = observations)
  # offset  = subset_observations(object = offset, which = observations)
  weights = subset_observations(object = weights, which = observations)

  #subset features
  x = subset_features(object = x, which = features)

  #--------------------------------------------------------------------------------------------#
  #set fun
  # fun = xgboost:::xgb.train;
  fun = xgboost:::xgboost;

  #--------------------------------------------------------------------------------------------#
  #objective
  objective = switch(
    resp.type,
    'gaussian'    = 'reg:squarederror',
    'binomial'    = 'binary:logistic',
    'multinomial' = 'mult:softprob',
    'cox'         = 'survival:cox',
    'poisson'     = 'count:poisson'
  )

  #--------------------------------------------------------------------------------------------#
  #check y
  if(isTRUE((identical(resp.type, "binomial") || identical(resp.type, "multinomial")))){
    #check if matrix
    if(isTRUE(is.null(dim(y)) && !is.factor(y))){
      y = factor(y)

    } else if(isTRUE(!is.null(dim(y)))){
      y = dummy.matrix.to.factor(y)
    }

    #store classes
    classes = levels(y)

    #update y
    y = as.numeric(y) - 1#function expect y in [0, 1, ..., nclasses - 1]
  } else {classes = NULL}

  #--------------------------------------------------------------------------------------------#
  #get formals
  args.def = names(formals(fun = fun))

  #get provided
  args = c(list(
    data = x,
    label = y,
    weight = weights,
    nrounds = nrounds,
    params = c(list(
      eta = eta,
      nthread = nthread,
      alpha = alpha,
      lambda = lambda,
      objective = objective),
      list(...)),
    verbose = verbose
    ));

  #check passed arguments
  if(any(!(names(args) %in% args.def))){
    # warning("Provided argument not found in formals\n")
    warning("Provided argument not found in formals:",
            paste(setdiff(x = names(args), y = args.def), collapse = ", "),
            "\n")
  }

  #--------------------------------------------------------------------------------------------#
  #learning method
  l.method = args$params$booster
  l.method = switch(
    l.method,
    'gbtree'   = 'xgbtree',
    'gblinear' = 'xgblinear'
  )

  #--------------------------------------------------------------------------------------------#
  #train
  out = do.call(what = fun, args = args)

  #--------------------------------------------------------------------------------------------#
  #clean
  if(isFALSE(keep.call)){
    out$call = NULL
  }

  if(isTRUE(clean)){
    #do nothing - currently not implemented
  }

  #--------------------------------------------------------------------------------------------#
  #update
  out$classes = classes

  #--------------------------------------------------------------------------------------------#
  #Set Trained object
  out = Trained(
    fit = out,
    learning.method = l.method,
    config = list(eta = eta, nrounds = nrounds, alpha = alpha, lambda = lambda),
    nfeatures = get_nrecords(records = record_xgboost(out))
  )

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}


#Tree Booster
renoir_xgbtree <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){
  #get args
  args = c(as.list(environment()), list(...));

  #fix booster
  args$booster = 'gbtree'

  #call renoir_xgboost
  out = do.call(what = renoir_xgboost, args = args)

  #return
  return(out)
}

#Linear Booster
renoir_xgblinear <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){
  #get args
  args = c(as.list(environment()), list(...));

  #fix booster
  args$booster = 'gblinear'

  #call renoir_xgboost
  out = do.call(what = renoir_xgboost, args = args)

  #return
  return(out)
}


#'Random Forest
#'
#'@param x the input matrix, where rows are observations and columns are variables
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}
#'@param weights priors of the observations
#'@param offset used only for GLM methods, it is an a priori known component to be included in the linear predictor during fitting
#'@param resp.type the response type
#'@param observations indices of observations to keep
#'@param features indices of predictors to keep
#'@param ... further arguments to \code{\link[randomForest]{randomForest}}
#'@inheritParams randomForest::randomForest
#'
#'@return An object of class \linkS4class{Trained}
#'
#'@author Alessandro Barberis
#'
#'@export
renoir_random_forest = function(
  x,
  y = NULL,
  weights = NULL,
  offset = NULL,
  resp.type,
  observations = NULL,
  features = NULL,
  ntree = 500,
  clean = FALSE,
  keep.call = TRUE,
  ...){

  #--------------------------------------------------------------------------------------------#
  #Subset observations
  x       = subset_observations(object = x, which = observations)
  y       = subset_observations(object = y, which = observations)
  # offset  = subset_observations(object = offset, which = observations)
  weights = subset_observations(object = weights, which = observations)

  #subset features
  x = subset_features(object = x, which = features)

  #--------------------------------------------------------------------------------------------#
  #set fun
  fun = randomForest:::randomForest.default

  #--------------------------------------------------------------------------------------------#
  #get formals
  args.def = names(formals(fun = fun))

  #get provided
  args = c(list(x = x, y = y, classwt = weights, ntree = ntree), list(...));

  #check passed arguments
  if(any(!(names(args) %in% args.def))){
    # warning("Provided argument not found in formals\n")
    warning("Provided argument not found in formals:",
            paste(setdiff(x = names(args), y = args.def), collapse = ", "),
            "\n")
  }

  #--------------------------------------------------------------------------------------------#
  #train
  out = do.call(what = fun, args = args)

  #--------------------------------------------------------------------------------------------#
  #clean
  if(isFALSE(keep.call)){
    out$call = NULL
  }

  if(isTRUE(clean)){
    #do nothing - currently not implemented
  }

  #--------------------------------------------------------------------------------------------#
  #Set Trained object
  out = Trained(
    fit = out,
    learning.method = "randomForest",
    config = list(ntree = ntree),
    nfeatures = get_nrecords(records = record_randomForest(out))
  )

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}

#'GLM with elastic-net
#'
#'@param x the input matrix, where rows are observations and columns are variables
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}
#'@param weights priors of the observations
#'@param offset used only for GLM methods, it is an a priori known component to be included in the linear predictor during fitting
#'@param resp.type the response type
#'@param observations indices of observations to keep
#'@param features indices of predictors to keep
#'@param gamma dummy variable, not used in training but set in config
#'@param ... further arguments to \code{\link[glmnet]{glmnet}}
#'@inheritParams glmnet::glmnet
#'
#'@return An object of class \linkS4class{Trained}
#'
#'@author Alessandro Barberis
#'
#'@export
renoir_glmnet = function(
  x, y, weights = NULL, offset = NULL, clean, keep.call,
  alpha = 1,
  relax = F,
  gamma = 1,
  resp.type,
  observations = NULL,
  features = NULL,
  learning.method,
  ...
){

  #--------------------------------------------------------------------------------------------#
  #Subset observations
  x       = subset_observations(object = x, which = observations)
  y       = subset_observations(object = y, which = observations)
  offset  = subset_observations(object = offset, which = observations)
  weights = subset_observations(object = weights, which = observations)

  #subset features
  x = subset_features(object = x, which = features)

  #--------------------------------------------------------------------------------------------#
  # args = c(as.list(environment()), list(...));
  args = list(...)

  #--------------------------------------------------------------------------------------------#
  #check family
  if(is.null(args$family)){
    args$family = resp.type
  }

  #--------------------------------------------------------------------------------------------#
  #if only one lambda is provided, use a path
  lambda = unlist(args$lambda)
  if(!is.null(lambda) && length(lambda)==1){
    #get default sequence
    s = renoir_default_arg_glmnet(name = "lambda")
    #set lambda
    #NOTE: due to precision lambda could be added even if already in s
    s = unique(rev(sort(c(s, lambda))))
    #update
    args$lambda = s
    #set flag
    check.obj = TRUE
    split.obj = T#if one single lambda is provided, not split as the single lambda will be in trained config

    #keep data as should be final fit
    if(missing(clean)){clean = FALSE}
    if(missing(keep.call)){keep.call = T}
  } else {
    args$lambda = lambda

    check.obj = FALSE
    #if multiple lambdas are provided, split
    split.obj = T
    #remove data as should be tuning fit
    if(missing(clean)){clean = TRUE}
    if(missing(keep.call)){keep.call = F}
  }

  #--------------------------------------------------------------------------------------------#
  #set fun
  fun = glmnet::glmnet

  #get formals
  args.def = names(formals(fun = fun))

  #get provided
  args = c(list(x = x, y = y, weights = weights, offset = offset, alpha = alpha, relax = relax), args);

  #check passed arguments
  if(any(!(names(args) %in% args.def))){
    # warning("Provided argument not found in formals\n")
    warning("Provided argument not found in formals:",
            paste(setdiff(x = names(args), y = args.def), collapse = ", "),
            "\n")
  }

  #--------------------------------------------------------------------------------------------#
  #train
  trained = do.call(what = fun, args = args)

  #--------------------------------------------------------------------------------------------#
  #check provided lambda was used
  if(check.obj){
    #get used lambda sequence
    s = trained$lambda
    #check
    if(!(lambda %in% s)){
      # warning(paste0("Used lambda sequence:\n", paste(formatC(x = s, digits = 4,  format = "f", drop0trailing = T), collapse = " "),"\n"))
      # warning(paste0("Provided lambda (",formatC(x = lambda, digits = 4, format = "f", drop0trailing = T),") not used in fit. Re-fit by using one single lambda instead of sequence.\n"))
      warning(paste0("Provided lambda not used in fit. Re-fit by using one single lambda instead of sequence.\n"))
      #update
      args$lambda = lambda
      trained = do.call(what = fun, args = args)
      split.obj = F
    }
  }

  #--------------------------------------------------------------------------------------------#
  #remove call from object (if present) to reduce space before splitting
  if(!keep.call){
    trained$call = NULL

    if(relax){
      trained$relaxed$call = NULL
    }
  }

  #clean object to reduce space before splitting
  if(clean){
    trained = clean_glmnet(trained)
  }

  #--------------------------------------------------------------------------------------------#
  #learning method
  if(missing(learning.method)){
    learning.method = if(alpha==1){"lasso"}else if(alpha==0){"ridge"}else{"elasticnet"}

    if(relax){
      learning.method = paste("relaxed",learning.method, sep = "_")
    }
  }

  #--------------------------------------------------------------------------------------------#
  #Split
  if(split.obj){
    trained = renoir_split_glmnet(object = trained)

    #--------------------------------------------------------------------------------------------#
    #If only one lambda was provided keep the configuration of that lambda
    if(!is.null(lambda) && length(lambda)==1){
      lambdas = lapply(X = trained, FUN = "[[", 'lambda')
      #check
      id = which(lambdas == lambda)
      #due to rounding error, more than one could be found (e.g. because lambda was added to s, even it was already there)
      #be sure to keep one
      id = id[1]
      #subset
      trained = trained[[id]]
    }
  }

  #check if class is RenoirSplitList
  if(!identical(class(trained), "RenoirSplitList")){
    trained = list(el1 = trained)
  }

  #--------------------------------------------------------------------------------------------#
  out = list()

  # for(itrained in trained){
  #
  #   #set config
  #   config = list(alpha = alpha, lambda = itrained$lambda)
  #
  #   if(relax){
  #     if(length(gamma)>1){
  #       outlist = list()
  #       for(igamma in gamma){
  #         outlist = c(outlist, Trained(fit = out, learning.method = learning.method, config = c(config, list(gamma = igamma))))
  #       }
  #       out = c(out, outlist)
  #       rm(outlist)
  #     } else {
  #       #update config with gamma
  #       config = c(config, list(gamma = gamma))
  #       #set
  #       out = c(out, Trained(fit = itrained, learning.method = learning.method, config = config))
  #     }
  #
  #   } else {
  #     #set
  #     out = c(out, Trained(fit = itrained, learning.method = learning.method, config = config))
  #   }
  #
  # }

  for(itrained in trained){
    #set config
    config = list(alpha = alpha, lambda = itrained$lambda)

    #set
    out = c(out, Trained(fit = itrained, learning.method = learning.method, config = config, nfeatures = get_nrecords(records = record_glmnet(itrained))))
  }

  if(relax){
    #make sure gamma is a vector
    gamma = unlist(gamma)

    if(length(gamma)>1){
      outlist = list()
      for(igamma in gamma){
        outlist = c(outlist,lapply(X = out, FUN = add_config_element, value = list(gamma = igamma)))
      }
      out = c(outlist)
      rm(outlist)
    } else {
      # #update config with gamma
      # config = c(config, list(gamma = gamma))
      #set
      out = lapply(X = out, FUN = add_config_element, list(gamma = gamma))
    }
  }

  #--------------------------------------------------------------------------------------------#
  if(length(out)>1){
    out = TrainedList(out)
  } else {
    out = out[[1]]
  }
  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
}


renoir_lasso <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){

  #get args
  args = c(as.list(environment()), list(...));

  #fix alpha
  args$alpha = 1

  #set learning method name
  # args$learning.method = "lasso"

  #call renoir_glmnet
  out = do.call(what = renoir_glmnet, args = args)

  #return
  return(out)
}

renoir_ridge <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){

  #get args
  args = c(as.list(environment()), list(...));

  #fix alpha
  args$alpha = 0

  #set learning method name
  # args$learning.method = "ridge"

  #call renoir_glmnet
  out = do.call(what = renoir_glmnet, args = args)

  #return
  return(out)

}

renoir_elnet <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){

  #get args
  args = c(as.list(environment()), list(...));


  #check alpha
  alpha = args$alpha
  #fix alpha
  if(is.null(alpha)){
    #set to default
    args$alpha = 0.5
  } else {
    if(!(alpha>=0 && alpha<=1)){
      warning("Provided hyperparameter not matching learning method. Hyperparameter forced to default value.\n")
      args$alpha = 0.5
    }
  }
  rm(alpha)

  #set learning method name
  args$learning.method = "elasticnet"

  #call renoir_glmnet
  out = do.call(what = renoir_glmnet, args = args)

  #return
  return(out)
}

renoir_relaxed_lasso <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){

  #get args
  args = c(as.list(environment()), list(...));

  #fix alpha
  args$alpha = 1

  #set relax
  args$relax = TRUE

  #set learning method name
  # args$learning.method = "relaxed_lasso"

  #call renoir_glmnet
  out = do.call(what = renoir_glmnet, args = args)

  #return
  return(out)
}

renoir_relaxed_ridge <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){

  #get args
  args = c(as.list(environment()), list(...));

  #fix alpha
  args$alpha = 0

  #set relaxed
  args$relax = TRUE

  #set learning method name
  # args$learning.method = "relaxed_ridge"

  #call renoir_glmnet
  out = do.call(what = renoir_glmnet, args = args)

  #return
  return(out)

}

renoir_relaxed_elnet <- function(x, y, weights = NULL, offset = NULL, clean, keep.call, resp.type, observations, features, ...){

  #get args
  args = c(as.list(environment()), list(...));

  #set relaxed
  args$relax = TRUE

  #fix alpha
  if(is.null(args$alpha)){
    args$alpha = 0.5
  }

  #set learning method name
  # args$learning.method = "relaxed_elnet"
  args$learning.method = "relaxed_elasticnet"

  #call renoir_glmnet
  out = do.call(what = renoir_glmnet, args = args)

  #return
  return(out)
}


#'Split glmnet object
#'@description Split the output object of a \code{\link[glmnet]{glmnet}} function call to have
#'one configuration (i.e. one lambda value) per object. It updates the main
#'slots of the object (\code{a0, beta, dfmat, df, dev.ratio, lambda, dim}) accordingly.
#'\code{npasses} is not updated.
#'@param object the output of a \code{\link[glmnet]{glmnet}} function call
#'@return a list of glmnet objects
#'@keywords internal
renoir_split_glmnet <- function(object){

  #get lambda
  lambda = object$lambda

  #get number of config
  nlambda = length(lambda)

  #check if relaxed
  relaxed = !is.null(object$relaxed)

  if(nlambda > 1){

    #check if is multi response
    is.multi = is.list(object$beta)

    #list
    out = list()

    #loop and split
    for(i in seq(nlambda)){

      #subset
      tmp = renoir_subset_glmnet(object = object, i = i)

      #if relaxed subset
      if(relaxed){
        tmp$relaxed = renoir_subset_glmnet(object = object$relaxed, i = i)
      }

      #store
      out[[i]] = tmp

    }

    #add a class to recognise
    class(out) = c("RenoirSplitList")

  } else {
    out = object
  }

  return(out)
}

#'Subset glmnet object
#'@param object a glmnet fit
#'@param i lambda
#'@return Modified object.
#'@keywords internal
renoir_subset_glmnet <- function(object, i){

  #check if is multi response
  is.multi = is.list(object$beta)

  #sub
  if(is.multi){
    #intercept
    object$a0 = object$a0[,i,drop=F]
    #coefficients
    object$beta = lapply(X = object$beta, FUN = "[", i, drop = F)
    #number of nonzero coefficients per class
    object$dfmat = object$dfmat[,i,drop=F]
  }else{
    #intercept
    object$a0 = object$a0[i]
    #coefficients
    object$beta = object$beta[,i,drop=F]
  }

  #Degrees of freedom
  if(!is.function(object$df)){
    object$df = object$df[i]
  }

  #Fraction of null deviance explained
  object$dev.ratio = object$dev.ratio[i]

  #Lambda
  object$lambda    = object$lambda[i]

  #dimension of coefficient matrix (ices)
  object$dim    = c(object$dim[1], 1)

  #return
  return(object)
}

glmnet_learning_method_names <- function(all = T, relaxed = T){

  # out = c("lasso", "ridge", "elnet", "elasticnet")
  # out.r = c("relaxed_lasso", "relaxed_ridge", "relaxed_elnet", "relaxed_elasticnet")

  out = c("lasso", "ridge", "elasticnet")
  out.r = c("relaxed_lasso", "relaxed_ridge", "relaxed_elasticnet")

  if(all){
    out = c(out, out.r)
  } else {
    if(relaxed){
      out = out.r
    }
  }

  return(out)
}

supported_learning_methods <- function(){

  #GLMNET
  out = glmnet_learning_method_names(all = T)

  #OTHERS
  out = c(out, "randomForest", "gbm",
          # "xgbtree", "xgblinear",
          "linear_SVM", "polynomial_SVM", "radial_SVM", "sigmoid_SVM",
          "linear_NuSVM", "polynomial_NuSVM", "radial_NuSVM", "sigmoid_NuSVM",
          "gknn", "nsc"
  )

  return(out)
}

renoir_learning_methods <- function(){
  out = supported_learning_methods()

  return(out)
}

#'Get default arguments for glmnet call
#'@return a list with default argument for \code{\link[glmnet]{glmnet}} call
#'@keywords internal
renoir_default_arg_glmnet <- function(name){
  #default
  lambda = 10^seq(3, -2, length=100)
  gamma  = c(0, 0.25, 0.5, 0.75, 1)

  #set out list
  out = list(lambda = lambda, gamma = gamma)

  #if name is provided
  if(!missing(name)){
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




#'@keywords internal
get_name_learning_method_default <- function(learning.method){
  #Get the learning method
  learning.method = switch(
    learning.method,
    glmnet             = "generalized linear model via penalized maximum likelihood",
    lasso              = "generalized linear model via L1 penalized maximum likelihood (lasso penalty)",
    ridge              = "generalized linear model via L2 penalized maximum likelihood (ridge penalty)",
    elasticnet         = "generalized linear model via L1/L2 penalized maximum likelihood (elasticnet penalty)",
    relaxed_lasso      = "generalized linear model via L1 penalized maximum likelihood (relaxed lasso penalty)",
    relaxed_ridge      = "generalized linear model via L2 penalized maximum likelihood (relaxed ridge penalty)",
    relaxed_elasticnet = "generalized linear model via L1/L2 penalized maximum likelihood (relaxed elasticnet penalty)",
    randomForest       = "random forest",
    xgbtree            = "extreme gradient tree boosting model",
    xgblinear          = "extreme gradient linear boosting model",
    gbm                = "generalized boosted model",
    linear_SVM         = "linear support vector machine",
    polynomial_SVM     = "polynomial support vector machine",
    radial_SVM         = "radial support vector machine",
    sigmoid_SVM        = "sigmoid support vector machine",
    linear_NuSVM       = "linear nu-type support vector machine",
    polynomial_NuSVM   = "polynomial nu-type support vector machine",
    radial_NuSVM       = "radial nu-type support vector machine",
    sigmoid_NuSVM      = "sigmoid nu-type support vector machine",
    gknn               = "generalized k-nearest neighbours model",
    nsc                = "nearest shrunken centroid model",
    ""
  );

  return(learning.method)
}

get_name_learning_method <- function(learning.method){

  out = sapply(X = learning.method, FUN = get_name_learning_method_default)

  #ret
  return(out)
}

renoir_trainer_default_hyperparameters <- function(learning.method){
  #Get the learning method
  learning.method = switch(
    learning.method,
    glmnet             = c("lambda", "alpha", "gamma"),
    lasso              = c("lambda"),
    ridge              = c("lambda"),
    elasticnet         = c("lambda", "alpha"),
    relaxed_lasso      = c("lambda", "gamma"),
    relaxed_ridge      = c("lambda", "gamma"),
    relaxed_elasticnet = c("lambda", "alpha", "gamma"),
    randomForest       = c("ntree"),
    xgbtree            = c("nrounds", "eta", "lambda", "alpha"),
    xgblinear          = c("nrounds", "eta", "lambda", "alpha"),
    gbm                = c("eta", "ntree"),
    linear_SVM         = c("cost"),
    polynomial_SVM     = c("cost", "gamma", "degree"),
    radial_SVM         = c("cost", "gamma"),
    sigmoid_SVM        = c("cost", "gamma"),
    linear_NuSVM       = c("nu"),
    polynomial_NuSVM   = c("nu", "gamma", "degree"),
    radial_NuSVM       = c("nu", "gamma"),
    sigmoid_NuSVM      = c("nu", "gamma"),
    gknn               = c("k"),
    nsc                = c("threshold"),
    ""
  );

  return(learning.method)
}

renoir_default_hyperparameters <- function(learning.method){

  out = sapply(X = learning.method, FUN = renoir_trainer_default_hyperparameters)
  # out = sapply(X = learning.method, FUN = renoir_trainer_default_hyperparameters, simplify = FALSE)

  #ret
  return(out)
}

#'Supported Learning Methods
#'
#'@description This function returns a \code{data.frame} containing the currently
#'supported learning methods.
#'
#'@return A \code{data.frame} with 3 columns:
#'\describe{
#'\item{\code{id}}{contains the id used in renoir for the learning method}
#'\item{\code{method}}{contains the name of the learning method (e.g. 'random forest')}
#'\item{\code{default_hyperparameters}}{containg the name of the hyperparameters used by default}
#'}
#'
#'@export
#'
#'@author Alessandro Barberis
list_supported_learning_methods <- function(){

  #get ids
  ids = supported_learning_methods()

  #get names
  nm = get_name_learning_method(learning.method = ids)

  #get hp
  hp = renoir_default_hyperparameters(learning.method = ids)

  #create df
  out = data.frame(id = names(nm), method = nm, row.names = NULL, stringsAsFactors = F)

  #add hp
  out$default_hyperparameters = hp

  #r
  return(out)
}
