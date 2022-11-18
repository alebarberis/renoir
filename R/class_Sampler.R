#' @include classes_generics.R class_Logger.R utils.R
NULL

#' Sampler Class
#'
#' @description
#' An S4 class to represent a sampling methodology.
#'
#' The object consists of 7 slots
#' @slot method The sampling strategy to use. The available options are:
#' \describe{
#'   \item{\code{'random'}}{random sampling}
#'   \item{\code{'cv'}}{k-fold cross-validation}
#'   \item{\code{'bootstrap'}}{bootstrap (sampling with replacement)}
#' }
#' @slot N a length-one integer vector, the population size
#' @slot n a length-one integer vector, the sample size for \code{method = "random"} and \code{method = "bootstrap"}
#' @slot k a length-one integer vector, the number of folds for \code{method = "cv"},
#' or the number of repeats for \code{method = "random"} and \code{method = "bootstrap"}
#' @slot strata (optional) vector of stratification variables. If provided must be of length \code{N}
#' @slot balance a length-one logical vector, whether to (try to) balance the strata frequency in the output sample.
#' Used only if \code{strata} is provided when \code{method = "random"} or \code{method = "bootstrap"}
#' @slot logger a \linkS4class{Logger}
#'@author Alessandro Barberis
methods::setClass(
  Class = "Sampler",
  slots = c(
    method  = "character",
    N       = "integer",
    n       = "integer",
    k       = "integer",
    strata  = "ANY",
    balance = "logical",
    logger  = "Logger"
  )
)

#' Sampler Class Constructor
#'
#' @description
#' Constructor for the S4 \linkS4class{Sampler} object.
#'
#' @param method The sampling strategy to use. The available options are:
#' \describe{
#'   \item{\code{'random'}}{random sampling}
#'   \item{\code{'cv'}}{k-fold cross-validation}
#'   \item{\code{'bootstrap'}}{bootstrap (sampling with replacement)}
#' }
#' @param N the population size
#' @param n the sample size for \code{method = "random"} and \code{method = "bootstrap"}
#' @param k the number of folds for \code{method = "cv"},
#' or the number of repeats for\code{method = "random"} and \code{method = "bootstrap"}
#' @param strata (optional) vector of stratification variables. If provided must be of length \code{N}
#' @param balance logical, whether to (try to) balance the strata frequency in the output sample.
#' Used only if \code{strata} is provided when \code{method = "random"} or \code{method = "bootstrap"}
#' @param logger a \linkS4class{Logger}
#'
#' @return a \linkS4class{Sampler} object
#'
#'@author Alessandro Barberis
#'
#' @export
Sampler <- function(
  method  = c("random", "bootstrap", "cv"),
  # N       = 100L,
  N       = integer(),
  n       = 10L,
  k       = 10L,
  strata  = NULL,
  balance = FALSE,
  logger  = Logger()
){

  # if(missing(N)){stop("'N' must be provided.\n")}

  method = match.arg(method)

  methods::new(
    Class = "Sampler",
    method    = method,
    N         = N,
    n         = n,
    k         = k,
    strata    = strata,
    balance   = balance,
    logger    = logger
  )
}


methods::setMethod(f = "get_method",   signature = "Sampler", definition = function(object){methods::slot(object = object, name = 'method')})
methods::setMethod(f = "get_N",        signature = "Sampler", definition = function(object){methods::slot(object = object, name = 'N')})
methods::setMethod(f = "get_n",        signature = "Sampler", definition = function(object){methods::slot(object = object, name = 'n')})
methods::setMethod(f = "get_k",        signature = "Sampler", definition = function(object){methods::slot(object = object, name = 'k')})
methods::setMethod(f = "get_balance",  signature = "Sampler", definition = function(object){methods::slot(object = object, name = 'balance')})
methods::setMethod(f = "get_strata",   signature = "Sampler", definition = function(object){methods::slot(object = object, name = 'strata')})
methods::setMethod(f = "get_logger",   signature = "Sampler", definition = function(object){methods::slot(object = object, name = 'logger')})


methods::setMethod(f = "set_N",        signature = "Sampler", definition = function(object, value){methods::slot(object = object, name = 'N') = value; return(object)})
methods::setMethod(f = "set_n",        signature = "Sampler", definition = function(object, value){methods::slot(object = object, name = 'n') = value; return(object)})
methods::setMethod(f = "set_strata",   signature = "Sampler", definition = function(object, value){methods::slot(object = object, name = 'strata') = value; return(object)})


is_valid_sampler_object <- function(object) {
  errors <- character()

  #id
  id = get_method(object)
  #N
  N = get_N(object)
  #n
  n = get_n(object)
  #k
  k = get_k(object)
  #strata
  strata = get_strata(object)


  if (identical(id, "cv") && (k > N)) {
    msg <- paste("Number of folds 'k' greater than population size 'N'. ", sep = "")
    errors <- c(errors, msg)
  }

  if (identical(id, "cv") && (k < 3)) {
    msg <- paste("Number of folds 'k' lower than 3 (i.e. the smallest value allowable of folds). ", sep = "")
    errors <- c(errors, msg)
  }

  if (identical(id, "random") && (n > N)) {
    msg <- paste("Sample size 'n' greater than population size 'N'. ", sep = "")
    errors <- c(errors, msg)
  }

  if ( !is.null(strata) && (get_nobs(strata) != N) ){
    msg <- paste("Strata not matching population size 'N'. ", sep = "")
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}

is_multi_sample <- function(object){
  #id
  id = get_method(object)
  #N
  N = get_N(object)
  #n
  n = get_n(object)
  #k
  k = get_k(object)

  out = switch(
    id,
    'cv'        = (k > 2),
    'random'    = k > 1,
    'bootstrap' = k > 1
  )

  return(out)

}

methods::setMethod(
  f = "subset_observations",
  signature = methods::signature(object = "Sampler"),
  definition = function(object, which){

    strata = get_strata(object = object)
    # N = get_N(object = object)

    #----------------------------------------------------------------------#
    #Subset strata
    if(!is.null(strata)){
      if (is.matrix(strata)){
        strata = strata[which, , drop = FALSE]
      } else {
        strata = strata[which]
      }

      #Update
      object = set_strata(object = object , value = strata)
    }

    #----------------------------------------------------------------------#
    #Update N
    N = if(is.logical(which)){
      sum(which)
    } else {
      length(which)
    }

    #set N
    object = set_N(object = object, value = N)

    #----------------------------------------------------------------------#
    #return
    return(object)
  }
)


#'Resample
#'@description Takes a sample with or without replacement from the population.
#'@return A \code{list} containing the samples
methods::setMethod(
  f = "resample",
  signature = "Sampler",
  definition = function(object){
    method = get_method(object)
    N = get_N(object)
    n = get_n(object)
    k = get_k(object)
    strata = get_strata(object)
    balance = get_balance(object)

    if(method %in% c("random", "bootstrap")){
      out = switch(
        method,
        "random"    = replicate(n = k, expr = rsample(N = N, n = n, strata = strata, replace = FALSE, balance = balance), simplify = F),
        "bootstrap" = replicate(n = k, expr = rsample(N = N, n = n, strata = strata, replace = TRUE, balance = balance),  simplify = F)
      )
    } else {
      foldid = create_folds(N = N, k = k, strata = strata)

      #get max fold id
      nfolds = max(foldid)

      #out list
      out = list()

      #loop over and keep only the training sets
      for(i in seq(nfolds)){
        keep = foldid != i

        out[[i]] = which(keep)
      }

    }

    return(out)
  }
)


# methods::setMethod(
#   f = "update_size",
#   signature = "Sampler",
#   definition = function(object, value){
#     #method
#     method = get_method(object)
#     #update
#     object = switch(
#       method,
#       'random'    = set_n(object, value),
#       'bootstrap' = set_n(object, value),
#       'cv'        = set_k(object, value)
#     )
#     #return
#     return(object)
# })



# methods::setMethod(
#   f = "sample2",
#   signature = "Sampler",
#   definition = function(object){
#     method = get_method(object)
#
#     out = sample(object = object)
#
#     if(identical(method, "cv")){
#       foldid = out
#
#       #get max fold id
#       nfolds = max(foldid)
#
#       #out list
#       out = list()
#
#       #loop over and keep only the training sets
#       for(i in seq(nfolds)){
#         keep = foldid != i
#
#         out[[i]] = which(keep)
#       }
#     }
#
#     return(out)
#
#   }
# )


#'Create k folds
#'@description Assigns the population to k folds. If \code{strata} is provided, a stratified approach
#'is adopted so that the percentage of each stratum in the population is preserved in each fold.
#'
#'@param N population size
#'@param k number of folds
#'@param strata (optional) vector of stratification variables. If provided must be of length \code{N}
#'@return vector of length \code{N} containing the fold ids
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
create_folds <- function(N, k, strata = NULL){

  #-----------------------------------------------------------------------------------#
  if(missing(N)){stop("'N' must be provided.\n")}

  if(!missing(strata) && !is.null(strata)){
    if(!is.factor(strata) && !is.matrix(strata) && !is.vector(strata)){
      stop("'strata' must be in the required format.")
    } else if(length(strata)!=N){
      stop("'strata' must contain N elements.")
    }
    stratified = TRUE
  } else {
    stratified = FALSE
  }

  if(k<3){stop("Smallest value allowable of folds is n=3")}

  #-----------------------------------------------------------------------------------#
  if(!stratified){
    out = base::sample(x = rep(x = seq(k), length = N), replace = F, size = N)
  } else {

    #cross tabulation
    xtable = table(strata)

    #Get the minimum number of observations per class (element != 0)
    min.nobs.class = min(xtable)

    #Get the strata names
    stratanames = names(xtable)

    #get n of levels
    n.lvls = length(stratanames)

    #Check the minimum number of observations for each class is >= the number of folds
    if(min.nobs.class>=k){

      #Create an empty vector
      out <- numeric(length = N);

      #Loop over the levels in the factor to assign the
      #samples evenly over the classes using the modulus operator
      i=1
      for(i in 1L:n.lvls) {
        #Get the index of elements of current class
        el.index = which(strata==stratanames[i])
        #Get the number of elements for each class
        nclass = length(el.index);
        #Assign fold ids for the current class
        foldid.i <- sample.int(nclass) %% k;
        #Assign the fold to the result vector
        out[el.index] <- foldid.i;
      }

      #Sum 1 to have the ids between 1 and nfolds
      out <- out + 1;
    } else {
      stop("The minimum number of observations for each stratum must be equal or greater than the number of folds.")
    }
  }

  #-----------------------------------------------------------------------------------#
  return(out)
}

#'Random samples
#'
#'@description Takes a sample with or without replacement from the population.
#'
#'@details
#'If \code{strata} is provided, sampling is applied within each stratum, with the strategy
#'depending on \code{balance}. If \code{balance = FALSE}, the proportion of the strata
#'in the population is maintained in the samples (also called "proportionate allocation").
#'If \code{balance = TRUE}, the proportion of strata in the sample is attempted to be balanced.
#'
#'@param N population size.
#'@param n sample size.
#'@param strata (optional) vector of stratification variables. If provided must be of length \code{N}
#'@param balance logical, whether to (try to) balance the strata frequency in the output sample.
#'Used only if \code{strata} is provided.
#'@param replace logical, whether to sample with replacement. Default is \code{FALSE}.
#'Set to \code{TRUE} for bootstrap method.
#'
#'@return A vector of length \code{n} containing the index of the computed random set of observations.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
rsample <- function(N, n, strata = NULL, balance = FALSE, replace = FALSE){

  #-----------------------------------------------------------------------------------#
  if(missing(N)){stop("'N' must be provided.\n")}

  if(!missing(strata) && !is.null(strata)){
    if(!is.factor(strata) && !is.matrix(strata) && !is.vector(strata)){
      stop("'strata' must be in the required format.")
    } else if(length(strata)!=N){
      stop("'strata' must contain N elements.")
    }
    stratified = TRUE
  } else {
    stratified = FALSE
  }

  #-----------------------------------------------------------------------------------#
  if(!stratified){
    #Select n random samples from the total observations
    out = sample.int(n = N, size = n, replace = replace);
  } else {

    #cross tabulation
    xtable = table(strata)

    #Get the minimum number of observations per class (element != 0)
    min.nobs.class = min(xtable)

    #Get the strata names
    stratanames = names(xtable)

    #get n of levels
    n.strata = length(stratanames)

    #Create an empty vector
    out = NULL;

    if(!balance){
      #Loop over the strata to assign the
      #samples trying to keep the original strata frequency
      for(i in 1L:n.strata) {
        #Get the index of elements of current stratum
        el.index = which(strata==stratanames[i])
        #Get the number of elements for each stratum
        nclass = length(el.index);
        #get number of element we want
        n.per.stratum = round(n * (xtable[stratanames[i]]/N))
        #Randomly select the element we want per stratum
        index = base::sample(x = el.index, size = n.per.stratum, replace = replace)
        #update index vector
        out = c(out, index);
      }
      #force to have length n
      out = out[1:n]
    } else {

      #check
      if((n%%n.strata) != 0){
        warning("Sample size is not multiple of number of strata. Balance will be approximated.\n")
        #number of elements we want per stratum
        n.per.stratum = ceiling(n / n.strata);
        #correct
        tmp = floor(n / n.strata)
        #rep
        n.per.stratum = c(n.per.stratum, rep(x = tmp, length = (n.strata-1)))
      } else {
        #number of elements we want per stratum
        n.per.stratum = n / n.strata;
        #rep
        n.per.stratum = rep(x = n.per.stratum, length = n.strata)
      }

      #Loop over the strata to assign the samples evenly
      # i=2
      for(i in 1L:n.strata) {
        #Get the index of elements of current stratum
        el.index = which(strata==stratanames[i])
        #Randomly select the element we want per stratum
        index = base::sample(x = el.index, size = n.per.stratum[i], replace = replace)
        #update index vector
        out = c(out, index);
      }

      #force to have length n
      out = out[1:n]
    }

  }

  #-----------------------------------------------------------------------------------#
  return(out)
}


#'Bootstrap
#'@description Takes a sample with replacement from the population.
#'
#'@details This function is an interface to \code{\link{rsample}} with
#'\code{replace = TRUE}.
#'
#'@param N population size.
#'@param n sample size.
#'@param strata (optional) vector of stratification variables. If provided must be of length \code{N}
#'@param balance logical, whether to (try to) balance the strata frequency in the output sample.
#'Used only if \code{strata} is provided.
#'
#'@return A vector of length \code{n} containing the index of the computed random set of observations.
#'
#'@seealso
#'\code{\link{rsample}}
#'
#'@author Alessandro Barberis
#'@keywords internal
bootstrap <- function(N, n, strata = NULL, balance = FALSE){

  out = rsample(N = N, n = n, strata = strata, balance = balance, replace = TRUE)

}

# #'Resample
# #'@inheritParams rsample
# #'@inheritParams create_folds
# #'@return A \code{list} of length \code{k} containing the samples if
# #'\code{method = "random"} and \code{method = "bootstrap"}, a vector
# #'of length \code{N} containing the fold ids if \code{method = "cv"}
# #'@author Alessandro Barberis
# #'@keywords internal
# resample <- function(
#   method = c("multi.random", "bootstrap", "cv"),
#   k, N, n, strata = NULL, balance = FALSE){
#
#   #-----------------------------------------------------------------------------------#
#   method = match.arg(method)
#
#   #-----------------------------------------------------------------------------------#
#   if(method %in% c("multi.random", "bootstrap")){
#     out = switch(
#       method,
#       "multi.random" = lapply(X = seq(k), FUN = rsample, N = N, n = n, strata = strata, replace = FALSE),
#       "bootstrap"    = lapply(X = seq(k), FUN = rsample, N = N, n = n, strata = strata, replace = TRUE)
#     )
#   } else {
#     out = create_folds(N = N, k = k, strata = strata)
#   }
#
#   #-----------------------------------------------------------------------------------#
#   return(out)
# }


get_training_sets <- function(method = c("multi.random", "bootstrap", "cv"), ...){
  #-----------------------------------------------------------------------------------#
  method = match.arg(method)

  args = c(list(method = method), list(...))

  #-----------------------------------------------------------------------------------#
  out = do.call(what = resample, args = args)

  #-----------------------------------------------------------------------------------#
  if(identical(method, "cv")){
    foldid = out

    #get max fold id
    max.foldid = max(foldid)

    #out list
    out = list()

    #loop over and keep only the training sets
    for(i in seq(max.foldid)){
      keep = foldid != i

      out[[i]] = which(keep)
    }
  }

  #-----------------------------------------------------------------------------------#
  return(out)
}


#'Get foldid
#'@description Helper function that, given a list of indices as returned by
#'resample, reshapes the data to obtain a vector containing the fold ids.
#'@param samples list of samples as returned by resample
#'@param N population size
#'@return a vector containing the fold ids
get_foldid <- function(samples, N){
  #set output vector
  out = vector(mode = "integer", length = N);
  #get sequence
  indices = seq(N)
  #get nfolds
  nfolds = length(samples)
  #get fold
  for(i in seq(nfolds)){
    index = setdiff(x = indices, y = samples[[i]])
    #update
    out[index] = i
  }

  return(out)
}

methods::setMethod(
  f = "get_train_set_size",
  signature = methods::signature(object = "Sampler"),
  definition = function(object){
    #get method
    method = get_method(object)
    #get population size
    N = get_N(object)
    #get sample size
    n = get_n(object)
    #get folds
    k = get_k(object)
    #get size
    out = get_train_set_size_def(method = method, N = N, n = n, k = k)
    #return
    return(out)
  }
)

get_train_set_size_def <- function(method, N, n, k){
  out = switch(
    method,
    "random"    = n,
    "bootstrap" = n,
    "cv"        = N*(1 - (1/k))
  )
  return(out)
}


methods::setMethod(
  f = "set_size",
  signature = methods::signature(object = "Sampler"),
  definition = function(object, value){
    #get method
    method = get_method(object)
    #update size
    object = switch(
      method,
      random    = set_n(object = object, value = value),
      bootstrap = set_n(object = object, value = value),
      cv        = set_k(object = object, value = value)
    )
    #return
    return(object)
  }
)


methods::setMethod(
  f = "get_grid",
  signature = methods::signature(object = "Sampler"),
  definition = function(object, nmin, npoints){
    #get method
    method = get_method(object)
    #get population size
    N = get_N(object)
    #get strata
    strata = get_strata(object)
    #get balance
    balance = get_balance(object)
    #get folds
    k = get_k(object)
    #get grid
    grid = switch(
      method,
      random    = get_sample_size_grid(npoints = npoints, nmin = nmin, N = N, strata = strata, balance = balance, replace = FALSE),
      bootstrap = get_sample_size_grid(npoints = npoints, nmin = nmin, N = N, strata = strata, balance = balance, replace = TRUE),
      cv        = get_nfolds_grid(npoints = npoints, kmin = round(-N/(nmin - N)), N = N, strata = strata, balance = balance)
    )
    #set as integer
    grid = as.integer(grid)
    #return
    return(grid)
  }
)


get_training_set_size_grid <- function(
  method  = c("random", "bootstrap", "cv"),
  N       = 100L,
  n       = 10L,
  k       = 10L,
  strata        = NULL,
  balance       = FALSE,
  min.obs       = 5,
  n.grid.points = 5
  ){

  #--------------------------------------------------------------------------------------------#
  method = match.arg(method)
  #--------------------------------------------------------------------------------------------#

  grid = switch(
    method,
    random = f,
    bootstrap = f,
    cv = f
  )

  #--------------------------------------------------------------------------------------------#
  return(grid)
}


get_size_grid <- function(
  method = c("random", "bootstrap", "cv"),
  N,
  strata  = NULL,
  balance = FALSE,
  nmin,
  npoints = 5L
){
  method = match.arg(method)

  out = switch(
    method,
    random    = get_sample_size_grid(npoints = npoints, nmin = nmin, N = N, strata = strata, balance = balance, replace = FALSE),
    bootstrap = get_sample_size_grid(npoints = npoints, nmin = nmin, N = N, strata = strata, balance = balance, replace = TRUE),
    cv        = get_nfolds_grid(npoints = npoints, N = N, strata = strata, balance = balance)
  )

  return(out)
}

#'@param N integer, population size
#'@param nmin integer, desired minimum number of observations (minimum sample size)
#@param nmax maximum number of observations
get_sample_size_grid <- function(
  N,
  strata  = NULL,
  balance = FALSE,
  replace = FALSE,
  nmin,
  npoints = 5L
  # nmax
){
  #--------------------------------------------------------------------------------------------#
  if(missing(N)){stop("'N' must be provided.\n")}

  if(!missing(strata) && !is.null(strata)){
    if(!is.factor(strata) && !is.matrix(strata) && !is.vector(strata)){
      stop("'strata' must be in the required format.")
    } else if(length(strata)!=N){
      stop("'strata' must contain N elements.")
    }
    stratified = TRUE
  } else {
    stratified = FALSE
  }


  #--------------------------------------------------------------------------------------------#
  if(!stratified){
    #Select n random samples from the total observations
    # out = unique(as.integer(seq.int(from = min.obs, to = (N-1), length.out = npoints)))
    #Set max sample size
    nmax = N - 1
    #Get grid
    out = unique(as.integer(seq.int(from = nmin, to = nmax, length.out = npoints)))
  } else {

    #cross tabulation
    xtable = table(strata)

    #Get the minimum number of observations per stratum (element != 0)
    min.nobs.stratum = min(xtable, na.rm = TRUE)

    #Get the strata names
    stratanames = names(xtable)

    #get n of levels
    n.strata = length(xtable)

    if(!balance){
      # out = unique(as.integer(seq.int(from = min.obs, to = (N-1), length.out = npoints)))
      #Set max sample size
      # nmax = N - 1
      #Get the minimum number of samples needed to keep the original strata frequency
      min.nobs = round(sum(xtable/min.nobs.stratum))

      #Starting point of the sequence (minimum sample size)
      from = max(round(nmin / min.nobs), 1) * min.nobs;

      #number of times
      times = floor(N / min.nobs)

      if(N%%min.nobs==0) {
        times = times - 1
      }

      #Ending point of the sequence (maximum sample size)
      to = times * min.nobs

      # #Set max sample size
      # if(!replace){
      #   to = N - min.nobs
      # } else {
      #   to = N - 1
      # }

      #Get a sequence of allowed sizes
      allowed =  seq.int(from = from, to = to, by = min.nobs);

      #Get the number of allowed sizes
      n.grid = length(allowed);

      if(n.grid < npoints){
        #Set a warning message
        logMsg = paste0("The number of allowed training set sizes (n = ",n.grid
                        ,") is less than the number of requested grid points (",
                        npoints,"). Grid is shrunk to n.")
        warning(logMsg);

        #Update n
        npoints = n.grid;
      }

      #Select from the allowed sizes
      index = unique(as.integer(seq.int(from = 1, to = n.grid, length.out = npoints)));

      #Get the sizes
      out = allowed[index];

    } else {

      #Starting point of the sequence (minimum sample size)
      from = n.strata * nmin;

      #max n
      if(!replace){
        #We want the validation set to have at least 1 observation per stratum,
        #so we select the minimum number of observations across strata - 1
        #n obs per stratum
        obs.x.stratum = (min.nobs.stratum - 1)

      } else {
        #n obs per stratum
        obs.x.stratum = floor(N / n.strata)
        #check if one observation per class should be removed
        if(N%%n.strata==0) {
          obs.x.stratum = obs.x.stratum - 1
        }
      }

      #Ending point of the sequence (maximum sample size)
      to = obs.x.stratum * n.strata

      #Get a sequence of allowed sizes
      allowed = get_balanced_training_set_size_grid(strata = strata, from = from, to = to)

      #Get the number of allowed sizes
      n.grid = length(allowed);

      if(n.grid < npoints){
        #Set a warning message
        logMsg = paste0("The number of allowed training set sizes (n = ",n.grid
                        ,") is less than the number of requested grid points (",
                        npoints,"). Grid is shrunk to n.")
        warning(logMsg);

        #Update n
        npoints = n.grid;
      }

      #Select from the allowed sizes
      index = unique(as.integer(seq.int(from = 1, to = n.grid, length.out = npoints)));

      #Get the sizes
      out = allowed[index];

    }
  }

  #--------------------------------------------------------------------------------------------#
  return(out)
}


get_nfolds_grid <- function(
  N,
  kmin    = 2L,
  strata  = NULL,
  balance = FALSE,
  npoints = 5L
){
  #--------------------------------------------------------------------------------------------#
  if(missing(N)){stop("'N' must be provided.\n")}

  if(!missing(strata) && !is.null(strata)){
    if(!is.factor(strata) && !is.matrix(strata) && !is.vector(strata)){
      stop("'strata' must be in the required format.")
    } else if(length(strata)!=N){
      stop("'strata' must contain N elements.")
    }
    stratified = TRUE
  } else {
    stratified = FALSE
  }

  #--------------------------------------------------------------------------------------------#
  # allowed = c(2,3,4,5,7,8,9,10,20,N)
  allowed = kmin:N

  #Get the number of allowed sizes
  n.grid = length(allowed);

  if(n.grid < npoints){
    #Set a warning message
    logMsg = paste0("The number of allowed training set sizes (n = ",n.grid
                    ,") is less than the number of requested grid points (",
                    npoints,"). Grid is shrunk to n.")
    warning(logMsg);

    #Update n
    npoints = n.grid;
  }

  #Select from the allowed sizes
  index = unique(as.integer(seq.int(from = 1, to = n.grid, length.out = npoints)));

  #Get the sizes
  grid = allowed[index];

  # #--------------------------------------------------------------------------------------------#
  # if(!stratified){
  #   #Select n random samples from the total observations
  #   #Set max sample size
  #   kmax = N
  #   #Get grid
  #   out = unique(as.integer(seq.int(from = kmin, to = kmax, length.out = n.grid.points)))
  # } else {
  # }
  return(grid)
}

# get_training_set_size_grid_random <- function(
#   y,
#   min.obs = 3,
#   n.grid.points = 5,
#   balance = FALSE,
#   resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox")){
#
#   #Get maximum allowed training set size
#   max.size = get_max_training_set_size_multirandom(y = y, balance = balance, resp.type = resp.type);
#
#   #Get minimum allowed training set size
#   min.size = get_min_training_set_size_multirandom(y = y, balance = balance, min.obs = min.obs, resp.type = resp.type)
#
#
#   if(!balance){
#     #Get a sequence of allowed sizes
#     grid = unique(as.integer(seq.int(from = min.size, to = max.size, length.out = n.grid.points)))
#
#   } else {
#     #Get training set grid
#     grid = switch(resp.type,
#                   gaussian    = get_training_set_size_grid_multirandom_gaussian(y = y, from = min.size, to = max.size, n.grid.points = n.grid.points),
#                   mgaussian   = get_training_set_size_grid_multirandom_gaussian(y = y, from = min.size, to = max.size, n.grid.points = n.grid.points),
#                   poisson     = get_training_set_size_grid_multirandom_gaussian(y = y, from = min.size, to = max.size, n.grid.points = n.grid.points),#same as gaussian
#                   binomial    = get_training_set_size_grid_multirandom_logistic(y = y, from = min.size, to = max.size, n.grid.points = n.grid.points),
#                   multinomial = get_training_set_size_grid_multirandom_logistic(y = y, from = min.size, to = max.size, n.grid.points = n.grid.points),
#                   cox         = get_training_set_size_grid_multirandom_logistic(y = as.factor(y[,2]), from = min.size, to = max.size, n.grid.points = n.grid.points))
#   }
#
#
#   return(grid);
# }


#'Allowed Training set sizes
#'@description The function computes a sequence of allowed training set size for
#'a logistic response variable based on the following rule:
#'given nc classes, training set (n) must include the same number of patients for each class.
#'@param y response variable
#'@param from Starting point of the sequence
#'@param to the maximum allowed size of a training set
#'@return a sequence of allowed sizes
#'@keywords internal
get_balanced_training_set_size_grid <- function(
  strata, from, to){

  #Force to factor if not a matrix
  if(is.null(dim(strata)) && !is.factor(strata)) {strata = as.factor(strata)}

  #Number of classes
  nc = if(is.factor(strata)){length(levels(strata))} else {ncol(strata)}

  #Get a sequence of allowed sizes
  allowed =  seq.int(from = from, to = to, by = nc);

  return(allowed);
}



get_name_sampling_strategy <- function(object, use.k = T){
  method = get_method(object)
  k = get_k(object)
  strata = get_strata(object)
  balance = get_balance(object)

  name = switch(
    method,
    "random"    = "random sampling",
    "bootstrap" = "random sampling with replacement",
    "cv"        = "cross-validation"
  )

  if(!is.null(strata)){
    if(isTRUE(balance)){
      name = switch(
        method,
        "random"    = paste("balanced", name),
        "bootstrap" = paste("balanced", name),
        "cv"        = paste(stratified, name)
      )
    } else {
      name = paste("stratified", name)
    }
  }

  if(isTRUE(k>1) & isTRUE(use.k)){
    name = switch(
      method,
      "random"    = paste(paste0(k,"-repeated"), name),
      "bootstrap" = paste(paste0(k,"-repeated"), name),
      "cv"        = paste(paste0(k, "-fold"), name)
    )
  } else {
    name = switch(
      method,
      "random"    = paste("repeated", name),
      "bootstrap" = paste("repeated", name),
      "cv"        = paste("k-fold", name)
    )
  }

  return(name)
}


supported_sampling_methods <- function(){
  #methods
  out = c("random" ,"bootstrap", "cv")
  #r
  return(out)
}

get_sampling_method_name <- function(method){
  n = c(
    'random'    = 'random sampling without replacement',
    'bootstrap' = 'random sampling with replacement',
    'cv'        = 'cross-validation'
  )

  out = n[method]

  return(out)
}

get_sampling_method_scheme_support <- function(method){
  typenames = c(
    random    = 'stratification, balance',
    bootstrap = 'stratification, balance',
    cv        = 'stratification'
  )

  out = typenames[method]

  return(out)
}


#'Supported Sampling Methods
#'
#'@description This function returns a \code{data.frame} containing the currently
#'supported sampling methods.
#'
#'@return A \code{data.frame} with 3 columns:
#'\describe{
#'\item{\code{id}}{contains the id used in renoir for the sampling method (e.g. 'cv')}
#'\item{\code{name}}{contains the name of the sampling method (e.g. 'cross-validation')}
#'\item{\code{supported}}{contains the supported strategy}
#'}
#'
#'@seealso
#'\code{\link{rsample}},
#'\code{\link{bootstrap}},
#'\code{\link{create_folds}}
#'
#'@export
#'
#'@author Alessandro Barberis
list_supported_sampling_methods <- function(){
  #metrics
  out = supported_sampling_methods()
  #create df
  out = data.frame(id = out, name = get_sampling_method_name(out), row.names = NULL, stringsAsFactors = F)
  #add schemes
  out$supported = get_sampling_method_scheme_support(out$id)

  #r
  return(out)
}
