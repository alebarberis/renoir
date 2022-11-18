#' @include classes_generics.R class_Filtered.R class_Logger.R
NULL

#' Filter Class
#'
#' @description
#' An S4 class representing a filter to pre-process the input data.
#'
#' The object consists of 3 slots
#' @slot id a name to identify the filter
#' @slot filter filter function
#' @slot parameters list containing the parameters for the chosen filtering method
#' @slot logger a \linkS4class{Logger}
#' @author Alessandro Barberis
methods::setClass(
  Class = "Filter",
  slots = c(
    id         = "character",
    filter     = "function",
    parameters = "list",
    logger     = "Logger"
  )
)

#'Filter Constructor
#'
#'@description
#'Constructor for the S4 \linkS4class{Filter} object.
#'
#'@param id a name to identify the filter.
#' If filtering method is one of the supported by renoir, the constructor will
#' automatically select a \code{filter}. See \code{list_supported_unsupervised_screening_methods()}
#' for the supported methods.
#' @param filter (optional) function to filter the initial data.
#' Used if \code{id} is not one of the supported by renoir.
#' If \code{filter} is provided it must conform to the renoir common interface,
#' and must have the following formal arguments:
#' \describe{
#'    \item{x}{the input matrix, where rows are observations and columns are variables.}
#'    \item{logger}{a \linkS4class{Logger}}
#    \item{threshold}{threshold used when filtering}
#'    \item{...}{additional arguments}
#' }
#' The output must be a \linkS4class{Filtered} object
#' @param parameters list containing the parameters to fix for the chosen filtering method
#' @param logger a \linkS4class{Logger}
#'
#' @return A \linkS4class{Filtered} object.
#'
#'@seealso
#'\code{\link{filter_by_na}},
#'\code{\link{filter_by_intensity}},
#'\code{\link{filter_by_variability}}
#'
#' @export
#'
#' @author Alessandro Barberis
Filter <- function(
  id,
  filter,
  parameters,
  logger = Logger(verbose = F)
){

  if(missing(filter) && (id %in% supported_filtering_methods())){
    filter = get_filter_function(id)
  } else if(!missing(filter)){
    if(id %in% supported_filtering_methods()){
      stop(paste("'id' not valid, please change name. It must be different from the built-in renoir method names:\n",paste(supported_filtering_methods(), collapse = ", "),"\n"))
    }
  } else {
    stop("Please provide a valid 'id' and/or 'filter'.\n")
  }

  #Check provided prediction function
  check_provided_filter_function(filter)

  #Check parameters
  if(missing(parameters) || is.null(parameters) || length(parameters)==0){
    parameters = list()
  }

  methods::new(
    Class = "Filter",
    filter     = filter,
    id         = id,
    parameters = parameters,
    logger     = logger
  )
}



methods::setMethod(f = "get_id",         signature = "Filter", definition = function(object){methods::slot(object = object, name = 'id')})
methods::setMethod(f = "get_filter",     signature = "Filter", definition = function(object){methods::slot(object = object, name = 'filter')})
methods::setMethod(f = "get_parameters", signature = "Filter", definition = function(object){methods::slot(object = object, name = 'parameters')})
methods::setMethod(f = "get_logger",     signature = "Filter", definition = function(object){methods::slot(object = object, name = 'logger')})


supported_filtering_methods <- function(){
  out = c('na', 'intensity', 'variability')
  return(out)
}

get_filter_function <-function(id) {
  out = switch(
    id,
    'na'          = filter_by_na,
    'intensity'   = filter_by_intensity,
    'variability' = filter_by_variability
  )
  return(out)
}

check_provided_filter_function <- function(f){

  if(missing(f)){
    stop("'filter' is missing with no default.\n")
  } else {
    #needed formals
    # formals.def = c("x", "threshold")
    formals.def = c("x", "logger")

    #get formals
    formals.fun = names(formals(fun = f))

    #check
    # if(any(!(formals.fun %in% formals.def))){
    if(any(!(formals.def %in% formals.fun))){

      stop(paste0("Provided filter function without required formal arguments.Function interface must match renoir requirements. Try with:\n
      function(", paste0(formals.def, collapse = ", "),"...){
        #Use provided data to filter the features
        # out = YOUR CODE

        #Set a Filtered object as output
        # out = Filtered(filtered = FILTERED_DATA, index = INDEX_OF_SELECTED_FEATURES, summary = DESCRIPTION_OF_FILTERING)

        #Return
        return(out)
      }
      \n"))
    }
  }
}


#'Unsupervised Screening
#'
#'@description This function filters the data in input by unsupervised screening.
#'
#'@param filter an object of class \linkS4class{Filter}, providing the function
#'performing the unsupervised screening
#'@param x the input matrix, where rows are observations and columns are variables
#'@param logger an object of class \linkS4class{Logger}
#'@param ... further arguments to the function performing the unsupervised screening
#'
#'@return An object of class \linkS4class{Filtered}.
#'
#'@seealso
#'\code{\link{filter_by_na}},
#'\code{\link{filter_by_intensity}},
#'\code{\link{filter_by_variability}}
#'
#'@export
#'
#'@author Alessandro Barberis
methods::setMethod(
  f = "filter",
  signature = methods::signature(filter = "Filter"),
  definition = function(
    filter,
    x,
    logger,
    ...
  ){

  #--------------------------------------------------------------------------------------------#
  #logger
  if(missing(logger)){logger = get_logger(filter)}

  #--------------------------------------------------------------------------------------------#
  #get parameters
  parameters = get_parameters(filter)

  #--------------------------------------------------------------------------------------------#
  #get function
  filter_fun = get_filter(filter)

  #--------------------------------------------------------------------------------------------#
  #check
  parameters = check_parameters(parameters, ...)

  #--------------------------------------------------------------------------------------------#
  #merge
  args = c(parameters, list(...))

  #--------------------------------------------------------------------------------------------#
  #filter
  out = do.call(what = filter_fun, args = c(list(x = x, logger = logger), args))

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)
})


filter_def <- function(
  x,
  methods = c('na', 'intensity', 'variability'),
  na.omit = F,
  cutoff.na   = 0.5,
  cutoff.int  = 0.25,
  cutoff.var  = 0.25,
  var.measure = c('sd', 'IQR'),
  logger = Logger()){

  #-----------------------------------------------------------------------------------#
  #Log
  logger = open_con(logger)

  #-----------------------------------------------------------------------------------#
  var.measure = match.arg(var.measure)

  #-----------------------------------------------------------------------------------#
  text.summary = paste(ncol(x), "features initially considered.")

  #-----------------------------------------------------------------------------------#
  #n of features
  nfeat = ncol(x)
  #index of features
  index = seq(nfeat)

  #-----------------------------------------------------------------------------------#

  if(!S4Vectors::isEmpty(methods)){

    log_debug(object = logger, message = paste("Filtering step"), sep = "\n", add.level = TRUE, add.time = TRUE)

    #Remove features with na in > 50% samples
    if('na' %in% methods){
      #filter
      filtered = filter_by_na(x = x, threshold = cutoff.na, logger = logger)
      #update x
      x = get_filtered(object = filtered)
      #update summary
      text.summary = paste(text.summary, get_summary(object = filtered))
      #update index
      index = index[get_index(filtered)]
    }

    #Select features expressed over the median
    if('intensity' %in% methods){
      #filter
      filtered = filter_by_intensity(x = x, threshold = cutoff.int, logger = logger)
      #update x
      x = get_filtered(object = filtered)
      #update summary
      text.summary = paste(text.summary, get_summary(object = filtered))
      #update index
      index = index[get_index(filtered)]
    }


    #Select the most variable features (features in top 25%)
    if('variability' %in% methods){
      #filter
      filtered = filter_by_variability(x = x, threshold = cutoff.var, method = var.measure, logger = logger)
      #update x
      x = get_filtered(object = filtered)
      #update summary
      text.summary = paste(text.summary, get_summary(object = filtered))
      #update index
      index = index[get_index(filtered)]
    }

    log_info(object = logger, message = get_log_line("long.line.1"), sep = "\n", add.level = F, add.time = F)
  }

  #-----------------------------------------------------------------------------------#
  if(na.omit){
    log_info(object = logger, message = "Handling missing values", sep = "\n", add.level = TRUE, add.time = TRUE)

    nfeat = ncol(x)

    #filter
    filtered = filter_by_na(x = x, threshold = 0, logger = logger)
    #update x
    x = get_filtered(object = filtered)
    #update summary
    # text.summary = paste(text.summary, get_summary(object = filtered))
    text.summary = paste0(text.summary, " Missing values were handled (",(nfeat - ncol(x))," features removed, ",ncol(x)," features left).")
    #update index
    index = index[get_index(filtered)]

    logMsg = paste(ncol(x),"features left");
    log_info(object = logger, message = logMsg, sep = "\n", add.level = TRUE, add.time = TRUE)

    log_info(object = logger, message = get_log_line("long.line.1"), sep = "\n", add.level = F, add.time = F)

    #update methods
    methods = c(methods, "na.omit")
  }

  #-----------------------------------------------------------------------------------#
  #create output
  out = Filtered(
    method   = methods,
    filtered = x,
    index    = index,
    summary  = text.summary
  )

  #-----------------------------------------------------------------------------------#

  close_con(logger);

  #-----------------------------------------------------------------------------------#

  return(out);
}

#'Filter by missing values
#'
#'@description This function filters the input matrix \code{x} depending on the presence of NAs.
#'A variable is removed if sum of NA elements is greater than a given percentage defined by \code{threshold}.
#'Default is 0.5 (i.e. select variables with less than 50\% of NAs)
#'
#'@param x the input matrix, where rows are observations and columns are variables.
#'@param threshold the cutoff
#'@param logger a \linkS4class{Logger}
#'
#'@return A \linkS4class{Filtered} object
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
filter_by_na <- function(x, threshold = 0.5, logger){

  #-----------------------------------------------------------------------------------#
  #Check input
  if(missing(x) || is.null(x) || !is.matrix(x)){
    stop("Need to specify an expression matrix where the rows represent the genes and the columns the samples.")
  }

  if(sum(!is.na(x))==0){stop("Input matrix is full of NAs. Check your data.")}

  if(!is.numeric(threshold) || threshold<0 || threshold > 1){
    threshold = 0.5;
    warning("Threshold not valid. Set to 0.5 by default. Valid range is [0, 1].")
  }

  if(missing(logger)){logger = Logger()}
  logger = open_con(logger)

  tryCatch({

    keep = seq(ncol(x))

    #-----------------------------------------------------------------------------------#
    #Compute cutoff
    thr = nrow(x) * threshold

    #-----------------------------------------------------------------------------------#
    #Test which value is NA
    na.test = is.na(x)

    #-----------------------------------------------------------------------------------#
    #Test which col should be removed
    remove.index = apply(X = na.test, MARGIN = 2, FUN=function(x, thr){sum(x)>thr}, thr = thr)

    #-----------------------------------------------------------------------------------#
    numRemoved = 0;

    if(sum(remove.index)>0){
      #Remove columns with NA in > threshold of rows
      log.msg = paste("The following column(s) were removed because the value was NA in more than", round(threshold*100, digits = 1) ,"% of the rows")
      log_debug(object = logger, message = log.msg, sep = "\n", add.level = TRUE, add.time = TRUE)

      removed = if(!is.null(colnames(x))){
        colnames(x)[which(remove.index==TRUE)]
      } else {
        which(remove.index==TRUE)
      }

      # for(irem in 1:length(removed)){
      #   log_debug(object = logger, message = removed[irem], sep = "\n", add.level = TRUE, add.time = TRUE)
      # }

      log_debug(object = logger, message = paste(removed, collapse = " "), sep = "\n", add.level = T, add.time = T)

      #keep
      keep = which(remove.index==FALSE)

      #Update the input matrix
      x = x[,keep,drop=FALSE]

      filtered.matrix = TRUE;
      numRemoved = length(removed);
    }

    log.msg = paste("Filtering the data with missing values:", numRemoved, "col(s) removed because the value was NA in more than", round(threshold*100, digits = 1) ,"% of the rows");
    log_info(object = logger, message = log.msg, sep = "\n", add.level = TRUE, add.time = TRUE)
    #-----------------------------------------------------------------------------------#
    # out = list("x"=x, "n.removed"=numRemoved);

    if(identical(x = numRemoved, y = 1)){
      text.summary = paste("One feature was removed because its value")
    } else {
      text.summary = paste(numRemoved, "features were removed because their values")
    }

    text.summary = paste0(text.summary, " is NA in more than ",round(threshold*100, digits = 1),"% of the observations.")

    out = Filtered(
      method     = "na",
      filtered   = x,
      index      = keep,
      summary    = text.summary
    )

    #-----------------------------------------------------------------------------------#
    return(out)
  }, error = function(err) {

    log_info(object = logger, message = "\n", sep = "\n", add.level = F, add.time = F)
    log_info(object = logger, message = paste("Errors occurred during na filtering:", err), sep = "\n", add.level = TRUE, add.time = TRUE)

    stop("Errors occurred during na filtering")
  }, finally = {
    close_con(object = logger)
  })#END tryCatch
}

#'Filter by intensity
#'
#'@description This function filters the input matrix \code{x} depending on the features intensity.
#'A variable is kept if value is greater than median across observations in a given percentage of observations defined by \code{threshold}.
#'Default is 0.25 (i.e. select variables with values > median in more than 25\% of observations)
#'
#'@param x the input matrix, where rows are observations and columns are variables.
#'@param threshold the cutoff
#'@param logger a \linkS4class{Logger}
#'
#'@return A \linkS4class{Filtered} object
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
filter_by_intensity <- function(x, threshold = 0.25, logger){
  #-----------------------------------------------------------------------------------#
  #Check input
  if(missing(x) || is.null(x) || !is.matrix(x)){
    stop("Need to specify an expression matrix where the rows represent the genes and the columns the samples.")
  }

  if(sum(!is.na(x))==0){stop("Input matrix is full of NAs. Check your data.")}

  if(!is.numeric(threshold) || threshold<0 || threshold > 1){
    threshold = 0.5;
    warning("Threshold not valid. Set to 0.5 by default. Valid range is [0, 1].")
  }

  if(missing(logger)){logger = Logger()}
  logger = open_con(logger)

  #-----------------------------------------------------------------------------------#
  tryCatch({

    #-----------------------------------------------------------------------------------#
    nfeat = ncol(x)
    keep = seq(nfeat)
    #-----------------------------------------------------------------------------------#
    #Compute cutoff
    thr = nrow(x) * threshold

    #-----------------------------------------------------------------------------------#
    #Compute median
    log_debug(object = logger, message = "Computing the median of each row", sep = "\n", add.level = TRUE, add.time = TRUE)
    median.rows = apply(X = x, MARGIN = 1, FUN = median, na.rm = TRUE)

    #-----------------------------------------------------------------------------------#
    #Test if expression is over median
    test = apply(X = x, MARGIN = 2, FUN=function(x, m){ x >= m}, m = median.rows)

    #Compute how many times test is TRUE
    times = colSums(x = test, na.rm = T)

    #-----------------------------------------------------------------------------------#
    #Test which col should be removed
    remove.index = times < thr

    #-----------------------------------------------------------------------------------#
    numRemoved = 0;

    log_info(object = logger, message = "Filtering the data by intensity...", sep = "", add.level = TRUE, add.time = TRUE)
    if(sum(remove.index)>0){
      #Remove columns with NA in > threshold of rows
      # log.msg = paste("The following column(s) were removed because the value was lower than the row(s) median in more than", round(threshold*100, digits = 1) ,"% of the rows")
      # log_debug(object = logger, message = log.msg, sep = "\n", add.level = TRUE, add.time = TRUE)

      removed = if(!is.null(colnames(x))){
        colnames(x)[which(remove.index==TRUE)]
      } else {
        which(remove.index==TRUE)
      }

      # for(irem in 1:length(removed)){
      #   log_debug(object = logger, message = removed[irem], sep = "\n", add.level = TRUE, add.time = TRUE)
      # }

      # log_debug(object = logger, message = removed, sep = "\n", add.level = TRUE, add.time = TRUE)

      #keep
      keep = which(remove.index==FALSE)

      #Saving the filtered matrix
      x = x[,keep,drop=FALSE]

      filtered.matrix = TRUE;
      numRemoved = length(removed);
    }

    log.msg = paste("DONE:", base::ncol(x), "col(s) selected over", (base::ncol(x) + numRemoved));
    log_info(object = logger, message = log.msg, sep = "\n", add.level = F, add.time = F)

    #-----------------------------------------------------------------------------------#
    # out = list("x"=x, "n.removed"=numRemoved);

    if(identical(x = ncol(x), y = 1)){
      text.summary = paste("One out of",nfeat,"features was selected because its value is")
    } else {
      text.summary = paste(ncol(x), "out of", nfeat, "features were selected because their values are")
    }

    text.summary = paste0(text.summary, " greater than the median value in more than ",round(threshold*100, digits = 1),"% of the observations.")

    out = Filtered(
      method     = "intensity",
      filtered   = x,
      index      = keep,
      summary    = text.summary
    )

    #-----------------------------------------------------------------------------------#
    return(out)

  }, error = function(err) {

    log_info(object = logger, message = "\n", sep = "\n", add.level = F, add.time = F)
    log_info(object = logger, message = paste("Errors occurred during median filtering:", err), sep = "\n", add.level = TRUE, add.time = TRUE)

    stop("Errors occurred during the filtering by median intensity\n")
  }, finally = {
    close_con(object = logger)
  })#END tryCatch
}

#'Filter by variability
#'
#'@description This function filters the input matrix \code{x} by features variability.
#'A feature is kept if part of the top variable features, where the cutoff is defined by \code{threshold}.
#'Default is 0.25 (i.e. select features in the top 25\% of variable features).
#'Two measures of variability are possible.
#'
#'@param x the input matrix, where rows are observations and columns are variables.
#'@param threshold the cutoff
#'@param method the measure of variability, i.e. the standard deviation (\code{sd}),
#'the interquartile range (\code{IQR}, the median absolute deviation (\code{mad}))
#'@param logger a \linkS4class{Logger}
#'
#'@return A \linkS4class{Filtered} object
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
filter_by_variability <- function(x, threshold = 0.25, logger, method = c("sd", "IQR", "mad")){
  #-----------------------------------------------------------------------------------#
  #Check input
  if(missing(x) || is.null(x) || !is.matrix(x)){
    stop("Need to specify an expression matrix where the rows represent the genes and the columns the samples.")
  }

  if(sum(!is.na(x))==0){stop("Input matrix is full of NAs. Check your data.")}

  if(!is.numeric(threshold) || threshold<0 || threshold > 1){
    threshold = 0.25;
    warning("Threshold not valid. Set to 0.25 by default. Valid range is [0, 1].\n")
  }

  if(missing(logger)){logger = Logger()}
  logger = open_con(logger)


  method = match.arg(method)

  #-----------------------------------------------------------------------------------#

  tryCatch({

    nfeat = ncol(x)
    keep = seq(nfeat)

    numRemoved = 0

    logMsg = paste0("Selecting top variable columns (top ", round(threshold*100, digits = 1) ,"%) by using ", get_var_measure_name(var.measure = method), "...");
    log_info(object = logger, message = logMsg, sep = "", add.level = TRUE, add.time = TRUE)


    #Compute interquartile range
    metric = switch(
      method,
      'sd'  = apply(X = x, MARGIN = 2, FUN = function(z){stats::sd( x = z, na.rm = TRUE)}),
      'IQR' = apply(X = x, MARGIN = 2, FUN = function(z){stats::IQR(x = z, na.rm = TRUE)}),
      'mad' = apply(X = x, MARGIN = 2, FUN = function(z){stats::mad(x = z, na.rm = TRUE)})
    );

    #keep
    keep = metric >= stats::quantile(x=metric, probs=(1 - threshold), na.rm=TRUE)

    # log_info(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #-----------------------------------------------------------------------------------#
    #Filter data

    # log_info(object = logger, message = "Filtering the data...", sep = "", add.level = TRUE, add.time = TRUE)

    x <- x[, keep, drop=F] #selecting top % highly variable samples

    #Compute removed cols
    numRemoved = sum(!keep);

    log.msg = paste("DONE:", base::ncol(x), "col(s) selected over", (base::ncol(x) + numRemoved));
    log_info(object = logger, message = log.msg, sep = "\n", add.level = F, add.time = F)

    #-----------------------------------------------------------------------------------#
    # out = list("x"=x, "n.removed"=numRemoved);

    #set summary
    if(identical(x = ncol(x), y = 1)){
      text.summary = paste("One out of", nfeat,"features was selected as top variable feature")
    } else {
      text.summary = paste(ncol(x), "out of", nfeat, "features were selected as top variable features")
    }
    text.summary = paste0(text.summary," (top ",round(threshold*100, digits = 1),"%) by using ", get_var_measure_name(var.measure = method),".")

    out = Filtered(
      method     = "variability",
      filtered = x,
      index    = which(keep),
      summary  = text.summary
    )

    #-----------------------------------------------------------------------------------#
    return(out)
  }, error = function(err) {

    log_info(object = logger, message = "\n", sep = "\n", add.level = F, add.time = F)
    log_info(object = logger, message = paste("Errors occurred during variability filtering:", err), sep = "\n", add.level = TRUE, add.time = TRUE)

    stop("Errors occurred during the filtering by variability\n")
  }, finally = {
    close_con(object = logger)
  })#END tryCatch
}


get_var_measure_name <- function(var.measure = c('sd', 'IQR', 'mad')){
  out = switch(
    var.measure,
    sd  = "standard deviation",
    IQR = "interquartile range",
    mad = "median absolute deviation"
  )

  return(out)
}



supported_unsupervised_screening_methods <- function(){

  #methods
  out = c(
    'na'          = "filter by NA presence in the data (remove if NAs > threshold)",
    'intensity'   = "filter by features intensity (keep if value > median across observations)",
    'variability' = "filter by features variability (select top variable features)"
  )

  #r
  return(out)
}

#'Supported Unsupervised Screening
#'
#'@description This function returns a \code{data.frame} containing the currently
#'supported unsupervised screening methods.
#'
#'@details The currently implemented unsupervised screening methods are returned in a \code{data.frame}.
#'
#'@return A \code{data.frame} with 2 columns:
#'\describe{
#'\item{\code{id}}{contains the id used in renoir for the unsupervised screening method (e.g. 'na')}
#'\item{\code{description}}{contains a short description of the method (e.g. 'filter by NA presence')}
#'}
#'
#'@seealso
#'\code{\link{filter_by_na}},
#'\code{\link{filter_by_intensity}},
#'\code{\link{filter_by_variability}}
#'
#'@export
#'
#'@author Alessandro Barberis
list_supported_unsupervised_screening_methods <- function(){
  #metrics
  out = supported_unsupervised_screening_methods()
  #create df
  out = data.frame(id = names(out), description = out, row.names = NULL, stringsAsFactors = F)

  #r
  return(out)
}
