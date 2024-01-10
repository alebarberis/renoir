#' @include classes_generics.R
NULL

#' Logger Class
#'
#' @description
#' An S4 class providing a logger.
#'
#' @slot path path to the log file to use
#' @slot con \code{file}
#' @slot verbose logical, indicating whether R should report extra information on progress
#' @slot level character, the log level
#'
#' @author Alessandro Barberis
methods::setClass(
  Class = "Logger",
  slots = c(
    path      = "character",
    con       = "fileOrNULL",
    verbose   = "logical",
    level     = "character"
  )
)

#' Logger Class Constructor
#'
#' @description
#' Constructor for the S4 \linkS4class{Logger} object.
#'
#' @details A log request in a \linkS4class{Logger} of \code{level} **l**
#' is enabled if the level of the request is **<= l**.
#'
#'
#'
#' @param path a path to the log file to use
#' @param con (optional) connection to a file
#' @param verbose logical, whether R should report extra information on progress
#' @param level the log level of the logger. Five levels are supported:
#' \describe{
#' \item{\code{"OFF"}}{indicates the logger is inactive}
#' \item{\code{"INFO"}}{indicates informational messages}
#' \item{\code{"DEBUG"}}{indicates fine-grained informational messages}
#' \item{\code{"TRACE"}}{indicates finer-grained informational events than the \code{"DEBUG"}}
#' \item{\code{"ALL"}}{all levels are considered}
#' }
#' Levels are considered ordered: \code{OFF < INFO < DEBUG < TRACE < ALL}
#'
#' @return a \linkS4class{Logger} object
#'
#'@seealso
#'\code{\link{log_all}},
#'\code{\link{log_trace}},
#'\code{\link{log_debug}},
#'\code{\link{log_info}}
#'
#' @author Alessandro Barberis
#'
#' @export
#'
#' @rdname Logger-class
Logger <- function(
  path    = character(),
  con     = NULL,
  verbose = TRUE,
  level   = c("INFO", "DEBUG", "TRACE", "ALL", "OFF")
){

  #----------------------------------------------------------------------#
  level = match.arg(level)

  #----------------------------------------------------------------------#
  use.path = FALSE

  #----------------------------------------------------------------------#
  if(!is.null(con)){
    #check con
    bool = check_con(con)
    if(!bool){
      use.path = TRUE
    }
  }

  #----------------------------------------------------------------------#
  if(use.path && !S4Vectors::isEmpty(path)){
    con = check_file_path_and_open_con(file.path = path)
  }

  #----------------------------------------------------------------------#
  methods::new(
    Class = "Logger",
    path    = path,
    con     = con,
    verbose = verbose,
    level   = level
  )
}

methods::setMethod(f = "get_con",     signature = "Logger", definition = function(object){methods::slot(object = object, name = 'con')})
methods::setMethod(f = "get_verbose", signature = "Logger", definition = function(object){methods::slot(object = object, name = 'verbose')})
methods::setMethod(f = "get_path",    signature = "Logger", definition = function(object){methods::slot(object = object, name = 'path')})
methods::setMethod(f = "get_level",   signature = "Logger", definition = function(object){methods::slot(object = object, name = 'level')})

methods::setMethod(
  f = "close_con",
  signature = "Logger",
  definition = function(object){

    con = get_con(object = object)

    con = check_con_and_close(con = con)

  }
)

methods::setMethod(
  f = "open_con",
  signature = "Logger",
  definition = function(object){

    fpath = get_path(object = object)
    con = check_file_path_and_open_con(file.path = fpath)
    methods::slot(object = object, name = 'con') = con
    return(object)
  }
)

#'Get log levels
#'@description Returns all the log levels included in the selected level
#'@param level a log level
#'@return A vector containing all the levels corresponding to \code{level}
#'@author Alessandro Barberis
#'@keywords internal
get_log_levels = function(
  level = c("OFF", "INFO", "DEBUG", "TRACE", "ALL")
  ){

  level = match.arg(level)

  out = switch(
    level,
    ALL   = c("INFO", "DEBUG", "TRACE") ,
    TRACE = c("INFO", "DEBUG", "TRACE"),
    DEBUG = c("INFO", "DEBUG"),
    INFO  = c("INFO"),
    OFF   = ""
  )

}

#'@keywords internal
log_level_name_fixed_length <- function(level){
  out = switch(
    level,
    ALL   = "[ALL]  ",
    TRACE = "[TRACE]",
    DEBUG = "[DEBUG]",
    INFO  = "[INFO] ",
    OFF   = ""
  )
}


#'Log Method
#'
#'@description Generic function to print the log information.
#'
#'@param object an object of class \linkS4class{Logger}
#'@param log.level a character string, the log level of the information
#'@param message a character string, the message to print
#'@param sep a character vector of strings to append after \code{message}
#'@param add.level logical, whether to add the log level to \code{message}
#'@param add.time logical, whether to add the time to \code{message}
#'
#'@return None (invisible \code{NULL}).
#'
#'@seealso
#'\code{\link{log_all}},
#'\code{\link{log_trace}},
#'\code{\link{log_debug}},
#'\code{\link{log_info}}
#'
#'@keywords internal
#'
#'@author Alessandro Barberis
log_default = function(object, log.level, message, sep, add.level = FALSE, add.time = FALSE){
  level = get_level(object)

  if(log.level %in% get_log_levels(level = level)){

    if(add.time){
      message = paste(Sys.time(), message)
    }

    if(add.level){
      # message = paste0("[",log.level,"] ", message)
      message = paste(log_level_name_fixed_length(log.level), message)
    }

    print_log(object = object, message = message, sep = sep)
  }
}

#'Log Method
#'
#'@name log_all
#'
#'@description This function prints the log information of level \code{ALL}.
#'
#'@details A log information of level \code{ALL} is printed if the provided logger
#'has a >= log level.
#'
#'@inheritParams log_default
#'@inherit log_default return
#'
#'@seealso
#'\code{\link{log_trace()}},
#'\code{\link{log_debug()}},
#'\code{\link{log_info()}}
#'
#'@author Alessandro Barberis
#'
#'@export
methods::setMethod(
  f = "log_all",
  signature = "Logger",
  definition = function(object, message, sep, add.level = FALSE, add.time = FALSE){
    log_default(object = object, log.level = "ALL", message = message, sep = sep, add.level = add.level, add.time = add.time)
  }
)

#'Log Method
#'
#'@name log_trace
#'
#'@description This function prints the log information of level \code{TRACE}.
#'
#'@details A log information of level \code{TRACE} is printed if the provided logger
#'has a >= log level.
#'
#'@inheritParams log_default
#'@inherit log_default return
#'
#'@seealso
#'\code{\link{log_all}},
#'\code{\link{log_debug}},
#'\code{\link{log_info}}
#'
#'@author Alessandro Barberis
#'
#'@export
methods::setMethod(
  f = "log_trace",
  signature = "Logger",
  definition = function(object, message, sep, add.level = FALSE, add.time = FALSE){
    log_default(object = object, log.level = "TRACE", message = message, sep = sep, add.level = add.level, add.time = add.time)
  }
)

#'Log Method
#'
#'@name log_debug
#'
#'@description This function prints the log information of level \code{DEBUG}.
#'
#'@details A log information of level \code{DEBUG} is printed if the provided logger
#'has a >= log level.
#'
#'@inheritParams log_default
#'@inherit log_default return
#'
#'@seealso
#'\code{\link{log_all}},
#'\code{\link{log_trace}},
#'\code{\link{log_info}}
#'
#'@author Alessandro Barberis
#'
#'@export
methods::setMethod(
  f = "log_debug",
  signature = "Logger",
  definition = function(object, message, sep, add.level = FALSE, add.time = FALSE){
    log_default(object = object, log.level = "DEBUG", message = message, sep = sep, add.level = add.level, add.time = add.time)
  }
)

#'Log Method
#'
#'@name log_info
#'
#'@description This function prints the log information of level \code{INFO}.
#'
#'@details A log information of level \code{INFO} is printed if the provided logger
#'has a >= log level.
#'
#'@inheritParams log_default
#'@inherit log_default return
#'
#'@seealso
#'\code{\link{log_all}},
#'\code{\link{log_trace}},
#'\code{\link{log_debug}}
#'
#'@author Alessandro Barberis
#'
#'@export
methods::setMethod(
  f = "log_info",
  signature = "Logger",
  definition = function(object, message, sep, add.level = FALSE, add.time = FALSE){
    log_default(object = object, log.level = "INFO", message = message, sep = sep, add.level = add.level, add.time = add.time)
  }
)

methods::setMethod(
  f = "print_log",
  signature = "Logger",
  definition = function(object, message, sep){

    con     = get_con(object = object)
    verbose = get_verbose(object = object)

    if(verbose){
      cat(message, sep = sep);
    }

    if(check_con(con)){
      cat(message, file = con, sep = sep);
    }

  }
)

#'@keywords internal
get_log_line <- function(line = c("long.line.1", "long.line.2", "long.line.3",
                                  "short.line.1", "short.line.2", "short.line.3")){
  line = match.arg(line);

  message = switch(line,
                   long.line.1  = "----------------------------------------------------------------------------",
                   long.line.2  = "#--------------------------------------------------------------------------#",
                   long.line.3  = "############################################################################",
                   short.line.1 = "------------------------------------------",
                   short.line.2 = "#----------------------------------------#",
                   short.line.3 = "##########################################")

  return(message)
}

methods::setMethod(
  f = "print_log_line",
  signature = "Logger",
  definition = function(
    object,
    line = c("long.line.1", "long.line.2", "long.line.3",
             "short.line.1", "short.line.2", "short.line.3"),
    sep){

    line = match.arg(line);

    message = switch(line,
                     long.line.1  = "----------------------------------------------------------------------------",
                     long.line.2  = "#--------------------------------------------------------------------------#",
                     long.line.3  = "############################################################################",
                     short.line.1 = "------------------------------------------",
                     short.line.2 = "#----------------------------------------#",
                     short.line.3 = "##########################################")

    print_log(object = object, message = message, sep = sep)

  }
)

#'Print log file
#'@keywords internal
print.log <- function(message, verbose, con, sep){

  if(verbose){
    cat(message, sep=sep);
  }

  if(check.con(con)){
    cat(message, file = con, sep=sep);
  }

}

#'Print log line
#'@keywords internal
print.log.line <- function(line = c("long.line.1", "long.line.2", "long.line.3",
                                    "short.line.1", "short.line.2", "short.line.3"),
                           verbose, con, sep){

  line = match.arg(line);

  message = switch(line,
                   long.line.1  = "----------------------------------------------------------------------------",
                   long.line.2  = "#--------------------------------------------------------------------------#",
                   long.line.3  = "############################################################################",
                   short.line.1 = "------------------------------------------",
                   short.line.2 = "#----------------------------------------#",
                   short.line.3 = "##########################################")

  print.log(message = message, verbose = verbose, con = con, sep = sep);
}

#'Check the existance of a file
#'@keywords internal
check_file_path <- function(path = NULL){
  if(!missing(path) && !is.null(path) && !S4Vectors::isEmpty(path) && dir.exists(dirname(path))){
    bool = TRUE;
  } else {
    bool = F;
  }
  return(bool);
}


#'Check file path and open connection
#'@keywords internal
check_file_path_and_open_con <- function(file.path = NULL){
  if(check_file_path(file.path)){
    #Open connection to log and warning files
    con = file(file.path, open = "a");
  } else {
    con = NULL;
    # con = character();
  }
  return(con);
}

#'Check file connection
#'@keywords internal
check_con <- function(con){

  out = FALSE;

  if(!missing(con) && !is.null(con)){
    if((sum(grepl(pattern = "connection", x = class(con)))==1) && isOpen(con, rw = "")){
      out=TRUE;
    }
  }

  return(out);
}


#'Check and close file connection
#'@keywords internal
check_con_and_close <- function(con){
  if(check_con(con)){
    close(con);
  }
}
