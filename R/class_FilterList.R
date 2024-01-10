#' @include classes_generics.R class_Filter.R
NULL

#'FilterList Class
#'
#'An S4 class to represent a list of filters to pre-process the data.
#'
#'This subclass extends the S4  \linkS4class{List} virtual class by using
#'the  \linkS4class{SimpleList} implementation.
#'
#' The object consists of 4 slots
#' @slot listData \code{list} storing the list elements
#' @slot elementType the type of data represented in the sequence
#' @slot elementMetadata  \linkS4class{DataFrame} storing the element-wise metadata,
#' with a row for each element and a column for each metadata available. Default is \code{NULL}
#' @slot metadata \code{list} storing the global metadata annotating the object as a whole
methods::setClass(
  Class = "FilterList",
  contains = "SimpleList"
  ,slots = c(listData = "list")
  # ,prototype = methods::prototype(elementType = "FilterList")
)


#'FilterList Constructor
#'
#'Constructor for the S4 FilterList object.
#'
#'Constructor for the S4 \linkS4class{FilterList} object.
#'
#'@param ... \linkS4class{Filter} objects
#'
#'@return a \linkS4class{FilterList} object
#'
#' @export
#'
#' @rdname FilterList-class
FilterList <- function(...){

  obj = new("FilterList", S4Vectors::SimpleList(...))

  # S4Vectors::elementType(obj) = "Filter"
  obj@elementType = "Filter"
  obj

}

#'@keywords internal
create_FilterList <- function(){

  #get available filters
  ids = supported_filtering_methods()

  #create
  out = lapply(ids, FUN = Filter)

  #set as FL
  out = FilterList(out)

  #return
  return(out)
}

#'@rdname filter
methods::setMethod(
  f = "filter",
  signature = "FilterList",
  definition = function(
    filter,
    x,
    na.omit = F,
    logger = Logger(),
    ...){

    #-----------------------------------------------------------------------------------#
    #Log
    logger = open_con(logger)

    #-----------------------------------------------------------------------------------#
    text.summary = paste(ncol(x), "features initially considered.")

    #-----------------------------------------------------------------------------------#
    #n of features
    nfeat = ncol(x)
    #index of features
    index = seq(nfeat)

    #-----------------------------------------------------------------------------------#
    #Loop over FilterList
    log_debug(object = logger, message = paste("Filtering step"), sep = "\n", add.level = TRUE, add.time = TRUE)
    for(i in seq(length(filter))){
      #filter
      filtered = filter(filter = filter[[i]], x = x, logger = logger)
      #update x
      x = get_filtered(object = filtered)
      #update summary
      text.summary = paste(text.summary, get_summary(object = filtered))
      #update index
      index = index[get_index(filtered)]
    }

    log_info(object = logger, message = get_log_line("long.line.1"), sep = "\n", add.level = F, add.time = F)
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
    }

    #-----------------------------------------------------------------------------------#
    #create output
    out = Filtered(
      filtered = x,
      index    = index,
      summary  = text.summary
    )

    #-----------------------------------------------------------------------------------#

    close_con(logger);

    #-----------------------------------------------------------------------------------#

    return(out);

  })
