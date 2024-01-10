#' @include classes_generics.R class_Marked.R
NULL

#' MarkedList Class
#' An S4 class to represent a renoir trained and tested models list
#'
#' This subclass extends the S4  \linkS4class{List} virtual class by using
#' the  \linkS4class{SimpleList} implementation.
#'
#' The object consists of 4 slots
#' @slot listData \code{list} storing the list elements
#' @slot elementType the type of data represented in the sequence
#' @slot elementMetadata  \linkS4class{DataFrame} storing the element-wise metadata,
#' with a row for each element and a column for each metadata available. Default is \code{NULL}
#' @slot metadata \code{list} storing the global metadata annotating the object as a whole
methods::setClass(
  Class = "MarkedList",
  contains = "SimpleList"
  ,slots = c(listData = "list")
  # ,prototype = methods::prototype(elementType = "Tuned")
)


#' LearnedList Constructor
#' Constructor for the S4 LearnedList object.
#'
#' Constructor for the S4 \linkS4class{LearnedList} object.
#' @rdname MarkedList-class
MarkedList <- function(...){

  obj = new("MarkedList", S4Vectors::SimpleList(...))

  obj@elementType = "Marked"
  obj

}

methods::setMethod(f = "get_set",     signature = "MarkedList", definition = function(object){ sapply(X = object, FUN = get_set    )})
methods::setMethod(f = "get_measure", signature = "MarkedList", definition = function(object){ sapply(X = object, FUN = get_measure)})
methods::setMethod(f = "get_mark",    signature = "MarkedList", definition = function(object){ lapply(X = object, FUN = get_mark   )})

methods::setMethod(
  f = "subset_list",
  signature = methods::signature(object = "MarkedList"),
  definition = function(object, set, measure){

    if(!missing(set)){
      #keep
      keep = get_set(object) %in% set
      #subset
      object = object[keep]
    }

    if(!missing(measure)){
      #keep
      keep = get_measure(object) %in% measure
      #subset
      object = object[keep]
    }

    if(length(object)==1){
      object = object[[1]]
    }

    return(object)
  }
)


methods::setMethod(
  f = "isEmpty",
  signature = "MarkedList",
  definition = function(x){

    out = S4Vectors::isEmpty(x)

    #return
    return(out)
  })

# methods::setMethod(f = "get_mark",           signature = "MarkedList", definition = function(object){out = lapply(X = object, FUN = get_mark);return(out)})
# methods::setMethod(f = "get_config",         signature = "MarkedList", definition = function(object){out = lapply(X = object, FUN = get_config);return(out)})
# methods::setMethod(f = "get_measure",        signature = "MarkedList", definition = function(object){out = lapply(X = object, FUN = get_measure);return(out)})
# methods::setMethod(f = "get_set",            signature = "MarkedList", definition = function(object){out = lapply(X = object, FUN = get_set);return(out)})
# methods::setMethod(f = "get_marking_system", signature = "MarkedList", definition = function(object){out = lapply(X = object, FUN = get_marking_system);return(out)})
#
# # methods::setMethod(
# #   f = "get_mark",
# #   signature = methods::signature(object = "MarkedList", config = "missing", type.measure = "missing", set = "missing", marking.system = "missing"),
# #   definition = function(object){
# #     out = lapply(X = object, FUN = get_mark);
# #     return(out)
# #   })
#
# methods::setMethod(
#   f = "get_marked",
#   signature = methods::signature(object = "MarkedList", marking.system = "character", config = "character", set = "character", type.measure = "character"),
#   definition = function(object, marking.system, config, set, type.measure){
#
#     #match config
#     object = get_marked(object = object, config = config);
#     #match measure
#     object = get_marked(object = object, type.measure = type.measure);
#     #match set
#     object = get_marked(object = object, set = set);
#     #match system
#     object = get_marked(object = object, marking.system = marking.system);
#
#     return(object)
#   })
#
# methods::setMethod(
#   f = "get_marked",
#   signature = methods::signature(object = "MarkedList", marking.system = "character", config = "character", set = "character", type.measure = "missing"),
#   definition = function(object, marking.system, config, set, type.measure){
#
#     #match config
#     object = get_marked(object = object, config = config);
#     #match set
#     object = get_marked(object = object, set = set);
#     #match system
#     object = get_marked(object = object, marking.system = marking.system);
#
#     return(object)
#   })
#
# methods::setMethod(
#   f = "get_marked",
#   signature = methods::signature(object = "MarkedList", marking.system = "missing", config = "character", set = "missing", type.measure = "missing"),
#   definition = function(
#     object,
#     config = c("opt", "1se")){
#     #match
#     config = match.arg(config)
#     #get config
#     config.l = get_config(object);
#     #unlist
#     configs = unlist(config.l)
#     #match
#     keep = configs %in% config
#     #check
#     # if(sum(keep)==0){warning("No element found")}
#     #subset
#     object = object[keep]
#     #return
#     return(object)
#   })
#
# methods::setMethod(
#   f = "get_marked",
#   signature = methods::signature(object = "MarkedList", marking.system = "missing", config = "missing", set = "missing", type.measure = "character"),
#   definition = function(
#     object,
#     type.measure){
#     #get measures
#     measure.l = get_measure(object);
#     #unlist
#     measures = unlist(measure.l)
#     #match
#     keep = measures %in% type.measure
#     #check
#     # if(sum(keep)==0){warning("No element found")}
#     #subset
#     object = object[keep]
#     #return
#     return(object)
#   })
#
# methods::setMethod(
#   f = "get_marked",
#   signature = methods::signature(object = "MarkedList", marking.system = "missing", config = "missing", set = "character", type.measure = "missing"),
#   definition = function(
#     object,
#     set = c('test', 'train', 'full')){
#
#     #match
#     set = match.arg(set)
#     #get sets
#     set.l = get_set(object);
#     #unlist
#     sets = unlist(set.l)
#     #match
#     keep = sets %in% set
#     #check
#     # if(sum(keep)==0){warning("No element found")}
#     #subset
#     object = object[keep]
#     #return
#     return(object)
#   })
#
# methods::setMethod(
#   f = "get_marked",
#   signature = methods::signature(object = "MarkedList", marking.system = "character", config = "missing", set = "missing", type.measure = "missing"),
#   definition = function(
#     object,
#     marking.system){
#
#     #match
#     # set = match.arg(set)
#     #get sets
#     ms.l = get_marking_system(object);
#     #unlist
#     msys = unlist(ms.l)
#     #match
#     keep = msys %in% marking.system
#     #check
#     # if(sum(keep)==0){warning("No element found")}
#     #subset
#     object = object[keep]
#     #return
#     return(object)
#   })
#
#
# methods::setMethod(
#   f = "mark",
#   signature = methods::signature(object = "MarkedList", learned = "missing", set = "character", config = "character"),
#   definition = function(
#     object, set, config, marking.system, logger){
#
#     #--------------------------------------------------------------------------------------------#
#     #get marked
#     object = get_marked(object = object, config = config, marking.system = marking.system, set = set);
#
#     #--------------------------------------------------------------------------------------------#
#     #remove auc if present
#     #get measures
#     measures = unlist(get_measure(marked.tmp))
#     #check
#     keep = !(measures %in% "auc")
#     #update
#     marked.tmp = marked.tmp[keep]
#
#     if(length(marked.tmp)>1){
#       #--------------------------------------------------------------------------------------------#
#       #get scores
#       scores = get_mark(marked.tmp)
#       #set df as list
#       scores = lapply(X = scores, FUN = function(x){split.default(x = x, f = names(x))})
#       #reshape
#       scores = do.call(what = Map, args = c(cbind, scores))
#       #compute mark
#       rp = lapply(X = scores, FUN = compute_rank_product)
#
#       #--------------------------------------------------------------------------------------------#
#       #select positively associated
#       rp.pos = lapply(X = rp, FUN = subset, select = 'pfp_pos', drop = F)
#       nm = names(rp.pos)
#       rp.pos = do.call(what = cbind, args = rp.pos)
#       #set name
#       colnames(rp.pos) = nm
#
#       mark.pos = Marked(
#         mark           = as.data.frame(rp.pos),
#         config         = config,
#         type.measure   = "pfp_pos",
#         set            = set,
#         marking.system = marking.system
#       )
#
#       #--------------------------------------------------------------------------------------------#
#       #select positively associated
#       rp.neg = lapply(X = rp, FUN = subset, select = 'pfp_pos', drop = F)
#       nm = names(rp.neg)
#       rp.neg = do.call(what = cbind, args = rp.neg)
#       #set name
#       colnames(rp.neg) = nm
#
#       mark.neg = Marked(
#         mark           = as.data.frame(rp.neg),
#         config         = config,
#         type.measure   = "pfp_neg",
#         set            = set,
#         marking.system = marking.system
#       )
#
#       #--------------------------------------------------------------------------------------------#
#       #list
#       out = list(pfp_pos = mark.pos, pfp_neg = mark.neg)
#     } else {
#       out = list()
#     }
#
#     #--------------------------------------------------------------------------------------------#
#     #convert
#     # out = MarkedList(out)
#
#     #--------------------------------------------------------------------------------------------#
#     return(out)
#   })
#
# methods::setMethod(
#   f = "mark",
#   signature = methods::signature(object = "MarkedList", learned = "missing", set = "character", config = "missing"),
#   definition = function(
#     object, set, config, marking.system, logger){
#
#     #--------------------------------------------------------------------------------------------#
#     #Set logger
#     logger = open_con(logger)
#
#     #--------------------------------------------------------------------------------------------#
#     #opt config
#     log_debug(object = logger, message = "Marking features from best config (opt).", sep = "\n", add.level = TRUE, add.time = TRUE)
#     m.opt = mark(object = object, set = set, config = "opt", marking.system = marking.system, logger = logger)
#
#     #1se config
#     log_debug(object = logger, message = "Marking features from best config (1se).", sep = "\n", add.level = TRUE, add.time = TRUE)
#     m.1se = mark(object = object, set = set, config = "1se", marking.system = marking.system, logger = logger)
#
#     #out obj
#     out = list('opt' = m.opt, '1se' = m.1se)
#
#     #--------------------------------------------------------------------------------------------#
#     #close
#     close_con(logger)
#
#     #--------------------------------------------------------------------------------------------#
#     return(out)
#   })
#
# methods::setMethod(
#   f = "mark",
#   signature = methods::signature(object = "MarkedList", learned = "missing", set = "missing", config = "missing"),
#   definition = function(
#     object, set, config, marking.system, logger){
#
#     #--------------------------------------------------------------------------------------------#
#     #Set logger
#     logger = open_con(logger)
#
#     #--------------------------------------------------------------------------------------------#
#     #training set
#     log_debug(object = logger, message = "Grading features on training set.", sep = "\n", add.level = TRUE, add.time = TRUE)
#     m.train = mark(object = object, set = "train", marking.system = marking.system, logger = logger)
#
#     #testing set
#     log_debug(object = logger, message = "Grading features on testing set.", sep = "\n", add.level = TRUE, add.time = TRUE)
#     m.test  = mark(object = object, set = "test", marking.system = marking.system, logger = logger)
#
#     #whole set
#     log_debug(object = logger, message = "Grading features on full set.", sep = "\n", add.level = TRUE, add.time = TRUE)
#     m.full  = mark(object = object, set = "full", marking.system = marking.system, logger = logger)
#
#     #--------------------------------------------------------------------------------------------#
#     #out obj
#     out = list(train = m.train, test = m.test, full = m.full)
#
#     #--------------------------------------------------------------------------------------------#
#     #close
#     close_con(logger)
#
#     #--------------------------------------------------------------------------------------------#
#     return(out)
#   })
