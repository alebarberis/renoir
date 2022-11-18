#' @include classes_generics.R class_Logger.R class_Looper.R class_Scorer.R class_ScorerList.R class_TunedList.R class_TrainedList.R class_ForecasterList.R class_Tester.R
NULL

#' Tester List Class
#'
#' An S4 class to represent a scorer list.
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
  Class = "TesterList",
  contains = "SimpleList",
  slots = c(listData = "list")
)


#'TesterList Constructor
#'
#' @title Constructor for the S4 TesterList object.
#'
#'Constructor for the S4 \linkS4class{TesterList} object.
TesterList <- function(...){

  obj = new("TesterList", S4Vectors::SimpleList(...))

  # S4Vectors::elementType(obj) = "Tester"
  obj@elementType = "Tester"
  obj

}
