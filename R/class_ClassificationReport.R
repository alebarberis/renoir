# #' @include classes_generics.R
# NULL
#
# #' ClassificationReport Class
# #' An S4 class to represent a classification report
# #'
# #' This subclass extends the S4  \linkS4class{List} virtual class by using
# #' the  \linkS4class{SimpleList} implementation.
# #'
# #' The object consists of 4 slots
# #' @slot listData \code{list} storing the list elements
# #' @slot elementType the type of data represented in the sequence
# #' @slot elementMetadata  \linkS4class{DataFrame} storing the element-wise metadata,
# #' with a row for each element and a column for each metadata available. Default is \code{NULL}
# #' @slot metadata \code{list} storing the global metadata annotating the object as a whole
# methods::setClass(
#   Class = "ClassificationReport",
#   slots = c(
#     TP         = "integer",
#     TN         = "integer",
#     FN         = "integer",
#     FP         = "integer",
#     P          = "integer",
#     N          = "integer",
#     PP         = "integer",
#     PN         = "integer",
#     TPR        = "double",
#     FPR        = "double",
#     FNR        = "double",
#     TNR        = "double",
#     FDR        = "double",
#     PPV        = "double",
#     FOR        = "double",
#     NPV        = "double",
#     LRpos      = "double",
#     LRneg      = "double",
#     MK         = "double",
#     DOR        = "double",
#     prevalence = "double",
#     ACC        = "double",
#     BA         = "double",
#     F1         = "double",
#     FM         = "double",
#     MCC        = "double",
#     TS         = "double",
#     BM         = "double",
#     PT         = "double"
#   )
# )
