#'Create a confusion table in html
#'@param x a confusion matrix as returned by \code{\link{confusion_matrix}}
#'@inheritParams knitr::kable
#'@inheritParams base::round
#'@return A character vector of the table source code.
#'@keywords internal
#'@source https://haozhu233.github.io/kableExtra/awesome_table_in_html.html
#'@examples
#'\dontrun{
#'true = c(0, 1, 2, 2, 0)
#'pred = c(0, 0, 2, 1, 0)
#'x = confusion_matrix(true = true, pred = pred)
#'print.ConfusionMatrix(x = x)
#'print.ConfusionMatrix(x = x, format = 'html')
#'
#'true = c('dog', 'cat', 'dog', 'cat', 'dog', 'dog', 'cat', 'dog', 'cat', 'dog')
#'pred = c('dog', 'dog', 'dog', 'cat', 'dog', 'dog', 'cat', 'cat', 'cat', 'cat')
#'x = confusion_matrix(true = true, pred = pred)
#'print.ConfusionMatrix(x = x)
#'print.ConfusionMatrix(x = x, format = 'html')
#'
#'true = sample(x = 1:4, size = 10, replace = T)
#'pred = sample(x = 1:4, size = 10, replace = T)
#'x = confusion_matrix(true = true, pred = pred)
#'
#'print.ConfusionMatrix(x = x, format = 'html')
#'}
print.ConfusionMatrix = function (
  x, digits = max(3, getOption("digits") - 3), format = c('text', 'html'),
  html.pkg = c("DT", "kable"),
  taglist = T,
  ...) {


  format   = match.arg(format)
  html.pkg = match.arg(html.pkg)

  if(identical(format, "text")){
    NextMethod("print");
  }else{

    out = switch(
      html.pkg,
      DT    = confusion_matrix_dt(x = x, digits = digits, ...),
      kable = confusion_matrix_kable(x = x, digits = digits, format = format, ...)
    )

    if(taglist){out = htmltools::tagList(out)}

    print(out)
  }
}





print.BinaryClassificationReport <- function(x, short = T){

  measures.to.report.short = c("ACC", "TPR", "TNR", "PPV", "NPV", "PRV", "F1S", "BAS", "MCC")
  measures.to.report.long  = c(measures.to.report.short,
                               "FDR", "FOR", "FNR", "FPR", "DOR", "CSI")


  measures.to.report = if(short){measures.to.report.short}else{measures.to.report.long}

  # cat("\nClassification report\n\n")

  xsub = x[measures.to.report]

  #Paste name and values
  outstring = paste(get_measure_names(type.measure = names(xsub)), round(x = unlist(xsub), digits = 3), sep = " : ")
  # outstring = paste(get_measure_names(type.measure = names(xsub)), xsub, sep = " : ")

  #Merge different scores and separate by new line
  outstring = paste(outstring, collapse = "  \n")

  #add new lines
  outstring = paste(outstring, "\n\n")

  #add positive class
  outstring = paste0(outstring, paste("'positive' class", x$pos, sep = " : "))

  #add new line
  outstring = paste(outstring, "\n")

  #print
  # cat(print(outstring))
  # print(x = cat(outstring))
  cat(outstring)
  # cat("\n\n")
  # cat(paste("'positive' class", x$pos, sep = " : "), sep = "\n")
}

print.ClassificationReport <- function(
  x, short = T, report = c("plain", "table"),
  format = c('text', 'html'), taglist = T,
  header=''){

  report = match.arg(report)
  format = match.arg(format)

  measures.to.report.short = c("ACC", "TPR", "TNR", "PPV", "NPV", "PRV", "F1S", "BAS", "MCC")
  measures.to.report.long  = c(measures.to.report.short,
                               "FDR", "FOR", "FNR", "FPR", "DOR", "CSI")


  measures.to.report = if(short){measures.to.report.short}else{measures.to.report.long}

  cat("\n\n")
  cat(paste(header, "Confusion Matrix", "\n\n"))
  # cat(paste("\n", header, "Confusion Matrix", "\n\n"))
  print(x$confusion, format = format, taglist = taglist)
  # if(identical(format, "html")){
  #   print(print.ConfusionMatrix(x$confusion, format = format))
  # } else {print.ConfusionMatrix(x$confusion, format = format)}
  cat("  \n\n")

  cat(paste(header, "Classification report  \n\n"))
  nclasses = length(x$classes)
  if(identical(report, "plain")){
    for(i in seq(nclasses)){
      print.BinaryClassificationReport(x = x$classes[[i]], short = short)
      cat("\n\n")
    }
  } else {
    binary = do.call(what = cbind, args = x$classes)
    #colnames
    colnames(binary) = binary["pos",]
    binary = binary[-match(x = "pos", table = rownames(binary)),]
    #subset
    binary = binary[measures.to.report,,drop=F]
    #change name
    rownames(binary) = get_measure_names(type.measure = rownames(binary))
    #print
    if(identical(format, "html")){
      # suppressWarnings(print( htmltools::tagList(DT::datatable(data = binary,
      #                                         selection = 'none',
      #                                         height = 100,
      #                                         width = "100%",
      #                                         # height = "auto",
      #                                         autoHideNavigation = TRUE,
      #                                         rownames = T))))
      suppressWarnings(print( htmltools::tagList(create_dt(
        data = binary,
        scrollX = T,
        rownames = T
      ))))

      cat("  \n\n")
    } else {
      print(binary)
    }
  }


  cat("\nAverage measures\n\n")
  overall = do.call(what = cbind, args = x[["overall"]])
  #change name
  rownames(overall) = get_measure_names(type.measure = rownames(overall))
  if(identical(format, "html")){
    print(knitr::kable(x = overall, format = format, output = TRUE, align = 'c'))
  } else{
    print(overall)
  }

}


print.PcaReport <- function(x, header='', ...){

  nout = length(x)

  is.multi = isTRUE(nout>1)

  cat("\n\n")
  cat(paste(header, "PCA", "\n\n"))
  # cat(paste("\n", header, "PCA", "\n\n"))

  if(!identical(header, '')){header = paste0(header, '#')}

  names_out = names(x);
  loop = if(!is.null(names_out)){names_out}else{seq(nout)}
  for(iout in loop){

    if(is.multi){
      cat(paste0(header, " Outcome category: ", iout, " \n\n"))
    }

    for(i in seq(length(x[[iout]]))){

      if(!is.null(x[[iout]][[i]]$obj)){

        # cat(paste(header, x[[iout]][[i]]$title, "\n\n"))

        print(x[[iout]][[i]]$obj)
        cat('\n\n')
      }


    }

    cat('\n\n')

  }

}

print.TestReport <- function(x, header='', ...){

  nout = length(x)

  is.multi = isTRUE(nout>1)

  cat("\n\n")
  cat(paste(header, "Test", "\n\n"))
  # cat(paste("\n", header, "Test", "\n\n"))

  if(!identical(header, '')){header = paste0(header, '#')}

  names_out = names(x);
  loop = if(!is.null(names_out)){names_out}else{seq(nout)}
  for(iout in loop){

    if(is.multi){
      cat(paste0(header, " Outcome category: ", iout, " \n\n"))
    }

    if(!is.null(x[[iout]]$obj)){

      for(i in seq(length(x[[iout]]$obj))){
        print(x[[iout]]$obj[[i]])
        cat(' \n\n')
      }
      # print(x[[iout]]$obj)
      # cat('\n\n')
    }

    cat('\n\n')

  }

}
