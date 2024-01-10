#' @include classes_generics.R performance.R utils.R
NULL


#Create an interactive report
#@description Create an interactive report for the computed analysis.
#@param object the output of \code{\link{run}}
#@param report.type the type of report to create. If \code{short}, the feature importance tables
#are not created (this could solve the pandoc "out of memory" issue when the number of considered features is high)
#@inheritParams run
#@param annotation a data frame containing features annotation
#@param feat.index numeric, the index of the column in \code{annotation} matching \code{x} feature names.
#\code{0} means rownames
#@param ... further arguments to \code{\link[rmarkdown]{render}} \code{params} argument. Possible options are:
#\describe{
#   \item{\code{yml_author}}{A character vector, name of the author(s)}
#   \item{\code{yml_affiliation}}{The author’s affiliation; must match length of name,
#   e.g. if name has length of two, affiliation must as well; use NA if you don’t want
#   to include an affiliation for a given author.Note that not all formats support the affiliation field.}
#   \item{\code{yml_email}}{The author email address. Note that not all formats support the email field.}
#   \item{\code{yml_title}}{A character vector, the title of the document}
#   \item{\code{yml_subtitle}}{A character vector, the subtitle of the document.
#   Not all R Markdown formats use subtitles, so it may depend on what you use in the output field.
#   It is available in \code{pdf_document}, \code{html_document}, and \code{word_document} by default}
#   \item{\code{yml_abstract}}{A character vector, the abstract. Long character vectors
#   are automatically wrapped using valid YAML syntax.
#   This field is not available in all output formats; it is available in \code{pdf_document} and \code{html_document} by default.}
#   \item{\code{yml_name}}{A character vector, the name of the document}
#  \item{\code{yml_toc}}{logical, whether to use a Table Of Contents}
#  \item{\code{yml_toc_depth}}{integer, the depth of headers to use in the TOC}
#  \item{\code{yml_toc_float}}{logical, to float the table of contents to the left of the main document}
#   \item{\code{tabset}}{logical, whether to use tabbed sections (if any)}
#}
#@return The compiled report is written into the output file.
#@author Alessandro Barberis
# create_report_old <- function(
#                           #General Options
#                           output_format = "html_document",
#                           outdir = tempdir(),
#                           filename = NULL,
#                           report.type = c("full", "short"),
#                           #Params
#                           object,
#                           annotation = NULL,
#                           feat.index = 0,
#                           ...){
#
#   # needed_args = c("x", "y", "object", "resp.type")
#   needed_args = c("object")
#   provided_args = names(as.list(match.call())[-1])
#
#   report.type = match.arg(report.type)
#
#   if(any(!(needed_args %in% provided_args))){stop(paste("Missing values:",setdiff(x = needed_args, y = provided_args),"must be provided."))}
#
#   name = switch(object[['resampling.method']],
#                 multi.random = 'multirandom')
#
#   # filename_report_rmd = paste0('report_',name, '.Rmd')
#   filename_report_rmd = if(identical(report.type, "full")){paste0('report_',name, '.Rmd')}else{paste0('report_',name, '_short.Rmd')}
#
#   #get the path to the r markdown
#   filepath = system.file("rmd", filename_report_rmd, package = "renoir");
#
#   #Render the rmarkdown and generate the report
#   rmarkdown::render(input = filepath,
#                     output_format = output_format,
#                     output_file = filename,
#                     output_dir = outdir,
#                     clean = TRUE,
#                     params = c(list(object = object,
#                                     annotation = annotation,
#                                     feat.index = feat.index),
#                                list(...)))
# }

#@param object Trained or Tuned object
report_confusion_matrix <- function(object, newx, newy, weights, newoffset){

}

#@param object list containing 2 slots for each element: title and obj
print_report_model_accuracy <- function(
  object, header = ""){

  for(obj in object){
    # cat(paste(header, obj$title, '\n' ))
    # print(obj$obj)
    # cat('\n\n')
    if(inherits(obj$obj,"ClassificationReport")){
      print.ClassificationReport(obj$obj, format = "html", header = header, report = c("plain", "table")[2])
    } else if(inherits(obj$obj,"PcaReport")){
      print.PcaReport(x = obj$obj, header=header)
    } else if(inherits(obj$obj,"TestReport")){
      print.TestReport(x = obj$obj, header=header)
    } else {
      if(!is.null(obj$obj)){
        cat("\n")
        cat(paste(header, obj$title, '\n' ))
        print(obj$obj)
      }
    }
    cat('\n\n')
  }
}

#@param object Trained object
report_model_accuracy <- function(
  object, newx, newy, weights, newoffset,
  set = c("train", "test", "full", "new"),
  observations = NULL,
  features = NULL,
  resp.type,
  strata = NULL,
  header = "",
  logger){


  #set default
  out = NULL

  #check arg
  set = match.arg(set)


  #check set
  if(isFALSE(identical(set, "new"))){
    #test on evaluation data

    ##subset if needed
    if(isFALSE(identical(set, "full")) && isTRUE(!is.null(observations))){
      ##get x obs
      nobs = get_nobs(newx)
      indices = seq(nobs)
      ##check
      indices = switch(
        set,
        train = observations,
        test  = setdiff(x = indices, y = observations),
        full  = indices
      )

      #subset
      newx      = subset_observations(object = newx, which = indices)
      newoffset = subset_observations(object = newoffset, which = indices)
      newy      = subset_observations(object = newy, which = indices)
      weights   = subset_observations(object = weights, which = indices)
    }


  }

  #--------------------------------------------------------------------------------------------#
  #logger
  if(missing(logger)){logger = Logger(verbose = F)}
  #open connection
  logger = open_con(logger)

  #--------------------------------------------------------------------------------------------#
  #test on data
  if(identical(x = resp.type, y = "binomial") || identical(x = resp.type, y = "multinomial")){

    out1 = report_logistic_model_accuracy_class_report(object, newx, newy, weights, newoffset, logger)

    out2 = report_logistic_model_accuracy_roc_plot(
      object, newx, newy, weights, newoffset,
      prefix = "plot_roc_r",
      interactive = F
    )

    if(isTRUE(missing(strata) && is.factor(newy))){strata = newy}
    out3 = report_model_accuracy_pca_plot(object, newx, strata)

    out4 = report_test_between_strata(object = object, newx = newx, strata = strata)

    # cat(paste(header,'Classification report', '\n' ))
    # print(out1)
    # cat('\n\n')

    # cat(paste(header,'ROC curve', '\n' ))
    # print(out2)
    # cat('\n\n')

    out = list(
      list(title = 'Classification report', obj = out1),
      list(title = 'ROC curve', obj = out2),
      list(title = 'PCA', obj = out3),
      list(title = 'Test', obj = out4)
    )
  } else {
    out = list()
  }

  #--------------------------------------------------------------------------------------------#
  #close connection
  close_con(logger)

  #--------------------------------------------------------------------------------------------#
  return(out)
}


report_logistic_model_accuracy_class_report <- function(
  object, newx, newy, weights, newoffset, logger
  ){

  #--------------------------------------------------------------------------------------------#
  #logger
  if(missing(logger)){logger = Logger(verbose = F)}
  #open connection
  logger = open_con(logger)

  #--------------------------------------------------------------------------------------------#
  #get forecaster
  forecaster = Forecaster(id = get_learning_method(object))

  #--------------------------------------------------------------------------------------------#
  # newx = subset_features(object = newx, which = unique(unlist(features(object = object, type = "all"))))

  #--------------------------------------------------------------------------------------------#
  #for each required score select the needed prediction type
  log_trace(object = logger, message = paste("Check prediction type"), sep = "\n", add.level = TRUE, add.time = TRUE)
  pred.type = get_prediction_type(object = forecaster, type.measure = "class_report")

  #--------------------------------------------------------------------------------------------#
  #predict
  log_info(object = logger, message = paste("Computing prediction..."), sep = "", add.level = TRUE, add.time = TRUE)
  pred = forecast(
    forecaster = forecaster,
    models = object,
    newx = newx,
    type = pred.type,
    newoffset = newoffset,
    check.newx = T)
  log_info(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #-------------------------------------------------------------------------------------------#
  #Compute score estimate and standard error
  log_info(object = logger, message = paste("Computing accuracy"), sep = "\n", add.level = TRUE, add.time = TRUE)

  out = classification_report(true = newy, pred = pred)

  #--------------------------------------------------------------------------------------------#
  #close connection
  close_con(logger)

  #--------------------------------------------------------------------------------------------#
  return(out)

}


report_logistic_model_accuracy_roc_plot <- function(
  object, newx, newy, weights, newoffset,
  prefix = "plot_roc_r",
  interactive = F,

  ...
){
  #get forecaster
  forecaster = Forecaster(id = get_learning_method(object))

  #--------------------------------------------------------------------------------------------#
  #for each required score select the needed prediction type
  pred.type = get_prediction_type(object = forecaster, type.measure = "roc")

  #--------------------------------------------------------------------------------------------#
  #predict
  # log_info(object = logger, message = paste("Computing prediction..."), sep = "", add.level = TRUE, add.time = TRUE)
  pred = forecast(
    forecaster = forecaster,
    models = object,
    newx = newx,
    type = pred.type,
    newoffset = newoffset,
    check.newx = T)
  # log_info(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #-------------------------------------------------------------------------------------------#
  #Compute score estimate and standard error
  # log_info(object = logger, message = paste("Computing accuracy"), sep = "\n", add.level = TRUE, add.time = TRUE)
  out = plot_roc(
    true = newy,
    pred = pred,
    # name,
    # add.auc = F,
    interactive = interactive,
    prefix = prefix,
    ...)

  #-------------------------------------------------------------------------------------------#
  return(out)
}



report_model_accuracy_pca_plot <- function(object, newx, strata = NULL, do.warning = F){

  #check strata
  # if(isTRUE(missing(strata) || is.null(strata) || is.na(strata))){return()}
  if(isFALSE(missing(strata) || is.null(strata) || is.na(strata))){
    if(is.vector(strata)){strata = factor(strata)}
    # if(!is.factor(strata)){return()}

    #order
    if(is.null(row.names(newx))){
      if(do.warning) warning("'newx' has no row names: 'strata' is assumed in same order")
    }else if(is.null(names(strata))){
      if(do.warning) warning("'strata' has no name: observations are assumed in the same order as 'newx'")
    } else if(!is.null(row.names(newx)) && !is.null(names(strata))){
      #inter
      inter = intersect(x = row.names(newx), y = names(strata))
      #check
      if(isTRUE(length(inter)<1)){stop("Not matching observation names for 'strata' and 'newx'")}
    }
  }




  #Get features with non-zero coefficients
  feats = features(object = object, type = "nonzero")

  #
  nout = length(feats)
  #check if multi response
  is.multi = isTRUE(is.list(feats) && nout > 1)

  #output
  plots = list()

  #Check names
  names_out = names(feats);
  loop = if(!is.null(names_out)){names_out}else{seq(nout)}
  for(iout in loop){

    feats_nz_i = unlist(feats[[iout]])

    if(length(feats_nz_i)>0){
      #rm NAs
      x_no_na = stats::na.omit(object = subset_features(object = newx, which = feats_nz_i))

      if(!is.null(dim(x_no_na)) && all(dim(x_no_na)>0)){
        #Compute PCA
        PCA <- stats::prcomp(x = x_no_na, scale = F)
      } else {
        PCA = NULL
      }
      rm(x_no_na)
    } else {
      PCA = NULL
    }

    if(!is.null(PCA)){
      obj1 = plot_pca(PCA = PCA,
                      pc = c(1,2),
                      groups = data.frame(groups = strata, stringsAsFactors = F))

      obj2 = plot_pca(PCA = PCA,
                      pc = c(3,4),
                      groups = data.frame(groups = strata, stringsAsFactors = F))

      # #test on PC1
      # test_on_pc1 = compute_test_on_PC(PCA = PCA, pc = "PC1", groups = strata, verbose = F)
      #
      # #check strata is matching PCA$x
      # inter = intersect(x = rownames(PCA$x), y = names(strata))
      # has.inter = (!is.null(inter) && length(inter)>0)
      #
      # if(isTRUE((is.null(inter) && nobs(PCA$x) == nobs(strata)) || has.inter)){
      #
      #   if(has.inter){
      #     df.data = data.frame(data = PCA$x[inter,'PC1'], strata = strata[inter], stringsAsFactors = F)
      #   } else {
      #     df.data = data.frame(data = PCA$x[,'PC1'], strata = strata, stringsAsFactors = F)
      #   }
      #
      #   df.test = format_score(x = test_on_pc1)
      #
      #
      #   #plot
      #   obj3 = create_violin_plot(
      #     #Data
      #     df.data = df.data,
      #     df.test = df.test,
      #
      #     #Plot
      #     add.boxplot  = T,
      #     add.jitter   = T,
      #     add.test     = T,
      #
      #     #Axes
      #     xstring   = 'strata',
      #     ystring   = 'data',
      #     colstring = 'strata',
      #     xlab      = "Group",
      #     ylab      = 'PC1',
      #
      #     #Facet
      #     scales    = "free_x",
      #
      #     #Legend
      #     legend.title = "Group"
      #     # ,
      #     #
      #     # #Further args to ggplot
      #     # ...
      #   )
      # } else {obj3 = NULL}
      #
      # rm(inter, test_on_pc1)
    } else {
      obj1 = NULL;
      obj2 = NULL;
      # obj3 = NULL;
    }

    # plots[[iout]] = list(obj1 = obj1, obj2 = obj2, obj3 = obj3)
    plots[[iout]] = list(
      list(title = 'PC1 vs PC2', obj = obj1),
      list(title = 'PC3 vs PC4', obj = obj2)
      # ,list(title = 'Test on PC1', obj = obj3)
    )

  }

  class(plots) = c("PcaReport", class(plots))

  return(plots)
}


report_test_between_strata <- function(object, newx, strata = NULL, do.warning = F, na.rm = T){

  #Get features with non-zero coefficients
  feats = features(object = object, type = "nonzero")

  #
  nout = length(feats)
  #check if multi response
  is.multi = isTRUE(is.list(feats) && nout > 1)

  #output
  plots = list()

  #Check names
  names_out = names(feats);
  loop = if(!is.null(names_out)){names_out}else{seq(nout)}
  for(iout in loop){
    feats_nz_i = unlist(feats[[iout]])

    if(length(feats_nz_i)>0){

      #summarise features
      df = suppressWarnings(summarise_features(x = newx, features = feats_nz_i, na.rm = na.rm))

      #reshape to plot
      df.data = unlist(data.frame(df))
      #as df
      df.data = data.frame(value = df.data, stringsAsFactors = F)
      df.data$score  = unlist(lapply(X = colnames(df), FUN = rep, times = nrow(df)))
      df.data$obs    = rep(x = rownames(df), times = ncol(df))
      df.data$groups = rep(x = strata, times = ncol(df))
      # unlist(lapply(X = rownames(df), FUN = rep, times = ncol(df)))

      #compute test on strata
      ctbs = suppressWarnings(compute_test_between_strata(scores = df, strata = strata, na.rm = na.rm))

      #format
      df.test = format_score(
        x = data.frame(score = ctbs[,1], row.names = rownames(ctbs), stringsAsFactors = F),
        score = "p",
        name  = get_name_screener_by_id(id = colnames(ctbs)[1], short = T)
      )
      df.test$score = rownames(df.test)


    } else {
      df.data = df.test = NULL
    }

    if(!is.null(df.data)){
      # obj1 = create_violin_plot(
      #   df.data = df.data,
      #   df.test = df.test,
      #   xstring = "groups",
      #   ystring = "value",
      #   xlab      = "Group",
      #   ylab = "Score",
      #   colstring = "groups",
      #   add.test     = T,
      #   facet.rows = ggplot2::vars(score)
      #   # facet.cols = ggplot2::vars(site)
      # )

      obj1 = lapply(X = colnames(df), FUN = function(var) {
        create_violin_plot(
          df.data = df.data[df.data$score == var,],
          df.test = df.test[df.test$score == var,],
          xstring = "groups",
          ystring = "value",
          xlab      = "Group",
          ylab = var,
          colstring = "groups",
          add.test     = T
          # facet.rows = ggplot2::vars(score)
          # facet.cols = ggplot2::vars(site)
        )
      })
    } else {
      obj1 = NULL
    }

    #store
    plots[[iout]] = list(title = paste("Outcome:", iout), obj = obj1)

    rm(df.data, df.test, obj1)
  }

  class(plots) = c("TestReport", class(plots))

  return(plots)
}

#'Summarisation of features as single score
#'@description Summarise the set of features as a single score to
#'allow easy comparison between observations
#'@param x the input matrix, where rows are observations and columns are variables
#'@param features the features to summarise. If \code{features} is an integer vector,
#'values are considered as \code{x} column indices, whether if is a character vector
#'it must match the \code{x} column names. If \code{NULL}, then all the columns of
#'\code{x} are considered
#'@param method the method to use for summarising the set of features. Six methods
#'are currently supported
#'@param na.rm logical, whether to remove NAs
#'@param ... further arguments to \code{method}
#'@return a named (if \code{x} has row names) vector containing one summary
#'score per observation
#'@author Alessandro Barberis
#'@examples
#'\dontrun{
#' nvar = 20
#' nobs = 100
#' x = matrix(
#'   data = rnorm(n = nobs*nvar, mean = 3, sd = 0.2), nrow = nobs, ncol = nvar,
#'   dimnames = list(paste0("O",seq(nobs)), paste0("V", seq(nvar)))
#' )
#'
#' sscore = summarise_as_single_score(x = x, features = colnames(x)[1:5], method = "mean")
#' sscore = summarise_as_single_score(x = x, features = colnames(x)[1:5], method = "median")
#' sscore = summarise_as_single_score(x = x, features = colnames(x)[1:5], method = "pc1")
#' sscore = summarise_as_single_score(x = x, features = colnames(x)[1:5], method = "gsva")
#' sscore = summarise_as_single_score(x = x, features = colnames(x)[1:5], method = "ssgsea")
#' sscore = summarise_as_single_score(x = x, features = colnames(x)[1:5], method = "plage")
#' }
#'
#'@keywords internal
summarise_features_as_single_score <- function(
  x,
  features = NULL,
  method = c("mean", "median", "pc1", "gsva", "ssgsea", "plage", "zscore"),
  na.rm = T,
  log.warning = F,
  ...){

  method = match.arg(method)

  #check features
  if(is.null(features)){features = seq(ncol(x))}

  if(is.character(features)){
    ifeatures = match(x = features, table = colnames(x))
  }

  if(is.integer(features)){
    ifeatures = features
    features = colnames(x)[ifeatures]
  }

  #compute
  if(any(is.na(features) | is.na(ifeatures))){
    out = NULL
    if(log.warning){warning("Check the provided input: 'features' is not matching 'x' columns.")}
  } else {
    out = switch(
      method,
      # 'mean'   = rowMeans(x = x, na.rm = na.rm),
      'mean'   = matrixStats::rowMeans2( x = x, cols = ifeatures, na.rm = na.rm, useNames = T),
      'median' = matrixStats::rowMedians(x = x, cols = ifeatures, na.rm = na.rm, useNames = T),
      'pc1'    = summarise_features_as_single_score_by_PCA(x = x, features = features, na.rm = na.rm),
      'gsva'   = unlist(t(GSVA::gsva(expr = t(x), gset.idx.list = list(gs1 = features), verbose=FALSE, method = "gsva", ...))[,1]),
      'ssgsea' = unlist(t(GSVA::gsva(expr = t(x), gset.idx.list = list(gs1 = features), verbose=FALSE, method = "ssgsea", ...))[,1]),
      'plage'  = unlist(t(GSVA::gsva(expr = t(x), gset.idx.list = list(gs1 = features), verbose=FALSE, method = "plage"), ...)[,1]),
      'zscore' = unlist(t(GSVA::gsva(expr = t(x), gset.idx.list = list(gs1 = features), verbose=FALSE, method = "zscore"), ...)[,1])
    )
  }

  return(out)

}

summarise_features_as_single_score_by_PCA <- function(x, features = NULL, na.rm = T){

  x = subset_features(object = x, which = features)

  #rm NAs
  if(na.rm){
    x = stats::na.omit(object = x)
  }

  #compute PCA
  if(!is.null(dim(x)) && all(dim(x)>0)){
    PCA <- stats::prcomp(x = x, scale = F)
    #select first component
    out = PCA$x[,1]
  } else {
    out = NULL
  }
  #return
  return(out)
}


summarise_features <- function(
  x,
  features = NULL,
  method = c("mean", "median", "pc1", "gsva", "ssgsea", "zscore", "plage"),
  na.rm = T,
  log.warning = F,
  ...){

  #apply
  out = sapply(X = method, FUN = summarise_features_as_single_score, x = x, features = features, na.rm = na.rm, log.warning = log.warning, ..., simplify = "data.frame")

  #set observations
  # rownames(out) = rownames(x)

  #report as df
  if(!is.data.frame(out)){
    out = as.data.frame(x = out, row.names = rownames(x))
  }

  #return
  return(out)
}


compute_test_between_strata <- function(scores = NULL, x, features = NULL, strata = NULL, na.rm = T){

  if(!is.null(strata)){
    #factor
    strata = as.factor(strata)

    #levels
    nlevels = length(levels(strata))

    #resp
    resp.type = ifelse(test = nlevels > 2, yes = "multinomial", no = "binomial")

    #summarise features
    if(is.null(scores)){
      scores = summarise_features(x = x, features = features, na.rm = na.rm)
    }

    #compute test
    out = screening_by_default(x = scores, y = strata, resp.type = resp.type)

    #
    out = data.frame(test = out, row.names = colnames(scores), stringsAsFactors = F)
    colnames(out) = get_id_default_screener_test(resp.type = resp.type, y = strata)
  } else {
    out = NULL
  }

  return(out)
}





#'Compute significance on PC
#'@description Computes wilcoxon or Kruskal-Wallis test for the given groups
#'on the selected principal components
#'@param PCA principal component analysis
#'@param pc which principal component to consider
#'@param groups groups to consider while performing the test
#'@param ... further arguments to compute_score
#'@return non adjusted p-value
#'
#'@keywords internal
compute_test_on_PC <- function(PCA, pc = "PC1", strata = NULL, survival = NULL, verbose = TRUE, ...){

  # out = data.frame(stringsAsFactors = F)
  out = list()

  if(!is.null(strata)){
    # out$strata = compute_test_on_PC_by_strata(PCA = PCA, pc = pc, groups = strata, verbose = verbose, ...)
    out = compute_test_on_PC_by_strata(PCA = PCA, pc = pc, groups = strata, verbose = verbose, ...)
  }

  if(!is.null(survival)){

  }

  return(out)
}

compute_test_on_PC_by_strata <- function(PCA, pc = "PC1", groups, verbose = TRUE, ...){

  #default
  method = "test"
  out = NULL

  if(!is.null(PCA)){
    #store rownames
    rnames = rownames(PCA$x);

    #get selected principal component
    PCx = PCA$x[, pc, drop=F]

    #order
    if(is.null(rnames)){
      warning("The rotated data (PCA$x) has no name: 'groups' is assumed in same order")
    }else if(is.null(names(groups))){
      warning("'groups' has no name: it is assumed in same order as the rotated data (PCA$x)")
    } else if(!is.null(rnames) && !is.null(names(groups))){
      #order
      groups = groups[rnames]
      #check
      if(isTRUE(is.na(groups))){return(out)}
    }

    #Get number of classes
    n.classes = length(levels(as.factor(groups)))

    #compute test
    if(verbose){cat(paste(Sys.time(), "Computing test considering", n.classes, "groups..."), sep = "")}
    if(n.classes == 2) {

      out = switch(method,
                   'test'               = stats::setNames(
                     object = screening_default_binomial(x = t(PCx), y = groups),
                     nm = get_id_default_screener_test(resp.type = "binomial", y = groups)),
                   'mutual.information' = mutual_information(x = t(PCx), g = groups, verbose = verbose, ...))
    } else {
      if(is.null(method) || identical(method, 'test')){
        out = screening_default_multinomial(x = t(PCx), y = groups)
        names(out) = get_id_default_screener_test(resp.type = "multinomial", y = groups)
      } else {
        out = NA
      }

    }
    if(verbose){cat("DONE", sep = "\n")}
  } else {
    out = NULL
  }


  return(out)
}

compute_test_on_PC_by_survival <- function(PCA, pc = "PC1", survival, verbose = TRUE, ...){
  out = screening_default_cox(x=PCA$x[,pc], y=survival, ...)

  names(out) = get_id_default_screener_test(resp.type = "cox")

  return(out)
}





