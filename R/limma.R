#' @include classes_generics.R class_Logger.R
NULL

#'Linear model for series of arrays
#'@description Fit linear model for each gene given a series of arrays
#'@param x A matrix-like data object containing log-ratios or log-expression
#'         values for a series of arrays, with rows corresponding to genes and columns to samples.
#'@param design the design matrix of the microarray experiment, with rows corresponding to arrays
#'              and columns to coefficients to be estimated. Defaults to the unit vector meaning
#'              that the arrays are treated as replicates.
#'@param method fitting method; "ls" for least squares or "robust" for robust regression
#'@param contrasts numeric matrix with rows corresponding to coefficients in fit and columns
#'                 containing contrasts. May be a vector if there is only one contrast.
#'@param coef column number or column name specifying which coefficient or contrast of the linear
#'            model is of interest. For topTable, can also be a vector of column subscripts, in which
#'            case the gene ranking is by F-statistic for that set of contrasts.
#'            If coef has a single value, then the moderated t-statistics and p-values for that coefficient
#'            or contrast are used.
#'            If coef takes two or more values, the moderated F-statistics for that set of coefficients or
#'            contrasts are used. If coef is left NULL, then all the coefficients or contrasts in the fitted
#'            model are used, except that any coefficient named (Intercept) will be removed.
#'@param number maximum number of genes to list
#'@param adjust.method method used to adjust the p-values for multiple testing. Options, in
#'                     increasing conservatism, include "none", "BH", "BY" and "holm"
#'@param p.value cutoff value for adjusted p-value
#'@param assay.type Assay type: "array" for microarray data, "seq" for sequencing data
#'@param proportion numeric value between 0 and 1, assumed proportion of genes which are differentially expressed
#'@param top.table.sort.by character string specifying statistic to rank genes by.
#'Possible values for topTable are "logFC", "AveExpr", "t", "P", "p", "B"
#'or "none". (Permitted synonyms are "M" for "logFC", "A" or "Amean" for "AveExpr",
#'"T" for "t" and "p" for "P".)
#'@param outdir output directory
#'@param warningsFile A path to the warning file to use.
#'@param logFile A path to the log file to use.
#'@param plotVenn logical, whether to plot a Venn diagram showing numbers of genes signifcant in each comparison
#@param topF logical, whether the F statistics should be computed
#@param verbose logical. Should R report extra information on progress? Set to TRUE.
#'@param logger a \linkS4class{Logger}
#'@return A list with components (obtained from the different limma functions calls)
#'@keywords internal
limma <- function(
  #lmFit
  x, design=NULL, method="ls",
  weights = NULL,

  #contrasts.fit
  contrasts=NULL,

  #eBayes
  assay.type=c("array","seq"),
  proportion = 0.01,

  #topTable, decideTests,topTableF
  coef = NULL,
  number = Inf,
  adjust.method="BH",
  # p.value=0.05,
  p.value=1,
  top.table.sort.by = "none",

  #vennDiagram
  plotVenn = FALSE,


  outdir,
  logger = Logger()){

  #0) Get the list of parameters
  # this.call = c(match.call()[1], as.list(environment()));
  this.call = match.call()

  #---------------------------------------------------------------------------#
  assay.type = match.arg(assay.type);
  #---------------------------------------------------------------------------#

  if(!missing(outdir) && !is.null(outdir)){hasOutdir = T} else {hasOutdir = F}

  if(hasOutdir && !dir.exists(outdir)){dir.create(outdir, recursive = TRUE)}

  logger = open_con(logger)

  #------------------------------------------------------#
  #Fit the model (x has rows corresponding to genes and columns to samples)
  log_debug(object = logger, message = "Fitting a linear model for each gene given a series of arrays...", sep = "", add.level = TRUE, add.time = TRUE)
  limmaFit <- limma::lmFit(object = x, design = design, method = method, weights = weights)
  log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #------------------------------------------------------#

  if(!is.null(contrasts)){
    #Compute the coefficients and the standard errors for the given set of contrasts
    log_debug(object = logger, message = "Computing estimated coefficients and standard errors for a given set of contrasts...", sep = "", add.level = TRUE, add.time = TRUE)
    limmaFit <- limma::contrasts.fit(fit = limmaFit, contrasts = contrasts)
    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
  }

  #------------------------------------------------------#
  #Compute statistics on the model by empirical Bayes moderation of the standard errors towards a common value
  log_debug(object = logger, message = "Computing moderated t-/F-statistic, and log-odds of differential expression by empirical Bayes moderation of the standard errors towards a common value...", sep = "", add.level = TRUE, add.time = TRUE)
  limmaEBayes <- switch(assay.type,
                        array = limma::eBayes(fit = limmaFit, proportion=proportion, trend = FALSE),
                        seq   = limma::eBayes(fit = limmaFit, proportion=proportion, trend = TRUE))
  log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #------------------------------------------------------#

  #Get a list of top-ranked genes differential expressed in obese versus normal
  #from a previous linear model fit.
  #We adjust the p-value for multiple testing.
  log_debug(object = logger, message = "Extracting a table of the top-ranked genes from the linear model fit...", sep = "", add.level = TRUE, add.time = TRUE)

  limmaTop = limma::topTable(fit = limmaEBayes, coef=coef, number = number, adjust.method=adjust.method, p.value = p.value, sort.by=top.table.sort.by)

  if(hasOutdir){
    if(p.value!=1){
      limmaTop.p1 = limma::topTable(fit = limmaEBayes, coef=coef, number = number, adjust.method=adjust.method, p.value = 1, sort.by=top.table.sort.by)
    } else {
      limmaTop.p1 = limmaTop
    }
  }
  log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
  #------------------------------------------------------#

  #The outcome of each hypothesis test can be assigned using decideTests function.
  #decideTests identifies which genes are significantly differentially expressed for each contrast
  #from a fit object containing p-values and test statistics.
  #
  #The output of this function is essentially a numeric matrix with elements -1, 0 or 1
  #depending on whether each t-statistic is classified as significantly negative, not significant
  #or significantly positive.
  log_debug(object = logger, message = "Identifying which genes are significantly differentially expressed for each contrast...", sep = "", add.level = TRUE, add.time = TRUE)
  limmaTests <- limma::decideTests(limmaEBayes, adjust.method=adjust.method, p.value=p.value)
  log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #------------------------------------------------------#
  #Can't plot Venn diagram for more than 5 sets
  if(plotVenn && (is.null(dim(contrasts)) || (!is.null(dim(contrasts)) && dim(contrasts)[2]<=5))){
    #A Venn diagram showing numbers of genes signifcant in each comparison can be obtained from
    log_debug(object = logger, message = "Computing classification counts and drawing a Venn diagram....", sep = "", add.level = TRUE, add.time = TRUE)

    if(hasOutdir){
      resFilePath = file.path(outdir, paste0("limma_vennDiagram.pdf"));
      grDevices::pdf(file = resFilePath)

      limma::vennDiagram(limmaTests)

      grDevices::dev.off(); # to reset the graphics pars to defaults
    } else {
      limma::vennDiagram(limmaTests)
    }

    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
  }


  #------------------------------------------------------#

  # The statistic fit2$F and the corresponding fit2$F.p.value combine the three pair-wise comparisons
  # into one F-test. This is equivalent to a one-way ANOVA for each gene except that the residual
  # mean squares have been moderated between genes. To find genes which vary between the three RNA
  # targets in any way, look for genes with small p-values. To find the top 30 genes:
  # limmaTop30 = limma::topTableF(fit=limmaEBayes, number=30, adjust.method = "BH", p.value = 0.5)

  # if(topF){
  #   logMsg = paste(Sys.time(), "Extracting a table of the top-ranked genes from a linear model fit on the basis of moderated F-statistics...");
  #   if(verbose){cat(logMsg, sep="");}
  #   if(hasLog){cat(logMsg, file = log.con, sep="");}
  #
  #   limmaTopF = limma::topTableF(fit=limmaEBayes, number=number, adjust.method = adjust.method, p.value = p.value, sort.by = top.tableF.sort.by)
  #
  #   if(hasOutdir){
  #     if(p.value!=1){
  #       limmaTopF.p1 = limma::topTableF(fit=limmaEBayes, number=number, adjust.method = adjust.method, p.value = 1, sort.by = top.tableF.sort.by)
  #     } else {
  #       limmaTopF.p1 = limmaTopF
  #     }
  #   }
  #
  #
  #   logMsg = paste("DONE");
  #   if(verbose){cat(logMsg, sep="\n");}
  #   if(hasLog){cat(logMsg, file = log.con, sep="\n");}
  # }

  #------------------------------------------------------#
  #close connection
  close_con(object = logger)

  #------------------------------------------------------#
  limmaResList = list();
  limmaResList = list(limmaFit=limmaFit, limmaEBayes=limmaEBayes, limmaTop=limmaTop,
                      limmaTests=limmaTests)
  # if(topF) {limmaResList[["limmaTopF"]]=limmaTopF;}
  limmaResList[["call"]]=this.call;

  #------------------------------------------------------#

  if(hasOutdir){
    save(limmaResList, file=file.path(outdir, "limma_data.rda"));

    # if(!missing(design) && !is.null(design) && dim(design)[2]>2 && topF){
    #   limma.genes = row.names(limmaTopF);
    # } else {
      limma.genes = row.names(limmaTop);
    # }
    utils::write.csv(x = limma.genes, file = file.path(outdir, "significant_genes.csv"), row.names = F)
    utils::write.csv(x = limmaTop.p1, file = file.path(outdir, "limmaTop.csv"), row.names = T)
    # if(topF) {utils::write.csv(x = limmaTopF.p1, file = file.path(outdir, "limmaTopF.csv"), row.names = T)}

  }


  return(limmaResList);
}




#'Linear model for series of arrays
#'
#'@description Fit linear model for each feature given a series of observations.
#'This function is a wrapper for \code{\link[renoir]{limma}}
#'
#'@inheritParams limma
#'@param y the response variable
#'@param weights vector of observation weights
#'@param resp.type the response type
#'@param logged2 logical, whether data was logged 2
#'@param ... further arguments to \code{\link[renoir]{limma}}
#'
#'@return a list of element as returned by \code{\link[renoir]{limma}}
#'
#'@seealso
#'\code{\link{limma}}
#'
#'@keywords internal
#'
#'@author Alessandro Barberis
compute_limma <- function(x, y,
                          weights = NULL,
                          resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
                          logged2 = FALSE,
                          coef = NULL,
                          ...){
  args <- c(list(...));

  #LIMMA DOESN'T SUPPORT THESE TYPES
  if(identical(resp.type, 'poisson') || identical(resp.type, 'cox')){
    return(NULL)
  }

  #---------------------------------------------------------------------------#

  if(!missing(x)){
    rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");
  }

  #---------------------------------------------------------------------------#
  #Limma expects log2 data, so log the data if not already log2
  if(!logged2){
    x = log2(x);
  }

  #---------------------------------------------------------------------------#
  #Check y
  if(!is.null(args[['design']])){
    #get design matrix
    design = args[['design']];#this allows to pass a design matrix from the main renoir call as screening argument
    #check in common
    inter = intersect(x = colnames(x), y = rownames(design))
    #update
    if(length(inter)!=ncol(x)){
      warning("There is a problem with the provided design matrix. 'y' will be used as design matrix.")
      design = y;
    } else {
      design = design[inter,,drop=F]
      # #check if any empty column
      # is.zero = which(colSums(x = (design != 0), na.rm = TRUE) == 0)
      # #remove
      # if(length(is.zero)>0){
      #   design = design[,-is.zero,drop=F]
      # }
    }

  } else {
    if(identical(x = resp.type, y = "binomial") || identical(x = resp.type, y = "multinomial")){
      #check if y is a vector
      if(is.null(dim(y))){
        #Check if y is a factor, convert otherwise
        if(!is.factor(y)){ y = as.factor(y)}

        #Set the design matrix
        design = stats::model.matrix(object = ~0 + y);

        if(!is.null(names(y))){rownames(design) = names(y)}

        #clean colnames (removes the 'y' from names)
        colnames(design) = substring(text = colnames(design), first = 2)

      } else {
        #If y is a matrix, use as design matrix
        design = y;
      }
    } else {
      #Set the design matrix
      design = stats::model.matrix(object = ~0 + y);

      if(is.matrix(y) && !is.null(rownames(y))){rownames(design) = rownames(y)} else if(!is.null(names(y))){rownames(design) = names(y)}
    }
  }


  #check if any empty column
  is.zero = which(colSums(x = (design != 0), na.rm = TRUE) == 0)
  #remove
  if(length(is.zero)>0){
    design = design[,-is.zero,drop=F]
  }

  #---------------------------------------------------------------------------#

  #Set the contrast matrix
  if(identical(x = resp.type, y = "binomial") || identical(x = resp.type, y = "multinomial")){
    if(is.null(args[['contrasts']])){
      contrasts = utils::combn(colnames(design), m=2, FUN=paste, collapse="-");
      contrastMatrix <- limma::makeContrasts(contrasts = contrasts, levels=design);
    } else {
      contrastMatrix = args[['contrasts']]
    }
  } else {
    contrastMatrix = NULL;
  }

  #---------------------------------------------------------------------------#

  #Update arguments
  args[['x']]         = x;
  args[['weights']]         = weights;
  args[['design']]    = design;
  # args[['topF']]      = if(identical(x = resp.type, y = "multinomial") || identical(x = resp.type, y = "mgaussian")){TRUE}else{FALSE};
  args[['contrasts']] = contrastMatrix
  args[['coef']]   = coef;

  #clean to reduce space in memory
  rm(x)

  #---------------------------------------------------------------------------#

  #Design matrix should be full rank
  if(limma::is.fullrank(design)){

    limma.res = do.call(what = limma, args = args);

  } else {
    limma.res = NULL;
  }

  #---------------------------------------------------------------------------#


  return(limma.res);

}


#'Extract adjusted p-value from limma result
#'@param obj object returned from \code{\link{compute_limma}}
#'@inheritParams limma
#'@return the adjusted p-values of the test
extract_pval_limma <- function(obj,
                               coef = NULL,
                               feat.order = NULL,
                               pval = 'adj.P.Val'){

  if(!is.null(obj)){
    # #Limma
    # design = obj[["limmaFit"]][["design"]]
    #
    # topF = obj[["call"]][["topF"]]
    #
    # if((!is.null(topF) && topF) || (is.null(topF) && dim(design)[2]>2 && is.null(coef))){
    #   #Extracting a table of the top-ranked genes from a linear
    #   #model fit on the basis of moderated F-statistics
    #   limmaT = obj$limmaTopF#equivalent to anova over many groups
    # } else {
    #   #Extracting a table of the top-ranked genes from the linear model fit
    #   limmaT  = obj$limmaTop
    # }

    #Extracting a table of the top-ranked genes from the linear model fit
    limmaT  = obj$limmaTop

    #get features
    feat = rownames(limmaT)

    #Select p-val
    limma.sel = limmaT[,pval];

    if(!is.null(feat)){
      names(limma.sel) = feat
    }


  } else {
    limma.sel = NULL;
  }

  if(!missing(feat.order) & !is.null(feat.order)){
    limma.sel = limma.sel[feat.order]
  }

  return(limma.sel)
}
