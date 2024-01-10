#' @include classes_generics.R class_Logger.R class_Screened.R
NULL

#' Screener Class
#' An S4 class providing the methods to perform a features screening.
#'
#' The object consists of 3 slots
#' @slot id a name to identify the screener
#' @slot screener screener function
#' @slot parameters list containing the parameters for the chosen screening method
#' @slot logger a \linkS4class{Logger}
#' @author Alessandro Barberis
methods::setClass(
  Class = "Screener",
  slots = c(
    id         = "character",
    screener   = "function",
    parameters = "list",
    logger     = "Logger"
  )
)

#'Screener Constructor
#'
#'Constructor for the S4 Screener object.
#'
#'Constructor for the S4 \linkS4class{Screener} object.
#'
#'@param id a name to identify the screener.
#' If screening method is one of the supported by renoir, the constructor will
#' automatically select a \code{screener}. See \code{supported_screening_methods()}
#' for the supported methods.
#' @param screener (optional) function to screen the data for features selection.
#' Used if \code{id} is not one of the supported by renoir.
#' If \code{screener} is provided it must conform to the renoir common interface,
#' and must have the following formal arguments:
#' \describe{
#'    \item{x}{the input matrix, where rows are observations and columns are variables.}
#'    \item{y}{the response variable. Its number of rows must match the number of rows of \code{x}.}
#'    \item{weights}{priors of the observations}
#'    \item{resp.type}{the response type}
#'    \item{observations}{indices of observations to keep}
#'    \item{...}{additional arguments}
#' }
#' The output must be a \linkS4class{Screened} object, where the slot \code{score} stores the
#' feature scores with the same order as in \code{x}
#' @param parameters list containing the parameters to fix for the chosen screening method
#' @param logger a \linkS4class{Logger}
#'
#' @export
#'
#' @author Alessandro Barberis
#'@rdname Screener-class
Screener <- function(
  id,
  screener,
  parameters,
  logger = Logger(verbose = F)
){

  if(missing(screener) && (id %in% supported_screening_methods())){
    screener = get_screener_function(id)
  } else if(!missing(screener)){
    if(id %in% supported_screening_methods()){
      stop(paste("'id' not valid, please change name. It must be different from the built-in renoir method names:\n",paste(supported_screening_methods(), collapse = ", "),"\n"))
    }
  } else {
    stop("Please provide a valid 'id' and/or 'screener'.\n")
  }

  #Check provided prediction function
  check_provided_screener_function(screener)

  #Check parameters
  if(missing(parameters) || is.null(parameters) || length(parameters)==0){
    parameters = list()
  }

  methods::new(
    Class = "Screener",
    screener    = screener,
    id         = id,
    parameters = parameters,
    logger     = logger
  )
}


methods::setMethod(f = "get_id",         signature = "Screener", definition = function(object){methods::slot(object = object, name = 'id')})
methods::setMethod(f = "get_screener",   signature = "Screener", definition = function(object){methods::slot(object = object, name = 'screener')})
methods::setMethod(f = "get_parameters", signature = "Screener", definition = function(object){methods::slot(object = object, name = 'parameters')})
methods::setMethod(f = "get_logger",     signature = "Screener", definition = function(object){methods::slot(object = object, name = 'logger')})



supported_screening_methods <- function(){
  out = c("ebayes", "permutation", "default")
  return(out)
}

get_screener_function <-function(id) {
  out = switch(
    id,
    # default     = screening_by_default,
    # ebayes      = screening_by_ebayes,
    # permutation = screening_by_permutation
    default     = default_screener,
    ebayes      = ebayes_screener,
    permutation = permutation_screener
  )
  return(out)
}

check_provided_screener_function <- function(screener){

  if(missing(screener)){
    stop("'screener' is missing with no default.\n")
  } else {
    #needed formals
    # formals.def = c("x", "y", "weights", "offset", "resp.type", "observations", "features")
    formals.def = c("x", "y", "weights", "resp.type", "observations")

    #get formals
    formals.fun = names(formals(fun = screener))

    #check
    # if(any(!(formals.fun %in% formals.def))){
    if(any(!(formals.def %in% formals.fun))){

      stop(paste0("Provided screener function without required formal arguments.Function interface must match renoir requirements. Try with:\n
      function(", paste0(formals.def, collapse = ", "),"...){
        #Use provided data to screen the features
        # out = YOUR CODE

        #Set a Screened object as output
        # out = Screened(method = SCREENER_ID, n = N_SELECTED_FEATURES, index = INDEX_SELECTED_FEATURES, score = SCORE_USED_FOR_SORTING_FEATURES)

        #Return screened
        return(out)
      }
      \n"))
    }
  }
}



#'Compute the significance of the filtering test
#'
#'@description This function compute the significance of the screening test
#'
#'@param x the input matrix, where rows are observations and columns are variables.
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}.
#'@param weights priors of the observations
#'@param resp.type the response type
#'@param observations (optional) indices of observations to keep
#'@param method which correlation method to use. Currently, only "pearson" is supported.
#'@param alternative alternative hypotesis to use. Must be one of \code{"two.sided"} (default),
#'\code{"greater"} or \code{"less"}.
#'@param conf.level confidence levels used for the confidence intervals (where computed). A single number or a
#'numeric vector with value for each observation. All values must be in the range of [0;1].
#'@param adjust.method method used to adjust the p-values for multiple testing. Options, in
#'                     increasing conservatism, include "none", "BH", "BY" and "holm"
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{max}}{the max value of scores across multiple outputs is selected to get a single value for each observation}
#'   \item{\code{average}}{scores of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{scores of multiple outputs are summed up to get a single value for each observation}
#'   \item{\code{raw}}{returns the scores for the multiple outputs}
#'}
#'@param coef (optional) an integer indicating the response variable to consider in multi-response data
#'when \code{multi = "raw"}
#'@param ... further arguments
#'@param logger a \linkS4class{Logger}
#@return the p-value of the selected test
#'@return a \linkS4class{Screened} object
#'@author Alessandro Barberis
#'@export
default_screener <- function(
  x, y, weights = NULL, alternative="two.sided", method = "pearson", conf.level=0.95,
  resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
  observations = NULL,
  # features,
  coef = NULL,
  adjust.method="none",
  logger,
  multi = c("max", "raw", "average", "sum"),
  # multi = c("raw", "combine"),
  ...
){
  #--------------------------------------------------------------------------------------------#
  multi = match.arg(multi)

  #--------------------------------------------------------------------------------------------#
  #Subset observations
  x       = subset_observations(object = x, which = observations)
  y       = subset_observations(object = y, which = observations)
  # offset  = subset_observations(object = offset, which = observations)
  weights = subset_observations(object = weights, which = observations)

  #--------------------------------------------------------------------------------------------#
  #transpose
  x = t(x)

  #--------------------------------------------------------------------------------------------#
  #Set feature names
  rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");

  #--------------------------------------------------------------------------------------------#
  screened = switch(resp.type,
                    gaussian    = screening_default_gaussian(x=x, y=y, alternative=alternative, method=method, conf.level = conf.level),
                    mgaussian   = screening_default_mgaussian(x=x, y=y, alternative=alternative, method=method, conf.level = conf.level),
                    binomial    = screening_default_binomial(   x=x, y=y, alternative=alternative, conf.level = conf.level, ...),
                    multinomial = screening_default_multinomial(x=x, y=y, ...),
                    poisson     = screening_default_gaussian(x=x, y=y, alternative=alternative, method=method, conf.level = conf.level),
                    cox         = screening_default_cox(        x=x, y=y, weights=weights, ...))


  #--------------------------------------------------------------------------------------------#
  #Check if multi response
  screened = multiresponse_screener(screened = screened, multi = multi, coef = coef)

  #--------------------------------------------------------------------------------------------#
  #Correct for multiple testing
  out = stats::p.adjust(p = screened, method = adjust.method)

  #--------------------------------------------------------------------------------------------#
  #Create output object
  out = Screened(
    # method = "default",
    method = get_id_default_screener_test(resp.type = resp.type, y = y),
    n      = length(out),
    index  = seq(length(out)),
    score  = out
  )

  #--------------------------------------------------------------------------------------------#
  return(out);
}


get_id_default_screener_test <- function(resp.type, y){

  if(isTRUE(resp.type %in% c("binomial", "multinomial"))){
    #Get minimum number of observations per class
    min.obs = if(!is.null(dim(y))){
      min(colSums(y))
    } else {
      min(table(as.factor(y)))
    }

    #out
    out = switch(
      resp.type,
      'binomial'    = if(min.obs>10){'wtest'}else{'ttest'},
      'multinomial' = if(min.obs>10){'kw'}else{'anova'},
    )

  } else {
    out = switch(
      resp.type,
      'gaussian'  = 'pcorr',
      'mgaussian' = 'pcorr',
      'poisson'   = 'pcorr',
      'cox'       = 'logtest'
    )
  }

  #return
  return(out)

}

get_name_screener_by_id <- function(id, short = F){

  if(isTRUE(short)){
    out = switch(
      id,
      'ebayes'      = 'eBayes t-test',
      'permutation' = 'permutation',
      'wtest'       = 'Wilcoxon',
      'ttest'       = "Welch",
      'kw'          = 'Kruskal-Wallis',
      'anova'       = 'Anova',
      'pcorr'       = "Pearson",
      'logtest'     = 'likelihood-ratio'
    )
  } else {
    out = switch(
      id,
      'ebayes'      = 'empirical Bayes moderated t-statistics test',
      'permutation' = 'permutation test',
      'wtest'       = 'Wilcoxon signed-rank test',
      'ttest'       = "Welch's t-test",
      'kw'          = 'Kruskal-Wallis rank sum test',
      'anova'       = 'one-way analysis of variance test',
      'pcorr'       = "Pearson's correlation test",
      'logtest'     = 'likelihood-ratio test'
    )
  }


  return(out)
}

#'Compute the significance of the filtering test
#'
#'@description This function compute the significance of the screening test by using an Empirical Bayes method.
#'
#'@param x the input matrix, where rows are observations and columns are variables.
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}.
#'@param weights priors of the observations
#'@param resp.type the response type
#'@param observations (optional) indices of observations to keep
#'@param logged2 whether data was logged 2.
#'@return The p-value associated with each feature or NULL if the response type is not supported
#'@param adjust.method method used to adjust the p-values for multiple testing. Options, in
#'                     increasing conservatism, include "none", "BH", "BY" and "holm"
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{max}}{the max value of scores across multiple outputs is selected to get a single value for each observation}
#'   \item{\code{average}}{scores of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{scores of multiple outputs are summed up to get a single value for each observation}
#'   \item{\code{raw}}{returns the scores for the multiple outputs}
#'}
#'@param coef (optional) an integer indicating the response variable to consider in multi-response data
#'when \code{multi = "raw"}
#'@param ... further arguments
#'@param logger a \linkS4class{Logger}
#'@inheritParams limma::lmFit
#'@inheritParams limma::eBayes
#'@inheritParams limma::contrasts.fit
#'@inheritParams limma::topTable
#'@inheritParams limma
#@return the p-value of the selected test
#'
#'@return a \linkS4class{Screened} object
#'
#'@seealso
#'\code{\link{permutation_screener}},
#'\code{\link{default_screener}}
#'
#'@author Alessandro Barberis
#'
#'@export
ebayes_screener <- function(
  #common formals
  x, y, weights = NULL,
  resp.type,
  observations = NULL,
  # features,

  #further arguments
  #setup
  logged2 = TRUE,

  #lmFit
  method = "ls",
  design = NULL,

  #contrasts.fit
  contrasts = NULL,

  #eBayes
  assay.type = c("array","seq"),
  proportion = 0.01,

  #topTable, decideTests,topTableF
  coef = NULL,
  number = Inf,
  adjust.method="BH",
  # p.value=0.05,
  p.value=1,
  top.table.sort.by = "none",

  #output
  return.type = c("pvalue", "object"),

  # multi = c("raw", "max", "average", "sum"),
  multi = c("combine", "max", "average", "sum", "raw"),

  #Logger
  logger = Logger(verbose = F),

  ...
){

  #--------------------------------------------------------------------------------------------#
  logger = open_con(logger)

  #--------------------------------------------------------------------------------------------#
  #Subset observations
  if(!is.null(observations)){
    log_trace(object = logger, message = "Subset observations", sep = "\n", add.level = TRUE, add.time = TRUE)
    x       = subset_observations(object = x, which = observations)
    y       = subset_observations(object = y, which = observations)
    # offset  = subset_observations(object = offset, which = observations)
    weights = subset_observations(object = weights, which = observations)
    design  = subset_observations(object = design, which = observations)
  }

  #--------------------------------------------------------------------------------------------#
  #transpose
  log_trace(object = logger, message = "Transpose predictors matrix", sep = "\n", add.level = TRUE, add.time = TRUE)
  x = t(x)

  #--------------------------------------------------------------------------------------------#
  #match
  multi = match.arg(multi)
  return.type = match.arg(return.type)
  assay.type = match.arg(assay.type)

  #--------------------------------------------------------------------------------------------#
  args <- c(list(...));

  #--------------------------------------------------------------------------------------------#
  #LIMMA DOESN'T SUPPORT THESE TYPES
  if(identical(resp.type, 'poisson') || identical(resp.type, 'cox')){
    return(NULL)
  }

  #--------------------------------------------------------------------------------------------#

  rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");

  #--------------------------------------------------------------------------------------------#
  #Limma expects log2 data, so log the data if not already log2
  if(!logged2){
    log_trace(object = logger, message = "Apply logarithmic transformation to data.", sep = "\n", add.level = TRUE, add.time = TRUE)
    x = log2(x);
  }

  #--------------------------------------------------------------------------------------------#
  if(!is.null(design)){
    log_trace(object = logger, message = "Check provided design matrix.", sep = "\n", add.level = TRUE, add.time = TRUE)
    #check in common
    inter = intersect(x = colnames(x), y = rownames(design))
    #update
    if(length(inter)!=ncol(x)){
      warning("There is a problem with the provided design matrix. 'y' will be used as design matrix.\n")
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

        #check colnames
        cnames = make.names(names = substring(text = colnames(design), first = 2), unique = T, allow_ = T)

        #clean colnames (removes the 'y' from names)
        colnames(design) = cnames

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

  #--------------------------------------------------------------------------------------------#

  #Set the contrast matrix
  # if(identical(x = resp.type, y = "binomial") || identical(x = resp.type, y = "multinomial")){
  #   if(is.null(contrasts)){
  #     contrasts = utils::combn(colnames(design), m=2, FUN=paste, collapse="-");
  #     contrasts <- limma::makeContrasts(contrasts = contrasts, levels=design);
  #   }
  # } else {
  #   contrasts = NULL;
  # }

  #--------------------------------------------------------------------------------------------#

  #Design matrix should be full rank
  if(limma::is.fullrank(design)){

    #--------------------------------------------------------------------------------------------#
    #Fit the model (x has rows corresponding to genes and columns to samples)
    log_debug(object = logger, message = "Fitting a linear model for each gene given a series of arrays...", sep = "", add.level = TRUE, add.time = TRUE)
    limmaFit <- limma::lmFit(object = x, design = design, method = method, weights = weights)
    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#
    if(!is.null(contrasts)){
      #Compute the coefficients and the standard errors for the given set of contrasts
      log_debug(object = logger, message = "Computing estimated coefficients and standard errors for a given set of contrasts...", sep = "", add.level = TRUE, add.time = TRUE)
      limmaFit <- limma::contrasts.fit(fit = limmaFit, contrasts = contrasts)
      log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    }

    #--------------------------------------------------------------------------------------------#
    #Compute statistics on the model by empirical Bayes moderation of the standard errors towards a common value
    log_debug(object = logger, message = "Computing moderated t-/F-statistic, and log-odds of differential expression by empirical Bayes moderation of the standard errors towards a common value...", sep = "", add.level = TRUE, add.time = TRUE)
    limmaEBayes <- switch(assay.type,
                          array = limma::eBayes(fit = limmaFit, proportion=proportion, trend = FALSE),
                          seq   = limma::eBayes(fit = limmaFit, proportion=proportion, trend = TRUE))
    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#

    # #The outcome of each hypothesis test can be assigned using decideTests function.
    # #decideTests identifies which genes are significantly differentially expressed for each contrast
    # #from a fit object containing p-values and test statistics.
    # #
    # #The output of this function is essentially a numeric matrix with elements -1, 0 or 1
    # #depending on whether each t-statistic is classified as significantly negative, not significant
    # #or significantly positive.
    # log_debug(object = logger, message = "Identifying which genes are significantly differentially expressed for each contrast...", sep = "", add.level = TRUE, add.time = TRUE)
    # limmaTests <- limma::decideTests(limmaEBayes, adjust.method=adjust.method, p.value=p.value)
    # log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#
    if(identical(return.type, "pvalue")){

      #p-value
      pval = limmaEBayes$p.value

      #check if has multiple coefficients/contrasts
      nc = ncol(pval)

      is.multi = !is.null(nc) && nc>1

      if(!is.multi){
        #correct for multiple testing
        out = stats::setNames(object = stats::p.adjust(p = pval, method = adjust.method), nm = rownames(pval))
      } else {
        if(identical(multi, "combine")){
          #--------------------------------------------------------------------------------------------#
          #Get a list of top-ranked genes differential expressed from a previous linear model fit.
          #We adjust the p-value for multiple testing.
          log_debug(object = logger, message = "Extracting a table of the top-ranked genes from the linear model fit...", sep = "", add.level = TRUE, add.time = TRUE)
          limmaTop = limma::topTable(fit = limmaEBayes, coef=coef, number = number, adjust.method=adjust.method, p.value = p.value, sort.by=top.table.sort.by)
          log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

          #--------------------------------------------------------------------------------------------#
          out = stats::setNames(object = limmaTop[, 'adj.P.Val'], nm = rownames(limmaTop))

        } else {
          out = multiresponse_screener(screened = pval, multi = multi, coef = coef)
          #correct for multiple testing
          out = if(!is.null(ncol(pval))){apply(X = pval, MARGIN = 2, FUN = stats::p.adjust, method = adjust.method)}else{stats::p.adjust(p = pval, method = adjust.method)}
        }
      }

      if(!is.null(names(out))){
        #order
        out = out[rownames(x)]
      } else {
        stop("Can't check if results are sorted as required.\n")
      }

      #create output
      out = Screened(
        method = "ebayes",
        n      = length(out),
        index  = seq(length(out)),
        score  = out
      )


    } else {
      out = list(
        limmaFit    = limmaFit,
        limmaEBayes = limmaEBayes
        # limmaTop    = limmaTop
      )
    }

  } else {
    log_trace(object = logger, message = "Design matrix is not full rank: screening skipped.", sep = "\n", add.level = TRUE, add.time = TRUE)
    # out = NULL;
    out = Screened(
      method = "ebayes"
      # n      = length(out),
      # index  = seq(length(out)),
      # score  = out
    )
  }

  #--------------------------------------------------------------------------------------------#
  #close connection
  close_con(object = logger)

  #--------------------------------------------------------------------------------------------#
  return(out);
}

#'Compute the significance of the filtering test
#'@description This function compute the significance of the screening test by using
#'a permutation approach (significance analysis of microarray). The score stored
#'in the reported \linkS4class{Screened} object is the computed q-value.
#'@param x the input matrix, where rows are observations and columns are variables.
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}.
#'@param weights priors of the observations
#'@param resp.type the response type
#'@param observations (optional) indices of observations to keep
#'@param sam.resp.type Problem type:\cr
#'\describe{
#'\item{\code{"Quantitative"}}{for a continuous parameter (Available for both array and sequencing data)}
#'\item{\code{"Two class unpaired"}}{for both array and sequencing data}
#'\item{\code{"Survival"}}{for censored survival outcome (for both array and sequencing data)}
#'\item{\code{"Multiclass"}}{more than 2 groups (for both array and sequencing data)}
#'\item{\code{"One class"}}{for a single group (only for array data)}
#'\item{\code{"Two class paired"}}{for two classes with paired observations (for both array and sequencing data)}
#'\item{\code{"Two class unpaired timecourse"}}{only for array data}
#'\item{\code{"One class time course"}}{only for array data}
#'\item{\code{"Two class.paired timecourse"}}{only for array data}
#'\item{\code{"Pattern discovery"}}{only for array data}
#'}
#'@inheritParams samr::SAMseq
#'@inheritParams samr::SAM
#@param adjust.method method used to adjust the p-values for multiple testing. Options, in
#                     increasing conservatism, include "none", "BH", "BY" and "holm"
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{max}}{the max value of scores across multiple outputs is selected to get a single value for each observation}
#'   \item{\code{average}}{scores of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{scores of multiple outputs are summed up to get a single value for each observation}
#'   \item{\code{raw}}{returns the scores for the multiple outputs}
#'}
#'@param coef (optional) an integer indicating the response variable to consider in multi-response data
#'when \code{multi = "raw"}
#'@param return.type debug argument, if \code{object} the output from samr is reported
#'@param ... further arguments
#'@param logger a \linkS4class{Logger}
#@return the p-value of the selected test
#'@return a \linkS4class{Screened} object
#'@author Alessandro Barberis
#'@export
permutation_screener <- function(
  x, y, weights = NULL,
  resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
  observations = NULL,
  # features,
  coef = NULL,

  sam.resp.type = c("Quantitative","Two class unpaired",
              "Survival","Multiclass", "One class", "Two class paired",
              "Two class unpaired timecourse", "One class timecourse",
              "Two class paired timecourse", "Pattern discovery"),
  geneid=NULL, genenames = NULL,
  assay.type=c("array","seq"), s0=0.0001, s0.perc=NULL, nperms=100,
  center.arrays=FALSE, testStatistic=c("standard","wilcoxon"),
  time.summary.type=c("slope","signed.area"),
  regression.method=c("standard","ranks"), return.x=FALSE,
  knn.neighbors=10, random.seed=NULL,
  logged2 = TRUE,

  fdr.output = 1,
  eigengene.number = 1,
  nresamp = 20,

  all.genes  = TRUE,

  return.type = c("pvalue", "object"),
  logger = Logger(verbose = F),
  multi = c("max", "average", "sum", "raw")

  ){
  #--------------------------------------------------------------------------------------------#
  logger = open_con(logger)

  #--------------------------------------------------------------------------------------------#
  #Subset observations
  x       = subset_observations(object = x, which = observations)
  y       = subset_observations(object = y, which = observations)
  # offset  = subset_observations(object = offset, which = observations)
  weights = subset_observations(object = weights, which = observations)

  #--------------------------------------------------------------------------------------------#
  #transpose
  x = t(x)

  #--------------------------------------------------------------------------------------------#
  #match
  return.type = match.arg(return.type)

  assay.type        = match.arg(assay.type);
  testStatistic     = match.arg(testStatistic);
  time.summary.type = match.arg(time.summary.type);
  regression.method = match.arg(regression.method);

  multi             = match.arg(multi);

  #--------------------------------------------------------------------------------------------#
  if(!missing(x)){
    rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");
  }

  #--------------------------------------------------------------------------------------------#
  #Feature order
  feat.order = rownames(x);

  #--------------------------------------------------------------------------------------------#
  if(missing(sam.resp.type)){
    sam.resp.type = switch(resp.type,
                           gaussian    = "Quantitative",
                           mgaussian   = "Quantitative",
                           binomial    = "Two class unpaired",
                           multinomial = "Multiclass",
                           poisson     = "Quantitative",
                           cox         = "Survival"
    )
  }

  #--------------------------------------------------------------------------------------------#
  #Set the response variable if resp.type = cox
  if(identical(x = resp.type, y = "cox")){
    if(identical(colnames(y), c("time", "status")) || identical(colnames(y), c("status", "time"))){
      censoring.status  = y[,"status"];
      y = y[,"time"];
    } else {
      censoring.status  = y[,2];
      y = y[,1];
    }

  } else {
    censoring.status  = NULL;
  }

  #--------------------------------------------------------------------------------------------#

  if(identical(x = resp.type, y = "binomial") || identical(x = resp.type, y = "multinomial")){
    #If y is matrix, convert to factor
    if(is.matrix(y)){
      #1) Convert matrix to factor
      y = dummy.matrix.to.factor(y);

    }

    #2) Convert factor to vector of integer
    y = as.vector(x = y, mode = "integer");

    if(is.element(el = 0, set = y)){
      #Sam wants integer >0 for 'Two Class (unpaired)' and 'Multiclass'
      y = y + 1;
    }
  }

  #--------------------------------------------------------------------------------------------#

  #Update genenames
  genenames = rownames(x);


  sam_fun = function(x, y, censoring.status, resp.type, geneid, genenames,
                     s0, s0.perc, nperms, center.arrays, testStatistic, time.summary.type, regression.method,
                     fdr.output, random.seed, logged2, eigengene.number, all.genes, nresamp, assay.type){
    capture.output(
      sam.res <- switch(assay.type,
                     array = SAM.mod(x=x, y = y, censoring.status = censoring.status, resp.type = resp.type,
                                     geneid=geneid, genenames=genenames,
                                     s0=s0, s0.perc=s0.perc, nperms=nperms, center.arrays=center.arrays,
                                     testStatistic=testStatistic,
                                     time.summary.type=time.summary.type, regression.method = regression.method,
                                     fdr.output = fdr.output, random.seed = random.seed, logged2 = logged2,
                                     eigengene.number = eigengene.number,
                                     all.genes = all.genes),
                     seq = samr::SAMseq(x=x, y=y, censoring.status = censoring.status, resp.type = resp.type,
                                        geneid=geneid, genenames=genenames, nperms=nperms,
                                        random.seed = random.seed, nresamp = nresamp,
                                        fdr.output = fdr.output))
    )
    return(sam.res)
  }

  #--------------------------------------------------------------------------------------------#
  nobs = ncol(x);

  if(nperms>nobs){nperms = nobs}

  #--------------------------------------------------------------------------------------------#
  #Compute Significance analysis of microarrays

  #--------------------------------------------------------------#
  #Significance analysis of microarrays (SAM)
  #
  #Correlates a large number of features (eg genes) with an
  #outcome variable, such as a group indicator, quantitative
  #variable or survival time.
  #
  #samr detects differential expression for microarray and
  #sequencing data
  #--------------------------------------------------------------#

  #Using the interface function
  #Important arguments:
  # logged2:            Has the data been transformed by log (base 2)?
  # fdr.output:         (Approximate) False Discovery Rate cutoff for output in significant genes table
  # regression.method:  Regression method for quantitative case: "standard", (linear least squares)
  #                     or "ranks" (linear least squares on ranked data). Only used for array data

  #A bug is happening when resp.type="multiclass"
  # sam.res = samr::SAM(x=x, y = y, censoring.status = censoring.status, resp.type = resp.type,
  #                     geneid=geneid, genenames=genenames,
  #                     s0=s0, s0.perc=s0.perc, nperms=nperms, center.arrays=center.arrays, testStatistic=testStatistic,
  #                     time.summary.type=time.summary.type, regression.method = regression.method,
  #                     fdr.output = fdr.output, logged2 = logged2, eigengene.number = eigengene.number)




  log_debug(object = logger, message = "Computing the significance analysis of microarrays...", sep = "", add.level = TRUE, add.time = TRUE)
  if(identical(resp.type, "mgaussian")){
    out = apply(X = y, MARGIN=2, FUN = sam_fun, x=x, censoring.status = censoring.status, resp.type = sam.resp.type,
                geneid=geneid, genenames=genenames,
                s0=s0, s0.perc=s0.perc, nperms=nperms, center.arrays=center.arrays,
                testStatistic=testStatistic,
                time.summary.type=time.summary.type, regression.method = regression.method,
                fdr.output = fdr.output, random.seed = random.seed, logged2 = logged2,
                eigengene.number = eigengene.number,
                all.genes = all.genes, nresamp = nresamp, assay.type = assay.type);

  } else {
    out = sam_fun(
      x=x, y = y, censoring.status = censoring.status, resp.type = sam.resp.type,
      geneid=geneid, genenames=genenames,
      s0=s0, s0.perc=s0.perc, nperms=nperms, center.arrays=center.arrays,
      testStatistic=testStatistic,
      time.summary.type=time.summary.type, regression.method = regression.method,
      fdr.output = fdr.output, random.seed = random.seed, logged2 = logged2,
      eigengene.number = eigengene.number,
      all.genes = all.genes, nresamp = nresamp, assay.type = assay.type)
  }


  log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------------------------------------#
  #Extract significance
  if(identical(return.type, "pvalue")){
    screened = extract_qval_sam(obj = list(sam.res=out),
                                feat.order = feat.order,
                                resp.type = resp.type,
                                coef = NULL)

    #--------------------------------------------------------------------------------------------#
    #Check if multi response
    out = multiresponse_screener(screened = screened, multi = multi, coef = coef)

    #--------------------------------------------------------------------------------------------#
    #create output
    out = Screened(
      method = "permutation",
      n      = length(out),
      index  = seq(length(out)),
      score  = out
    )

  }

  #--------------------------------------------------------------------------------------------#
  #close connection
  close_con(object = logger)

  #--------------------------------------------------------------------------------------------#
  return(out);
}

#'Multi-response screen
#'
#'@description If screened features are from multi-response data,
#'this function allows to adopt different strategies.
#'
#'@param screened vector or matrix containing the significance for the screened features
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{max}}{for each feature, the maximal significance value across responses is kept}
#'   \item{\code{average}}{significances of multiple outputs are averaged to get a single value for each feature}
#'   \item{\code{sum}}{significances of multiple outputs are summed up to get a single value for each feature}
#'   \item{\code{raw}}{for each feature, returns one score for each output}
#'}
#'@param coef (optional) integer value indicating the response to keep
#'
#'@return The significance of the screening test for each feature after the
#'application of the selected strategy.
#'
#'@export
#'
#'@author Alessandro Barberis
multiresponse_screener <- function(screened, multi = c("max", "average", "sum", "raw"), coef = NULL){

  #n output
  n = ncol(screened)

  #check if multi-response
  is.multi = !is.null(n)

  if(is.multi){

    if(n > 1){
      #multi
      multi = match.arg(multi)

      if(identical(multi, "raw")){
        if(!is.null(coef) && coef > 0 && coef <= n){
          #select coef
          screened = screened[,coef]
        }
      }else{
        if(identical(multi, "max")){
          #get the least significant value (i.e. max p-value) across coefficients
          screened = apply(X = screened, MARGIN = 1, FUN = max)
        } else if(identical(multi, "sum")){
          #sum
          screened = rowSums(x = screened, na.rm = T)
        } else if(identical(multi, "average")){
          #average
          screened = rowMeans(x = screened, na.rm = T)
          # screened = apply(X = screened, MARGIN = 1, FUN = stats::weighted.mean, w = weights, na.rm = TRUE)
        }
      }
    } else {
      #make sure output is vector
      screened = stats::setNames(object = as.vector(screened), nm = rownames(screened))
    }

  }

  return(screened)
}



#'Features screening
#'@description This function performs a feature screening, and return a \linkS4class{Screened} object
#'@param screener a \linkS4class{Screener} object
#'@param x the input matrix, where rows are observations and columns are variables.
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}.
#'@param weights priors of the observations
#'@param resp.type the response type
#'@param observations (optional) indices of observations to keep
#'@param multi what to do when response has multiple output values
#'\describe{
#'   \item{\code{max}}{the max value of scores across multiple outputs is selected to get a single value for each observation}
#'   \item{\code{average}}{scores of multiple outputs are averaged to get a single value for each observation}
#'   \item{\code{sum}}{scores of multiple outputs are summed up to get a single value for each observation}
#'}
#'@param coef (optional) an integer indicating the response variable to consider in multi-response data
#'when \code{multi = "raw"}
#'@param order whether to order the results by the score values
#'\describe{
#'   \item{\code{increasing}}{results are sorted in increasing order}
#'   \item{\code{decreasing}}{results are sorted in decreasing order}
#'   \item{\code{none}}{results are not sorted}
#'}
#'@param cutoff (optional) numeric, the value to use as a threshold for filtering not significant variables
#'@param maxvars integer, the maximum number of variables to keep. If the filtering is returning an higher number of variables,
#' only the most significant \code{maxvars} variables are kept.
#'@param logger a \linkS4class{Logger}
#'@param ... further arguments to \code{screener} function
#'@return a \linkS4class{Screened} object
#'@author Alessandro Barberis
#'@rdname screen
methods::setMethod(
  f = "screen",
  signature = methods::signature(screener ="Screener"),
  definition = function(
    screener,
    x,
    y,
    weights = NULL,
    # offset,
    resp.type,
    observations = NULL,
    # features,
    multi = c("max", "average", "sum"),

    #further args
    order = c("increasing", "decreasing", "none"),
    cutoff = NULL,
    maxvars = NULL,

    logger,

    ...){

    #--------------------------------------------------------------------------------------------#
    #match
    multi = match.arg(multi)
    order = match.arg(order)

    #--------------------------------------------------------------------------------------------#
    #Logger
    if(missing(logger)){logger = get_logger(screener)}
    logger = open_con(logger)

    #--------------------------------------------------------------------------------------------#
    if(length(maxvars)>1){
      warning("Multiple values passed as 'maxvars'. Only maximum value is kept.\n")
      maxvars = max(maxvars, na.rm = TRUE)
    }

    #--------------------------------------------------------------------------------------------#
    #get parameters
    parameters = get_parameters(screener)

    #--------------------------------------------------------------------------------------------#
    #get function
    screener_fun = get_screener(screener)

    #--------------------------------------------------------------------------------------------#

    #set arguments
    args = c(parameters, list(...))
    # clean
    rm(parameters)

    log_trace(object = logger, message = "Screening", sep = "\n", add.level = TRUE, add.time = TRUE)
    tryCatch(expr = {
      screened = do.call(
        what = screener_fun,
        args = c(
          list(
            x       = x,
            y       = y,
            weights = weights,
            # offset  = offset,
            resp.type    = resp.type,
            observations = observations
            # features     = features
          ),
          args
        )
      )

    },
    error = function(cnd){
      #Get the current process id
      pid = Sys.getpid();
      #Set a process index
      pidi = 1;
      #Setup a dump dir path
      dp = file.path(getwd(), "dump", paste(pid, pidi, sep="_"));
      while(dir.exists(dp)){
        pidi = pidi + 1;
        dp = file.path(getwd(), "dump", paste(pid, pidi, sep="_"));
      }
      dir.create(path = dp, recursive = T);
      #Setup a file name
      # paste(Sys.info()[['nodename']], Sys.getpid(), sep='-')
      dfn = paste0("screen_last_dump_",pidi,".rda")
      #Setup a file path
      dfp = file.path(dp, dfn);
      #args
      args = c(as.list(environment()), list(...))
      #Store the call
      save(args, file = dfp)
      #Return a meaningful message
      message("An error occurred during screening. A dump of data was saved ", dfp, appendLF = T)
      #Stop execution and raise the error
      stop(cnd);
    })



    #--------------------------------------------------------------------------------------------#
    #Get data
    log_trace(object = logger, message = "Combine results", sep = "\n", add.level = TRUE, add.time = TRUE)
    # test = do.call(what = cbind, args = out)
    score = get_score(screened)

    #--------------------------------------------------------------------------------------------#
    #Select the least significant among the values
    log_trace(object = logger, message = paste("Select the", multi, "values..."), sep = "", add.level = TRUE, add.time = TRUE)
    score = multiresponse_screener(screened = score, multi = multi)
    log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

    #--------------------------------------------------------------------------------------------#
    #order by input features
    score = score[get_index(screened)]

    #--------------------------------------------------------------------------------------------#
    #Get an ordered list of elements (increasing p-value)
    if(identical(order, "increasing")){
      log_trace(object = logger, message = "Order by increasing significance...", sep = "", add.level = TRUE, add.time = TRUE)
      ordered = order(score, decreasing = FALSE, na.last = TRUE);
      log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    } else if(identical(order, "decreasing")){
      log_trace(object = logger, message = "Order by decreasing significance...", sep = "", add.level = TRUE, add.time = TRUE)
      ordered = order(score, decreasing = T, na.last = TRUE);
      log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    } else {
      ordered = seq(length(score))
    }

    #--------------------------------------------------------------------------------------------#
    #If threshold was provided, get the index of the elements
    #whose significance is < than threshold (ordered by significance)
    if(!is.null(cutoff) && !S4Vectors::isEmpty(cutoff)){
      log_trace(object = logger, message = "Select most significant...", sep = "", add.level = TRUE, add.time = TRUE)
      index = which(score < cutoff);
      #intersect
      ordered = intersect(x = ordered, y = index);
      log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    }


    #--------------------------------------------------------------------------------------------#
    #If the number of significant elements is greater than maxvars, limit the results
    if(!S4Vectors::isEmpty(maxvars) && length(ordered)>maxvars){
      #Select first n elements
      log_trace(object = logger, message = paste("Select top", maxvars, "elements..."), sep = "", add.level = TRUE, add.time = TRUE)
      ordered = ordered[1:maxvars]
      log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
    }


    #--------------------------------------------------------------------------------------------#
    #close
    close_con(logger)

    #--------------------------------------------------------------------------------------------#
    #set output object
    out = Screened(
      method = get_id(screener),
      n      = length(ordered),
      index  = ordered,
      score  = score
    )

    #--------------------------------------------------------------------------------------------#
    return(out)
  }
)














#'Compute the significance of the filtering test
#'
#'@description This function compute the significance of the screening test by using
#'\code{\link[renoir]{compute_sam}}
#'
#'@param x the input matrix, where rows are observations and columns are variables.
#'@param y the response variable. Its number of rows must match the number of rows of \code{x}.
#'@param weights priors of the observations
#'@param resp.type the response type
#'@param observations (optional) indices of observations to keep
#'
#'@return The p-value associated with each feature
#'@keywords internal
screening_by_permutation <- function(
  x, y, weights = NULL,
  resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
  observations = NULL,
  features,
  coef = NULL,
  return.type = c("pvalue", "object"),
  logger = Logger(verbose = F),
  multi = c("raw", "max", "average", "sum"),
  ...){

  #--------------------------------------------------------------------------------------------#
  #Subset observations
  x       = subset_observations(object = x, which = observations)
  y       = subset_observations(object = y, which = observations)
  # offset  = subset_observations(object = offset, which = observations)
  weights = subset_observations(object = weights, which = observations)

  #--------------------------------------------------------------------------------------------#
  #transpose
  x = t(x)

  #--------------------------------------------------------------------------------------------#
  #match
  multi = match.arg(multi)
  return.type = match.arg(return.type)
  # args <- c(as.list(environment()), list(...));
  # args[c("resp.type")] = NULL;

  # args <- c(list(...));

  #--------------------------------------------------------------------------------------------#

  #Check if has coef
  if(!missing(coef) && !is.null(coef)){hasCoef=TRUE}else{hasCoef=FALSE}

  #--------------------------------------------------------------------------------------------#

  if(!missing(x)){
    rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");
  }

  #--------------------------------------------------------------------------------------------#
  #b) Update genenames
  feat.order = rownames(x);

  #--------------------------------------------------------------------------------------------#
  #Compute Significance analysis of microarrays
  screened = compute_sam(
    x = x,
    y = y,
    resp.type = resp.type,
    coef = coef,
    logger = logger,
    ...)

  #--------------------------------------------------------------------------------------------#
  #Extract significance
  if(identical(return.type, "pvalue")){
    screened = extract_qval_sam(obj = screened,
                                feat.order = feat.order,
                                resp.type = resp.type,
                                coef = NULL)
    #--------------------------------------------------------------------------------------------#
    #Check if multi response
    screened = multiresponse_screener(screened = screened, multi = multi, coef = coef)

  }

  #--------------------------------------------------------------------------------------------#
  return(screened);
}


#'Compute the significance of the filtering test
#'@description This function compute the significance of the screening test by using
#'\code{\link[renoir]{compute_limma}}
#'
#'@inheritParams limma
#'@inheritParams compute_limma
#'@inheritParams multiresponse_screener
#'@param logged2 whether data was logged 2
#'
#'@return The p-value associated with each feature or NULL if the response type is not supported
#'
#'@keywords internal
screening_by_ebayes <- function(
  x, y, weights = NULL,
  resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
  observations = NULL,
  # features,
  logged2 = FALSE,
  coef = NULL,
  design = NULL,
  return.type = c("pvalue", "object"),
  logger = Logger(verbose = F),
  multi = c("raw", "max", "average", "sum"),
  ...){


  #--------------------------------------------------------------------------------------------#
  #Subset observations
  x       = subset_observations(object = x, which = observations)
  y       = subset_observations(object = y, which = observations)
  # offset  = subset_observations(object = offset, which = observations)
  weights = subset_observations(object = weights, which = observations)
  design = subset_observations(object = design, which = observations)

  #--------------------------------------------------------------------------------------------#
  #transpose
  x = t(x)

  #--------------------------------------------------------------------------------------------#
  #match
  multi = match.arg(multi)
  return.type = match.arg(return.type)
  args <- c(list(...));

  #---------------------------------------------------------------------------#
  #LIMMA DOESN'T SUPPORT THESE TYPES
  if(identical(resp.type, 'poisson') || identical(resp.type, 'cox')){
    return(NULL)
  }

  #---------------------------------------------------------------------------#

  rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");

  #---------------------------------------------------------------------------#
  #Run limma
  out = compute_limma(
    x = x,
    y = y,
    weights = weights,
    resp.type = resp.type,
    logged2 = logged2,
    coef = coef,
    logger = logger,
    design = design,
    ...)

  #---------------------------------------------------------------------------#
  if(identical(return.type, "pvalue")){
    #Extract p-value
    out = extract_pval_limma(
      obj = out,
      coef = coef,
      feat.order = rownames(x))

    #--------------------------------------------------------------------------------------------#
    #Check if multi response
    out = multiresponse_screener(screened = out, multi = multi, coef = coef)
  }

  #---------------------------------------------------------------------------#
  return(out);

}


#'Compute the significance of the filtering test
#'@description This function compute the significance of the screening test
#'@param x numeric matrix
#'@param y response type (must be a factor for binomial and multinomial)
#'@param method which correlation method to use. Currently, only "pearson" is supported.
#'@param alternative alternative hypotesis to use. Must be one of \code{"two.sided"} (default),
#'\code{"greater"} or \code{"less"}.
#'@param conf.level confidence levels used for the confidence intervals (where computed). A single number or a
#'numeric vector with value for each observation. All values must be in the range of [0;1].
#'@param ... further arguments
#'@return the p-value of the selected test
#'@keywords internal
screening_by_default <- function(
  x, y, weights = NULL, alternative="two.sided", method = "pearson", conf.level=0.95,
  resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
  observations = NULL,
  # features,
  coef = NULL,
  logger,
  multi = c("raw", "max", "average", "sum"),
  ...){

  #--------------------------------------------------------------------------------------------#
  multi = match.arg(multi)

  #--------------------------------------------------------------------------------------------#
  #Subset observations
  x       = subset_observations(object = x, which = observations)
  y       = subset_observations(object = y, which = observations)
  # offset  = subset_observations(object = offset, which = observations)
  weights = subset_observations(object = weights, which = observations)

  #--------------------------------------------------------------------------------------------#
  #transpose
  x = t(x)

  #--------------------------------------------------------------------------------------------#
  #Check if has coef
  if(!missing(coef) && !is.null(coef)){hasCoef=TRUE}else{hasCoef=FALSE}

  #--------------------------------------------------------------------------------------------#
  screened = switch(resp.type,
                gaussian    = screening_default_gaussian(x=x, y=y, alternative=alternative, method=method, conf.level = conf.level),
                mgaussian   = screening_default_mgaussian(x=x, y=y, alternative=alternative, method=method, conf.level = conf.level),
                binomial    = screening_default_binomial(   x=x, y=y, alternative=alternative, conf.level = conf.level, ...),
                multinomial = screening_default_multinomial(x=x, y=y, ...),
                poisson     = screening_default_gaussian(x=x, y=y, alternative=alternative, method=method, conf.level = conf.level),
                cox         = screening_default_cox(        x=x, y=y, weights=weights, ...))


  #--------------------------------------------------------------------------------------------#
  #Check if multi response
  screened = multiresponse_screener(screened = screened, multi = multi, coef = coef)

  #--------------------------------------------------------------------------------------------#
  return(screened);
}















#' Screener Class
#' An S4 class providing the methods to perform a features screening.
#'
#' The object consists of 10 slots
#' @slot method the type of screening to apply
#'\describe{
#'   \item{\code{default}}{if default is selected, the adopted test is different depending on the response type:
#'        \describe{
#'            \item{\code{gaussian}}{test is correlation}
#'            \item{\code{mgaussian}}{test is correlation (for each gene, one correlation is computed with each response variable)}
#'            \item{\code{binomial}}{test is t-test or Wilcox test, depending on the number of observations}
#'            \item{\code{multinomial}}{test is anova or Kruskall-Wallis test, depending on the number of observations}
#'            \item{\code{poisson}}{test is correlation}
#'            \item{\code{cox}}{test is a Cox proportional hazards regression model}
#'        }
#'   }
#'   \item{\code{ebayes}}{}
#'   \item{\code{permutation}}{}
#'}
#' @slot resp.type the response type
#' @slot x the input matrix, where rows are observations and columns are variables.
#' @slot y the response variable. Its number of rows must match the number of rows of \code{x}.
#' @slot cutoff numeric, the p-value to use as a threshold for filtering not significant variables.
#' @slot coef (optional) an integer indicating the response variable to consider in multi-response data
#' @slot maxvars integer, the maximum number of variables to keep. If the filtering is returning an higher number of variables,
#' only the most significant \code{maxvars} variables are kept.
#' @slot default list, arguments to the function performing a default screening
#' @slot ebayes list, arguments to the function performing a screening based on empirical Bayes methods
#' @slot permutation list, arguments to the function performing a screening based on permutation
#' @slot logger a \linkS4class{Logger}
#' @author Alessandro Barberis
# methods::setClass(
#   Class = "Screener",
#   slots = c(
#     method      = "character",
#     resp.type   = "character",
#     # x           = "ANY",
#     # y           = "ANY",
#     # weights     = "ANY",
#     cutoff      = "numeric",
#     coef        = "integerOrNULL",
#     maxvars     = "integer",
#     default     = "list",
#     ebayes      = "list",
#     permutation = "list",
#     logger      = "Logger"
#   )
# )
#
#
# Screener <- function(
#   method      = c("default", "ebayes", "permutation"),
#   resp.type   = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
#   # x           = matrix(),
#   # y           = vector(),
#   # weights     = NULL,
#   cutoff      = numeric(),
#   coef        = NULL,
#   maxvars     = integer(),
#   default     = list(),
#   ebayes      = list(),
#   permutation = list(),
#   logger      = Logger()
# ){
#   methods::new(
#     Class = "Screener",
#     method      = method,
#     resp.type   = resp.type,
#     # x           = x,
#     # y           = y,
#     # weights     = weights,
#     cutoff      = cutoff,
#     coef        = coef,
#     maxvars     = maxvars,
#     default     = default,
#     ebayes      = ebayes,
#     permutation = permutation,
#     logger      = logger
#   )
# }


# methods::setMethod(f = "get_method",      signature = "Screener", definition = function(object){methods::slot(object = object, name = 'method')})
# # methods::setMethod(f = "get_x",           signature = "Screener", definition = function(object){methods::slot(object = object, name = 'x')})
# # methods::setMethod(f = "get_y",           signature = "Screener", definition = function(object){methods::slot(object = object, name = 'y')})
# # methods::setMethod(f = "get_weights",     signature = "Screener", definition = function(object){methods::slot(object = object, name = 'weights')})
# methods::setMethod(f = "get_resp_type",   signature = "Screener", definition = function(object){methods::slot(object = object, name = 'resp.type')})
# methods::setMethod(f = "get_cutoff",      signature = "Screener", definition = function(object){methods::slot(object = object, name = 'cutoff')})
# methods::setMethod(f = "get_maxvars",     signature = "Screener", definition = function(object){methods::slot(object = object, name = 'maxvars')})
# methods::setMethod(f = "get_coef",        signature = "Screener", definition = function(object){methods::slot(object = object, name = 'coef')})
# methods::setMethod(f = "get_logger",      signature = "Screener", definition = function(object){methods::slot(object = object, name = 'logger')})
#
# methods::setMethod(f = "get_default",     signature = "Screener", definition = function(object){methods::slot(object = object, name = 'default')})
# methods::setMethod(f = "get_ebayes",      signature = "Screener", definition = function(object){methods::slot(object = object, name = 'ebayes')})
# methods::setMethod(f = "get_permutation", signature = "Screener", definition = function(object){methods::slot(object = object, name = 'permutation')})
# methods::setMethod(
#   f = "get_args",
#   signature = "Screener",
#   definition = function(object, method = c("default", "ebayes", "permutation")){
#     method = match.arg(method);
#     return(methods::slot(object = object, name = method))
#   }
# )
#
# # methods::setMethod(f = "set_x",       signature = "Screener", definition = function(object, value){ methods::slot(object = object, name = 'x') = value; return(object)})
# # methods::setMethod(f = "set_y",       signature = "Screener", definition = function(object, value){ methods::slot(object = object, name = 'y') = value; return(object)})
# # methods::setMethod(f = "set_weights", signature = "Screener", definition = function(object, value){ methods::slot(object = object, name = 'weights') = value; return(object)})
# methods::setMethod(f = "set_ebayes",  signature = "Screener", definition = function(object, value){ methods::slot(object = object, name = 'ebayes') = value; return(object)})
# methods::setMethod(f = "set_maxvars", signature = "Screener", definition = function(object, value){ methods::slot(object = object, name = 'maxvars') = value; return(object)})

# methods::setMethod(
#   f = "subset_observations",
#   signature = "Screener",
#   definition = function(object, which){
#
#     # x       = get_x(object = object)
#     # y       = get_y(object = object)
#     ebayes  = get_ebayes(object = object)
#     # weights = get_weights(object = object)
#
#     # #----------------------------------------------------------------------#
#     # #Subset predictors matrix
#     # if(!S4Vectors::isEmpty(x)){
#     #   x = x[, which, drop = FALSE];
#     #   #update
#     #   object = set_x(object = object , value = x)
#     # }
#     #
#     # #----------------------------------------------------------------------#
#     # #Subset response
#     # if(!is.null(y) && !S4Vectors::isEmpty(y)){
#     #   if (is.matrix(y)){
#     #     y = y[which, , drop = FALSE]
#     #   } else {
#     #     y = y[which]
#     #   }
#     #   #update
#     #   object = set_y(object = object , value = y)
#     # }
#     #
#     # #----------------------------------------------------------------------#
#     # #Subset response
#     # if(!is.null(weights) && !S4Vectors::isEmpty(weights)){
#     #   if (is.matrix(weights)){
#     #     weights = weights[which, , drop = FALSE]
#     #   } else {
#     #     weights = weights[which]
#     #   }
#     #   #update
#     #   object = set_weights(object = object , value = weights)
#     # }
#
#     #----------------------------------------------------------------------#
#     #Subset design matrix
#     if(!is.null(ebayes)){
#       #get design matrix
#       design = ebayes[['design']]
#       if(!is.null(design)){
#         design = design[which,,drop=F]
#         ebayes[['design']] = design
#         #update
#         object = set_ebayes(object = object , value = ebayes)
#       }
#     }
#
#     #----------------------------------------------------------------------#
#     return(object)
#   }
# )

#' Features screening
#' @description This function performs a feature screening, and return the index of the retained elements
#' in order of significance.
#'@param x the input matrix, where rows are observations and columns are variables.
#'@param observations (optional) indices of observations to keep
#' @return the index of features to keep.
#' @author Alessandro Barberis
# methods::setMethod(
#   f = "screen",
#   signature = "Screener",
#   definition = function(object, x, y, weights, resp.type, coef, observations, ...){
#
#
#     #--------------------------------------------------------------------------------------------#
#     #logger
#     logger = get_logger(object = object)
#     logger = open_con(logger)
#
#
#     #--------------------------------------------------------------------------------------------#
#     #Subset observations
#     x       = subset_observations(object = x,       which = observations)
#     y       = subset_observations(object = y,       which = observations)
#     # offset  = subset_observations(object = offset,  which = observations)
#     weights = subset_observations(object = weights, which = observations)
#
#     #--------------------------------------------------------------------------------------------#
#     #screening method
#     methods = get_method(object = object);
#     #cutoff
#     cutoff  = get_cutoff(object = object)
#     #max number of variables
#     maxvars = get_maxvars(object = object)
#
#     if(length(maxvars)>1){
#       warning("Multiple values passed as 'maxvars'. Only maximum value is kept.\n")
#       maxvars = max(maxvars, na.rm = TRUE)
#     }
#     #--------------------------------------------------------------------------------------------#
#     #create args
#     # args = list(
#     #   x         = get_x(object = object),
#     #   y         = get_y(object = object),
#     #   weights   = get_weights(object = object),
#     #   resp.type = get_resp_type(object = object),
#     #   coef      = get_coef(object = object),
#     #   logger    = get_logger(object = object)
#     # )
#
#     log_trace(object = logger, message = "Setting arguments to pass to screening function...", sep = "", add.level = TRUE, add.time = TRUE)
#     # x         = ifelse(test = missing(x),         yes = get_x(object = object),         no = x)
#     # y         = ifelse(test = missing(y),         yes = get_y(object = object),         no = y)
#     # weights   = ifelse(test = missing(weights),   yes = get_weights(object = object),   no = weights)
#     # resp.type = ifelse(test = missing(resp.type), yes = get_resp_type(object = object), no = resp.type)
#     # coef      = ifelse(test = missing(coef),      yes = get_coef(object = object),      no = coef)
#     # if(missing(x)){        x = get_x(object = object)}
#     # if(missing(y)){        y = get_y(object = object)}
#     # if(missing(weights)){  weights = get_weights(object = object)}
#     if(missing(resp.type)){resp.type = get_resp_type(object = object)}
#     if(missing(coef)){     coef = get_coef(object = object)}
#     log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
#
#     log_trace(object = logger, message = "Creating list...", sep = "", add.level = TRUE, add.time = TRUE)
#     args = list(
#       x         = t(x),
#       y         = y,
#       weights   = weights,
#       resp.type = resp.type,
#       coef      = coef,
#       logger    = get_logger(object = object)
#     )
#     log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
#
#     rm(x, y, weights)
#
#     #--------------------------------------------------------------------------------------------#
#     #loop over methods
#     out = list()
#     for(method in methods){
#
#       log_trace(object = logger, message = paste0("Screening (", method, ")"), sep = "\n", add.level = TRUE, add.time = TRUE)
#
#       out[[method]] = do.call(
#         what = screening,
#         args = c(
#           args,
#           get_args(object = object, method = method),
#           list(method = method)
#         )
#       )
#     }
#
#     #--------------------------------------------------------------------------------------------#
#     #SIGNIFICANCE SELECTION
#     log_trace(object = logger, message = "Combine results", sep = "\n", add.level = TRUE, add.time = TRUE)
#     test = do.call(what = cbind, args = out)
#
#     #Select the least significant among the values
#     log_trace(object = logger, message = "Select the least significant value...", sep = "", add.level = TRUE, add.time = TRUE)
#     test = apply(X = test, MARGIN = 1, FUN = max, na.rm = TRUE)
#     log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
#
#     #--------------------------------------------------------------------------------------------#
#     #Get an ordered list of elements (increasing p-value)
#     log_trace(object = logger, message = "Order by significance...", sep = "", add.level = TRUE, add.time = TRUE)
#     ordered = order(test, decreasing = FALSE, na.last = TRUE);
#     log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
#
#     #--------------------------------------------------------------------------------------------#
#     #If threshold was provided, get the index of the elements
#     #whose significance is < than threshold (ordered by significance)
#     if(!is.null(cutoff) && !S4Vectors::isEmpty(cutoff)){
#       log_trace(object = logger, message = "Select most significant...", sep = "", add.level = TRUE, add.time = TRUE)
#       index = which(test < cutoff);
#       #intersect
#       ordered = intersect(x = ordered, y = index);
#       log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
#     }
#
#
#     #--------------------------------------------------------------------------------------------#
#     #If the number of significant elements is greater than maxvars, limit the results
#     if(!S4Vectors::isEmpty(maxvars) && length(ordered)>maxvars){
#       #Select first n elements
#       log_trace(object = logger, message = paste("Select top", maxvars, "elements..."), sep = "", add.level = TRUE, add.time = TRUE)
#       ordered = ordered[1:maxvars]
#       log_trace(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
#     }
#
#
#     #--------------------------------------------------------------------------------------------#
#     #set output object
#     out = Screened(
#       method = method,
#       n      = maxvars,
#       index  = ordered
#     )
#
#     #--------------------------------------------------------------------------------------------#
#     #close
#     close_con(logger)
#
#     #--------------------------------------------------------------------------------------------#
#     #Return the retained features ordered by significance
#     return(out);
#   }
# )


# screening <- function(method, ...){
#
#   #----------------------------------------------------------------------#
#   # args = list(...)
#   #
#   # #----------------------------------------------------------------------#
#   # test = switch(
#   #   method,
#   #   default     = do.call(what = screening_by_default,     args = args),
#   #   ebayes      = do.call(what = screening_by_ebayes,      args = args),
#   #   permutation = do.call(what = screening_by_permutation, args = args)
#   # )
#
#   test = switch(
#     method,
#     default     = screening_by_default(...),
#     ebayes      = screening_by_ebayes(...),
#     permutation = screening_by_permutation(...)
#   )
#
#   #----------------------------------------------------------------------#
#   return(test)
# }






#'Compute the significance of the filtering test for the gaussian family
#'@keywords internal
screening_default_gaussian <- function(x, y, alternative="two.sided", method = "pearson", conf.level=0.95){
  method      = match.arg(method);
  alternative = match.arg(alternative);

  r = matrixTests::row_cor_pearson(x=x, y = y, alternative=alternative, conf.level = conf.level);

  p = r$pvalue;

  return(p)
}


#'Compute the significance of the filtering test for the multivariate gaussian family
#'@keywords internal
screening_default_mgaussian <- function(x, y, alternative="two.sided", method = "pearson", conf.level=0.95){
  #Get number of vars
  nvar = nrow(x);

  #Get var names
  names = rownames(x);

  stats = NULL;

  stats = apply(X = y, MARGIN = 2, FUN = function(y.i,...){
    screening_default_gaussian(y=y.i, ...)
  }, x=x, alternative=alternative, method=method, conf.level = conf.level)

  return(stats)
}

#'Compute the significance of the filtering test for the binomial family
#'@keywords internal
screening_default_binomial <- function(x, y, conf.level = 0.95, ...){
  #0) Get the list of parameters
  # args <- c(as.list(environment()), list(...));

  isFactor = is.factor(y);

  #1) Get minimum number of observations per class
  min.obs = if(!is.null(dim(y))){
    min(colSums(y))
  } else {
    min(table(as.factor(y)))
  }

  #Convert to factor
  if(!isFactor && !is.null(ncol(y)) && ncol(y)>1) y = dummy.matrix.to.factor(y);

  res = if(min.obs>10){
    wtest(x=x, g=y, ...);
  } else{
    ttest(x=x, g=y, conf.level=conf.level, ...);
  }

  return(res);
}




#'Compute the significance of the filtering test for the multinomial family
#'@keywords internal
screening_default_multinomial <- function(x, y, ...){
  #0) Get the list of parameters
  # args <- c(as.list(environment()), list(...));

  isFactor = is.factor(y);

  #1) Get minimum number of observations per class
  min.obs = if(!is.null(dim(y))){
    min(colSums(y))
  } else {
    min(table(as.factor(y)))
  }

  #Convert to factor
  if(!isFactor && !is.null(ncol(y)) && ncol(y)>1) y = dummy.matrix.to.factor(y);

  res = if(min.obs>10){
    kw(x=x, g=y, ...);
  } else{
    anova(x=x, g=y, ...);
  }

  return(res);
}



#'Compute the significance of the filtering test for the cox family
#'@param ... further arguments to \code{\link[survival]{coxph}}
#'@keywords internal
screening_default_cox <- function(x, y, weights = NULL, ...){
  #0) Get the list of parameters
  # args <- c(as.list(environment()), list(...));

  if(identical(colnames(y), c("time", "status")) || identical(colnames(y), c("status", "time"))){
    data = data.frame(time = y[,'time'], status = y[,'status'])
  } else {
    data = data.frame(time = y[,1], status = y[,2])
  }

  test = apply(X = x, MARGIN = 1, FUN = function(x){
    #update data frame
    data[['x']] = x;
    #Fits a Cox proportional hazards regression model
    res.cox <- survival::coxph(survival::Surv(time, status) ~ x, data = data, weights = weights, ...);
    #Produces a summary of a fitted coxph model
    summary.res.cox = summary(res.cox);
    #Get the p-values for the likelihood-ratio test for overall significance of the model
    pval = summary.res.cox[['logtest']][['pvalue']];
    #Return
    return(pval)
  })

  return(test);
}



#'@keywords internal
get_sample_screenedlist <- function(screener, ..., logger, observations, iloop){

  #Set logger
  logger = open_con(logger)


  #--------------------------------------------------------------------------------------------#
  #Screen
  log_trace(object = logger, message = paste(iloop, "Screening"), sep = "\n", add.level = TRUE, add.time = TRUE)
  screened = screen(screener = screener, observations = observations[[iloop]], ...)

  #--------------------------------------------------------------------------------------------#
  #close
  close_con(logger)

  #--------------------------------------------------------------------------------------------#
  return(screened)
  # return(list(pred = pred, true = get_y(trainer.i), weights = get_weights(trainer.i)))
}


#'@keywords internal
get_resample_screenedlist = function(screener, ..., looper, observations){

  out = loop(
    looper = looper,
    n.iter = length(observations),
    # .inorder = TRUE,
    .inorder = TRUE,
    fun = get_sample_screenedlist,
    screener = screener, observations = observations, ...)

  return(out)
}



supported_supervised_screening_methods <- function(resp.type, y){

  if(isTRUE(!missing(resp.type) && !missing(y))){
    # out = c(
    #   'ebayes'      = 'ebayes',
    #   'permutation' = 'permutation',
    #   'default'     = get_id_default_screener_test(resp.type = resp.type, y = y)
    # )
    out = stats::setNames(object = c('ebayes', 'permutation', 'default'),
                          nm = c('ebayes', 'permutation',get_id_default_screener_test(resp.type = resp.type, y = y))
                        )
  } else {
    out = c(
      'ebayes'      = 'ebayes',
      'permutation' = 'permutation',
      'wtest'       = 'default',
      'ttest'       = 'default',
      'kw'          = 'default',
      'anova'       = 'default',
      'pcorr'       = 'default',
      'logtest'     = 'default'
    )
  }

  #r
  return(out)
}

#'Supported Supervised Screening
#'
#'@description This function returns a \code{data.frame} containing the currently
#'supported supervised screening methods.
#'
#'@details The currently implemented supervised screening methods are returned in a \code{data.frame}.
#'If \code{resp.type} and \code{y} are provided, only the metrics supported for the given response
#'are returned.
#'
#'@param resp.type (optional) the response type
#'@param y (optional) the response variable
#'
#'@return A \code{data.frame} with 3 columns:
#'\describe{
#'\item{\code{id}}{contains the id used in renoir for the supervised screening method (e.g. 'ebayes')}
#'\item{\code{name}}{contains a short description of the method (e.g. 'empirical Bayes moderated t-statistics test')}
#'}
#'
#'@seealso
#'\code{\link{ebayes_screener}},
#'\code{\link{permutation_screener}},
#'\code{\link{default_screener}}
#'
#'@export
#'
#'@author Alessandro Barberis
list_supported_supervised_screening_methods <- function(resp.type, y){
  #metrics
  out = supported_supervised_screening_methods(resp.type = resp.type, y = y)
  #names
  nm = sapply(X = names(out), FUN = get_name_screener_by_id, short = F)
  #create df
  out = data.frame(id = out, description = nm, row.names = NULL, stringsAsFactors = F)

  #r
  return(out)
}










# screen = function(screener = c("default", "ebayes", "permutation"), x, y, weights = NULL, resp.type, coef = NULL, cutoff = NULL, maxvars = NULL, ...){
#
#   #----------------------------------------------------------------------#
#   if(any(!(screener %in% c("default", "ebayes", "permutation")))){
#     warning("Screening method not supported. Screener set to 'default'.\n")
#   }
#
#   if(length(maxvars)>1){
#     warning("Multiple values passed as 'maxvars'. Only maximum value is kept.\n")
#     maxvars = max(maxvars, na.rm = TRUE)
#   }
#
#   #----------------------------------------------------------------------#
#   screening = switch(
#     screener,
#     default     = screening_by_default,
#     ebayes      = screening_by_ebayes,
#     permutation = screening_by_permutation
#   )
#
#   #----------------------------------------------------------------------#
#   #loop over methods
#   out = list()
#   for(method in screener){
#     out[[method]] = do.call(
#       what = screening,
#       args = c(list(
#         x         = x,
#         y         = y,
#         weights   = weights,
#         resp.type = resp.type,
#         coef      = coef
#       ), list(...))
#     )
#   }
#
#   #----------------------------------------------------------------------#
#   #SIGNIFICANCE SELECTION
#   test = do.call(what = cbind, args = out)
#
#   #Select the least significant among the values
#   test = apply(X = test, MARGIN = 1, FUN = max, na.rm = TRUE)
#
#   #----------------------------------------------------------------------#
#   #Get an ordered list of elements (increasing p-value)
#   ordered = order(test, decreasing = FALSE, na.last = TRUE);
#
#   #----------------------------------------------------------------------#
#   #If threshold was provided, get the index of the elements
#   #whose significance is < than threshold (ordered by significance)
#   if(!is.null(cutoff) && !S4Vectors::isEmpty(cutoff)){
#     index = which(test < cutoff);
#     #intersect
#     ordered = intersect(x = ordered, y = index);
#   }
#
#
#   #----------------------------------------------------------------------#
#   #If the number of significant elements is greater than maxvars, limit the results
#   if(!is.null(maxvars) && !S4Vectors::isEmpty(maxvars) && length(ordered)>maxvars){
#     #Select first n elements
#     ordered = ordered[1:maxvars]
#   }
#
#   #----------------------------------------------------------------------#
#   #Return the retained features ordered by significance
#   return(ordered);
#
# }
