#' #'Features screening
#' #'@description This function performs a feature screening, and return the index of the retained elements
#' #'in order of significance.
#' #'@param x the input matrix, where rows are observations and columns are variables.
#' #'@param y the response variable. Its number of rows must match the number of rows of \code{x}.
#' #'@param method the type of filtering to apply
#' #'\describe{
#' #'   \item{\code{default}}{if default is selected, the adopted test is different depending on the responste type:
#' #'        \describe{
#' #'            \item{\code{gaussian}}{test is correlation}
#' #'            \item{\code{mgaussian}}{test is correlation (for each gene, one correlation is computed with each response variable)}
#' #'            \item{\code{binomial}}{test is t-test or Wilcox test, depending on the number of observations}
#' #'            \item{\code{multinomial}}{test is anova or Kruskall-Wallis test, depending on the number of observations}
#' #'            \item{\code{poisson}}{test is correlation}
#' #'            \item{\code{cox}}{test is a Cox proportional hazards regression model}
#' #'        }
#' #'   }
#' #'   \item{\code{alternative}}{alternative test uses \code{\link{compute_sam}} and \code{\link{compute_limma}}.
#' #'                             It is slower than \code{"default"}.
#' #'                            }
#' #'}
#' #'@param resp.type the response type
#' #'@param coef (optional) a number indicating the response variable to consider in multi-response data
#' #'@param p.val numeric, the p-value to use as a threshold for filtering not significant variables.
#' #'@param nvars the maximum number of variables to keep. If the filtering is returning an higher number of variables,
#' #'only the most significant \code{nvars} variables are kept.
#' #'@param sam.args arguments to \code{\link{compute_sam}}
#' #'@param limma.args arguments to \code{\link{compute_limma}}
#' #'@param ... further arguments to \code{\link{screening_default}}
#' #'@return the index of features to keep. If \code{p.val} is provided, variables are filtered if
#' #'p-value of test < \code{p.val} threshold.
#' #'If \code{nvars} is provided, the number of significant elements is limited up to \code{nvars}.
#' #'@keywords internal
#' # screening <- function(method=c("default", "alternative"),
#' #                       resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
#' #                       x, y,
#' #                       p.val, coef, nvars = 1000,
#' #                       #alternative
#' #                       sam.args, limma.args,
#' #                       #default
#' #                       ...){
#' #
#' #   args <- c(as.list(environment()), list(...));
#' #
#' #   args.def = c(args[c("x", "y", "resp.type", "p.val", "coef", "nvars")], list(...));
#' #   args.alt = args[  c("x", "y", "resp.type", "p.val", "coef", "nvars", "sam.args", "limma.args")]
#' #
#' #
#' #   method    = match.arg(method);
#' #   resp.type = match.arg(resp.type);
#' #
#' #   index = switch(method,
#' #                  default     = do.call(what = screening_default,      args = args.def),
#' #                  alternative = do.call(what = screening_alternative,  args = args.alt))
#' #
#' #   return(index);
#' # }
#'
#'
#'
#'
#'
#'
#' #'Filter the less significant variables
#' #'@description This function performs a feature screening, and return the index of the retained elements
#' #'in order of significance.
#' #'Input variables are filtered if p-value of test < threshold.
#' #'The adopted test is different depending on the responste type:
#' #'\describe{
#' #'\item{\code{gaussian}}{test is correlation}
#' #'\item{\code{mgaussian}}{test is correlation (for each input variable, one correlation is computed with each response variable)}
#' #'\item{\code{binomial}}{test is t-test or Wilcox test, depending on the number of observations}
#' #'\item{\code{multinomial}}{test is anova or Kruskall-Wallis test, depending on the number of observations}
#' #'\item{\code{poisson}}{test is correlation}
#' #'\item{\code{cox}}{test is a Cox proportional hazards regression model}
#' #'}
#' #'
#' #'@inheritParams screening_default_pval
#' #'@param p.val numeric, the p-value to use as a threshold for filtering not significant variables.
#' #'@param nvars the maximum number of variables to keep. If the filtering is returning an higher number of variables,
#' #'only the most significant \code{nvars} variables are kept.
#' #'@return the index of retained elements (i.e. elements with p-value of test < \code{p.val})
#' #'@keywords internal
#' screening_default <- function(x, y, alternative="two.sided", method = "pearson", conf.level=0.95,
#'                        resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
#'                        p.val, coef,
#'                        nvars = 1000, ...){
#'
#'   #Get the arguments
#'   args <- c(as.list(environment()), list(...));
#'   args[c("p.val", "coef", "nvars")] = NULL;
#'
#'
#'   #----------------------------------------------------------------------#
#'
#'   #Check if has coef
#'   if(!missing(coef) && !is.null(coef)){hasCoef=TRUE}else{hasCoef=FALSE}
#'
#'   #Check if p-value threshold is provided
#'   if(!missing(p.val) && !is.null(p.val)){hasThr=TRUE}else{hasThr=FALSE}
#'
#'   #----------------------------------------------------------------------#
#'
#'   #Get the p-value for the auto-selected test
#'   test = do.call(what = screening_default_pval, args = args);
#'
#'   #----------------------------------------------------------------------#
#'
#'   #Check if test is a list, if so shape the results to have a vector of p-values
#'   if(is.matrix(test)){
#'     #Shape mgaussian results
#'     if(hasCoef){
#'       test = test[,coef];
#'     } else {
#'       #If no coefficients has been pre-selected, get the least significant value
#'       #(i.e. max p-value) across coefficients
#'       test = apply(X = test, MARGIN = 1, FUN = max)
#'     }
#'   }
#'
#'   #----------------------------------------------------------------------#
#'   #Get an ordered list of elements (increasing p-value)
#'   ordered = order(test, decreasing = FALSE, na.last = TRUE);
#'
#'   #----------------------------------------------------------------------#
#'
#'   #If threshold was provided, get the index of the elements
#'   #whose significance is < than threshold (ordered by significance)
#'   if(hasThr){
#'     index = which(test < p.val);
#'     #intersect
#'     ordered = intersect(x = ordered, y = index);
#'   }
#'
#'
#'   #----------------------------------------------------------------------#
#'   #If the number of significant elements is greater than nvars, limit the results
#'   if(length(ordered)>nvars){
#'     #Select first n elements
#'     ordered = ordered[1:nvars]
#'   }
#'
#'   #----------------------------------------------------------------------#
#'   #Return the retained features ordered by significance
#'
#'   return(ordered);
#' }
#'
#' #'Compute the significance of the filtering test
#' #'@description This function compute the significance of the screening test
#' #'@param x numeric matrix
#' #'@param y response type (must be a factor for binomial and multinomial)
#' #'@param method which correlation method to use. Currently, only "pearson" is supported.
#' #'@param alternative alternative hypotesis to use. Must be one of \code{"two.sided"} (default),
#' #'\code{"greater"} or \code{"less"}.
#' #'@param conf.level confidence levels used for the confidence intervals (where computed). A single number or a
#' #'numeric vector with value for each observation. All values must be in the range of [0;1].
#' #'@param ... further arguments
#' #'@return the p-value of the selected test
#' #'@keywords internal
#' screening_default_pval <- function(x, y, alternative="two.sided", method = "pearson", conf.level=0.95,
#'                             resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
#'                             ...){
#'   test = switch(resp.type,
#'                 gaussian    = screening_default_gaussian(x=x, y=y, alternative=alternative, method=method, conf.level = conf.level),
#'                 mgaussian   = screening_default_mgaussian(x=x, y=y, alternative=alternative, method=method, conf.level = conf.level),
#'                 binomial    = screening_default_binomial(   x=x, y=y, alternative=alternative, conf.level = conf.level, ...),
#'                 multinomial = screening_default_multinomial(x=x, y=y, ...),
#'                 poisson     = screening_default_gaussian(x=x, y=y, alternative=alternative, method=method, conf.level = conf.level),
#'                 cox         = screening_default_cox(        x=x, y=y, ...))
#'
#'
#'   return(test);
#' }
#'
#'
#' #'Compute the significance of the filtering test for the gaussian family
#' #'@keywords internal
#' screening_default_gaussian <- function(x, y, alternative="two.sided", method = "pearson", conf.level=0.95){
#'   method      = match.arg(method);
#'   alternative = match.arg(alternative);
#'
#'   r = matrixTests::row_cor_pearson(x=x, y = y, alternative=alternative, conf.level = conf.level);
#'
#'   p = r$pvalue;
#'
#'   return(p)
#' }
#'
#'
#' #'Compute the significance of the filtering test for the multivariate gaussian family
#' #'@keywords internal
#' screening_default_mgaussian <- function(x, y, alternative="two.sided", method = "pearson", conf.level=0.95){
#'   #Get number of vars
#'   nvar = nrow(x);
#'
#'   #Get var names
#'   names = rownames(x);
#'
#'   stats = NULL;
#'
#'   stats = apply(X = y, MARGIN = 2, FUN = function(y.i,...){
#'     screening_default_gaussian(y=y.i, ...)
#'   }, x=x, alternative=alternative, method=method, conf.level = conf.level)
#'
#'   return(stats)
#' }
#'
#' #'Compute the significance of the filtering test for the binomial family
#' #'@keywords internal
#' screening_default_binomial <- function(x, y, conf.level = 0.95, ...){
#'   #0) Get the list of parameters
#'   args <- c(as.list(environment()), list(...));
#'
#'   isFactor = is.factor(y);
#'
#'   #1) Get minimum number of observations per class
#'   min.obs = if(!is.null(dim(y))){
#'     min(colSums(y))
#'   } else {
#'     min(table(as.factor(y)))
#'   }
#'
#'   #Convert to factor
#'   if(!isFactor && !is.null(ncol(y)) && ncol(y)>1) y = dummy.matrix.to.factor(y);
#'
#'   res = if(min.obs>10){
#'     wtest(x=x, g=y, ...);
#'   } else{
#'     ttest(x=x, g=y, conf.level=conf.level, ...);
#'   }
#'
#'   return(res);
#' }
#'
#'
#'
#'
#' #'Compute the significance of the filtering test for the multinomial family
#' #'@keywords internal
#' screening_default_multinomial <- function(x, y, ...){
#'   #0) Get the list of parameters
#'   args <- c(as.list(environment()), list(...));
#'
#'   isFactor = is.factor(y);
#'
#'   #1) Get minimum number of observations per class
#'   min.obs = if(!is.null(dim(y))){
#'     min(colSums(y))
#'   } else {
#'     min(table(as.factor(y)))
#'   }
#'
#'   #Convert to factor
#'   if(!isFactor && !is.null(ncol(y)) && ncol(y)>1) y = dummy.matrix.to.factor(y);
#'
#'   res = if(min.obs>10){
#'     kw(x=x, g=y, ...);
#'   } else{
#'     anova(x=x, g=y, ...);
#'   }
#'
#'   return(res);
#' }
#'
#'
#'
#' #'Compute the significance of the filtering test for the cox family
#' #'@param ... further arguments to \code{\link[survival]{coxph}}
#' #'@keywords internal
#' screening_default_cox <- function(x, y, ...){
#'   #0) Get the list of parameters
#'   args <- c(as.list(environment()), list(...));
#'
#'   if(identical(colnames(y), c("time", "status")) || identical(colnames(y), c("status", "time"))){
#'     data = data.frame(time = y[,'time'], status = y[,'status'])
#'   } else {
#'     data = data.frame(time = y[,1], status = y[,2])
#'   }
#'
#'   test = apply(X = x, MARGIN = 1, FUN = function(x){
#'     #update data frame
#'     data[['x']] = x;
#'     #Fits a Cox proportional hazards regression model
#'     res.cox <- survival::coxph(survival::Surv(time, status) ~ x, data = data, ...);
#'     #Produces a summary of a fitted coxph model
#'     summary.res.cox = summary(res.cox);
#'     #Get the p-values for the likelihood-ratio test for overall significance of the model
#'     pval = summary.res.cox[['logtest']][['pvalue']];
#'     #Return
#'     return(pval)
#'   })
#'
#'   return(test);
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' #'Filter the less significant variables
#' #'@inheritParams screening
#' #'@return the index of retained elements (i.e. elements with p-value of test < \code{p.val}), ordered
#' #'by decreasing significance.
#' #'@keywords internal
#' screening_alternative <- function(x, y,
#'                                   resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
#'                                   sam.args, limma.args,
#'                                   p.val, coef,
#'                                   nvars = 1000, ...){
#'
#'   #Get the arguments
#'   args <- c(as.list(environment()), list(...));
#'   args[c("p.val", "coef", "nvars")] = NULL;
#'
#'
#'   #----------------------------------------------------------------------#
#'
#'   #Check if has coef
#'   if(!missing(coef) && !is.null(coef)){hasCoef=TRUE}else{hasCoef=FALSE}
#'
#'   #Check if p-value threshold is provided
#'   if(!missing(p.val) && !is.null(p.val)){hasThr=TRUE}else{hasThr=FALSE}
#'
#'   #----------------------------------------------------------------------#
#'
#'   #Get the p-value for the auto-selected test
#'   test = do.call(what = screening_alternative_pval, args = args);
#'
#'   #----------------------------------------------------------------------#
#'
#'   #Check if test is a list, if so shape the results to have a vector of p-values
#'   #This should never be triggered as for limma we use F-score and samr the max p-value
#'   if(is.list(test)){
#'     #Shape mgaussian results
#'     if(hasCoef){
#'       test = test[,coef];
#'     } else {
#'       #If no coefficients has been pre-selected, get the least significant value (i.e. max p-value) across coefficients
#'       test = apply(X = test, MARGIN = 1, FUN = max)
#'     }
#'   }
#'
#'   #----------------------------------------------------------------------#
#'   #Get an ordered list of elements (increasing p-value)
#'   ordered = order(test, decreasing = FALSE, na.last = TRUE);
#'
#'   #----------------------------------------------------------------------#
#'
#'   #If threshold was provided, get the index of the elements
#'   #whose significance is < than threshold (ordered by significance)
#'   if(hasThr){
#'     index = which(test < p.val);
#'     #intersect
#'     ordered = intersect(x = ordered, y = index);
#'   }
#'
#'
#'   #----------------------------------------------------------------------#
#'   #If the number of significant elements is greater than nvars, limit the results
#'   if(length(ordered)>nvars){
#'     #Select first n elements
#'     ordered = ordered[1:nvars]
#'   }
#'
#'   #----------------------------------------------------------------------#
#'   #Return the retained features ordered by significance
#'
#'   return(ordered);
#' }
#'
#'
#' #'Compute the significance of the filtering test
#' #'@description This function compute the significance of the screening test
#' #'@return The p-value associated with each feature
#' #'@keywords internal
#' screening_alternative_pval <- function(x, y,
#'                                        resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
#'                                        coef = NULL,
#'                                        sam.args, limma.args,
#'                                        ...){
#'
#'
#'   if(missing(x) || missing(y)){stop("Error: 'x' and 'y' must be provided.")}
#'
#'   #if var names are not provided, generate them (needed to return index)
#'   # if((missing(sam) || missing(limma)) && !missing(x)){
#'   rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");
#'
#'   #----------------------------------------------------------------------#
#'   #SAM
#'
#'   sam.out = do.call(what = screening_alternative_pval_sam,
#'                     args = c(sam.args, list(x = x, y = y, resp.type = resp.type, coef = coef)))
#'
#'
#'   #----------------------------------------------------------------------#
#'   #LIMMA
#'
#'   limma.out = do.call(what = screening_alternative_pval_limma,
#'                       args = c(limma.args, list(x = x, y = y, resp.type = resp.type)))
#'
#'
#'   #----------------------------------------------------------------------#
#'   #SIGNIFICANCE SELECTION
#'   test = cbind(sam.out, limma.out)
#'
#'   #Select the least significant among the values
#'   test = apply(X = test, MARGIN = 1, FUN = max, na.rm = TRUE)
#'
#'   #----------------------------------------------------------------------#
#'
#'   return(test);
#' }
#'
#'
#'
#' #'Compute the significance of the filtering test
#' #'@description This function compute the significance of the screening test by using
#' #'\code{\link[renoir]{compute_sam}}
#' #'@return The p-value associated with each feature
#' #'@keywords internal
#' screening_alternative_pval_sam <- function(x, y,
#'                                            resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
#'                                            coef = NULL,
#'                                            ...){
#'
#'   # args <- c(as.list(environment()), list(...));
#'   # args[c("resp.type")] = NULL;
#'
#'   args <- c(list(...));
#'
#'   #---------------------------------------------------------------------------#
#'
#'   #Check if has coef
#'   if(!missing(coef) && !is.null(coef)){hasCoef=TRUE}else{hasCoef=FALSE}
#'
#'   #---------------------------------------------------------------------------#
#'
#'   if(!missing(x)){
#'     rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");
#'   }
#'
#'   #---------------------------------------------------------------------------#
#'   #b) Update genenames
#'   feat.order = rownames(x);
#'
#'   #---------------------------------------------------------------------------#
#'   #Compute Significance analysis of microarrays
#'   samr.res = compute_sam(x = x,
#'                          y = y,
#'                          resp.type = resp.type,
#'                          coef = coef,
#'                          ...)
#'
#'   #---------------------------------------------------------------------#
#'   #Extract significance
#'
#'   samr.res = extract_qval_sam(obj = samr.res,
#'                               feat.order = feat.order,
#'                               resp.type = resp.type,
#'                               coef = coef)
#'
#'   #---------------------------------------------------------------------------#
#'
#'   return(samr.res);
#' }
#'
#'
#' #'Compute the significance of the filtering test
#' #'@description This function compute the significance of the screening test by using
#' #'\code{\link[renoir]{compute_limma}}
#' #'@param logged2 whether data was logged 2.
#' #'@return The p-value associated with each feature or NULL if the response type is not supported
#' #'@keywords internal
#' screening_alternative_pval_limma <- function(x, y,
#'                                              resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
#'                                              logged2 = FALSE,
#'                                              coef = NULL,
#'                                              ...){
#'   args <- c(list(...));
#'
#'   #LIMMA DOESN'T SUPPORT THESE TYPES
#'   if(identical(resp.type, 'poisson') || identical(resp.type, 'cox')){
#'     return(NULL)
#'   }
#'
#'   #---------------------------------------------------------------------------#
#'
#'   rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");
#'
#'   #---------------------------------------------------------------------------#
#'   #Run limma
#'   limma.res = compute_limma(x = x,
#'                             y = y,
#'                             resp.type = resp.type,
#'                             logged2 = logged2,
#'                             coef = coef, ...)
#'
#'   #---------------------------------------------------------------------------#
#'   #Extract p-value
#'   limma.sel = extract_pval_limma(obj = limma.res,
#'                                  coef = coef,
#'                                  feat.order = rownames(x))
#'
#'   #---------------------------------------------------------------------------#
#'   return(limma.sel);
#'
#' }
#'
#'
#'
#'
#'
