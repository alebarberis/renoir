#' @include classes_generics.R class_Logger.R
NULL

#'Significance analysis of microarrays (wrapper function to \code{\link[samr]{SAM}})
#'@description Correlates a large number of features (eg genes) with an outcome variable,
#'            such as a group indicator, quantitative variable or survival time by using \code{\link[samr]{SAM}}
#'            function.
#'@param x Feature matrix: p (number of features) by n (number of samples), one observation per column (missing values allowed)
#'@param y n-vector of outcome measurements
#'@param censoring.status n-vector of censoring censoring.status
#'            (1=died or event occurred, 0=survived, or event was censored), needed for a censored survival outcome
#'
#'@param resp.type Problem type:\cr
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
#'@param geneid Optional character vector of geneids for output.
#'@param genenames Optional character vector of genenames for output.
#'@param assay.type Assay type: "array" for microarray data, "seq" for counts from sequencing
#'@param s0 Exchangeability factor for denominator of test statistic; Default in original function is a automatic choice.
#'          Only used for array data. Our default is a small number since sometimes the automatic choice could get weird results.
#'@param s0.perc Percentile of standard deviation values to use for s0;
#'               default is automatic choice;
#'               -1 means s0=0 (different from s0.perc=0, meaning s0=zeroeth percentile of standard deviation values=
#'               min of sd values. Only used for array data.
#'@param nperms Number of permutations used to estimate false discovery rates
#'@param center.arrays Should the data for each sample (array) be median centered at the outset? Default =FALSE. Only used for array data.
#'@param testStatistic Test statistic to use in two class unpaired case.
#'                     Either "standard" (t-statistic) or ,"wilcoxon" (Two-sample wilcoxon or Mann-Whitney test).
#'                     Only used for array data.
#'@param time.summary.type Summary measure for each time course: "slope", or "signed.area"). Only used for array data.
#'@param return.x = Should the matrix of feature values be returned? Only useful for time course data, where x contains summaries of the features over time. Otherwise x is the same as the input data data\$x
#'@param knn.neighbors Number of nearest neighbors to use for imputation of missing features values. Only used for array data.
#'@param random.seed Optional initial seed for random number generator (integer)
#'@param nresapm For assay.type="seq", number of resamples used to construct test statistic. Default 20. Only used for sequencing data.
#@param nresamp.perm For assay.type="seq", number of resamples used to construct test statistic for permutations.
#'                    Default is equal to nresamp and it must be at most nresamp. Only used for sequencing data
#'@param logged2  Has the data been transformed by log (base 2)? This information is used only for computing fold changes
#'@param fdr.output (Approximate) False Discovery Rate cutoff for output in significant genes table
#'@param eigengene.number Eigengene to be used (just for resp.type="Pattern discovery")
#'@param plotSam whether to make Q-Q plot for SAM analysis, default TRUE.
#'@param outdir output directory
#'@param all.genes Should all genes be listed? Default FALSE
#'@param logger a \linkS4class{Logger}
#@param verbose logical. Should R report extra information on progress? Set to TRUE.
#@param warningsFile A path to the warning file to use.
#@param logFile A path to the log file to use.
#'@return A list with components (the same as returned by \code{\link[samr]{SAM}})
#'@keywords internal
sam <- function(
  x,
  y=NULL,
  censoring.status=NULL,
  resp.type=c("Quantitative","Two class unpaired",
              "Survival","Multiclass", "One class", "Two class paired",
              "Two class unpaired timecourse", "One class timecourse",
              "Two class paired timecourse", "Pattern discovery"),
 geneid=NULL, genenames = NULL,
 assay.type=c("array","seq"), s0=0.0001, s0.perc=NULL, nperms=100,
 center.arrays=FALSE, testStatistic=c("standard","wilcoxon"),
 time.summary.type=c("slope","signed.area"),
 regression.method=c("standard","ranks"), return.x=FALSE,
 knn.neighbors=10, random.seed=NULL,
 logged2 = FALSE,
 fdr.output = 0.20,
 eigengene.number = 1,
 nresamp = 20,
 plotSam=FALSE,
 verbose=FALSE,
 # outdir= getwd(),
 outdir = NULL,
 logger = Logger(),
 all.genes = FALSE){
  # nresamp=20,nresamp.perm=NULL){

  # this.call = c(match.call()[1], as.list(environment()));
  this.call = match.call()

  if(!missing(outdir) && !is.null(outdir)){hasOutdir = T} else {hasOutdir = F}

  if(hasOutdir && !dir.exists(outdir)){dir.create(outdir, recursive = TRUE)}

  logger = open_con(logger)

  #---------------------------------------------------------------------#
  assay.type        = match.arg(assay.type);
  testStatistic     = match.arg(testStatistic);
  time.summary.type = match.arg(time.summary.type);
  regression.method = match.arg(regression.method);
  #---------------------------------------------------------------------#

  log_debug(object = logger, message = "Computing the significance analysis of microarrays...", sep = "", add.level = TRUE, add.time = TRUE)

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

  nobs = ncol(x);

  if(nperms>nobs){nperms = nobs}

  sam.res = switch(assay.type,
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

  log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)

  #--------------------------------------------------------------#

  #ERROR IS RAISED:
  #sam.rnk.res = samr::SAM(x=x[,rownames(bmiValAnn)], y = bmiValAnn, resp.type = "Quantitative", fdr.output = 0.2, logged2 = TRUE, regression.method="ranks")

  # Value of delta (distance from 45 degree line in SAM plot)
  # used for creating siggenes.table.
  # Changing fdr.output will change the resulting del.
  #
  #   sam.delta = sam.res$del;
  #
  #   #Delta table
  #   #It lists the number of significant genes (# called) and the false
  #   #positive rate for a number of values of delta.
  #   #
  #   #The False Discovery Rate (FDR) is computed as [median (or 90th percentile) of the number
  #   #of falsely called genes] divided by [the number of genes called significant].
  #   sam.deltaT = sam.res$delta.table



  #--------------------------------------------------------------#
  if(hasOutdir){
    log_debug(object = logger, message = "Saving the results...", sep = "", add.level = TRUE, add.time = TRUE)

    save(sam.res, file = file.path(outdir, "samr.RData"))

    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
  }

  if(hasOutdir){

    #--------------------------------------------------------------#
    #
    # TO INTERPRET THE RESULTS
    #
    # Score(d)            = The T-statistic value. For each gene i, it is computed as:
    #                                   r_i
    #                         d_i = ----------
    #                               (s_i + s0)
    #
    #                       where r_i is a score, s_i is a standard deviation and s0 is
    #                       an exchangeability factor.
    #
    # Numerator(r)        = The numerator of the T-statistic. r is the linear regression coefficient of a gene
    #                       on the outcome.
    # Denominator(s + s0) = The denominator of the T-statistic.
    # q-value(%)          = the lowest False Discovery Rate at which the gene is called significant based on
    #                       the work of John Storey [6] who invented q-values. It is like the familiar "p-value", adapted
    #                       to the analysis of a large number of genes. The q-value measures how significant the gene
    #                       is: as d-ith > 0 increases, the corresponding q-value decreases.
    #                       I found a qvalue R package exists.
    #
    #--------------------------------------------------------------#

    #Significant genes positively correlated with the outcome
    # if(is.matrix(sam.res$siggenes.table$genes.up)){
    #   sam.genesUp = sam.res$siggenes.table$genes.up[,1]
    # } else if(is.vector(sam.res$siggenes.table$genes.up)){
    #   sam.genesUp = sam.res$siggenes.table$genes.up[1]
    # } else {
    #   sam.genesUp = NULL;
    # }

    if(is.vector(sam.res$siggenes.table$genes.up)){
      sam.res$siggenes.table$genes.up = t(as.matrix(sam.res$siggenes.table$genes.up))
    }
    sam.genesUp = sam.res$siggenes.table$genes.up[,1]

    #Significant genes negatively correlated with the outcome
    #Higher expressions correlate with lower bmi
    # if(is.matrix(sam.res$siggenes.table$genes.lo)){
    #   sam.genesDw = sam.res$siggenes.table$genes.lo[,1]
    # } else if(is.vector(sam.res$siggenes.table$genes.lo)){
    #   sam.genesDw = sam.res$siggenes.table$genes.lo[1]
    # } else {
    #   sam.genesDw = NULL;
    # }

    if(is.vector(sam.res$siggenes.table$genes.lo)){
      sam.res$siggenes.table$genes.lo = t(as.matrix(sam.res$siggenes.table$genes.lo))
    }
    sam.genesDw = sam.res$siggenes.table$genes.lo[,1]

    #Merge the results
    sam.genes = union(sam.genesUp, sam.genesDw);

    utils::write.csv(x = sam.genes, file = file.path(outdir, "significant_genes.csv"), row.names = F)
  }

  # length(which(sam.genesDw[,"Numerator(r)"]!=0))

  #Show the SAM plot
  #Positive significant genes are labelled in red and negative
  #significant genes are in green
  if(plotSam & hasOutdir){
    log_debug(object = logger, message = "Saving the Q-Q plot for SAM analysis...", sep = "", add.level = TRUE, add.time = TRUE)

    resFilePath = file.path(outdir, paste0("samr_qq_plot.pdf"));

    grDevices::pdf(file = resFilePath)
    samr::samr.plot(sam.res$samr.obj);
    grDevices::dev.off(); # to reset the graphics pars to defaults

    log_debug(object = logger, message = "DONE", sep = "\n", add.level = F, add.time = F)
  }


  close_con(logger)

  return(list(sam.res=sam.res, call=this.call))

}




#'Significance analysis of microarrays
#'@description This function computes the significance analysis of microarrays.
#'This function is a wrapper for \code{\link[renoir]{sam}}
#'@return The p-value associated with each feature
#'@keywords internal
compute_sam <- function(
  x, y,
  resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox"),
  coef = NULL,
  logger,
  ...){

  # args <- c(as.list(environment()), list(...));
  # args[c("resp.type")] = NULL;

  args <- c(list(logger = logger), list(...));

  #---------------------------------------------------------------------------#

  #Check if has coef
  if(!missing(coef) && !is.null(coef)){hasCoef=TRUE}else{hasCoef=FALSE}


  #---------------------------------------------------------------------------#

  if(!missing(x)){
    rownames(x) = rownames(x = x, do.NULL = FALSE, prefix = "r");
  }

  #---------------------------------------------------------------------------#
  sam.resp.type = switch(resp.type,
                         gaussian    = "Quantitative",
                         mgaussian   = "Quantitative",
                         binomial    = "Two class unpaired",
                         multinomial = "Multiclass",
                         poisson     = "Quantitative",
                         cox         = "Survival"
  )

  # f = formals(fun = computeSam)

  #---------------------------------------------------------------------------#

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

  #---------------------------------------------------------------------------#
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

  #---------------------------------------------------------------------------#

  #b) Update genenames
  genenames = rownames(x);

  #---------------------------------------------------------------------------#

  #Update arguments
  args[['x']]                = x;
  args[['genenames']]        = genenames;
  args[['resp.type']]        = sam.resp.type;
  args[['censoring.status']] = censoring.status;
  args[['all.genes']]        = TRUE;
  args[['fdr.output']]       = 1;

  rm(x)

  #---------------------------------------------------------------------#
  #Significance analysis of microarrays

  if(identical(resp.type, "mgaussian")){
    out = apply(X = y, MARGIN=2, FUN = function(z){
      do.call(what = sam, args = c(args, list(y = z)))
    });

  } else {
    out = do.call(what = sam, args = c(args, list(y = y)));
  }

  #---------------------------------------------------------------------------#

  return(out);
}


#'Extract q-value from SAM
#'@param obj object as returned by \code{\link{sam}}
#'@param feat.order (optional) order of output features
#'@return q-value
#'@keywords internal
extract_qval_sam_default = function(obj, feat.order = NULL){

  #Extract features with positive and negative correlation with the outcome
  genes.up = obj$sam.res$siggenes.table$genes.up;
  genes.lo = obj$sam.res$siggenes.table$genes.lo;

  #Bind the lists
  genes.res = rbind(genes.up, genes.lo)

  #Select the q-value column
  index = match(x = "q-value(%)", table = colnames(genes.res))

  #Get a named vector with q-values
  genes.p = as.numeric(genes.res[,index])/100;#divide by 100 to have a range [0,1]
  names(genes.p) = genes.res[,1]

  if(!missing(feat.order) & !is.null(feat.order)){
    #Sort the vector so that features have the same
    #order as in x
    genes.p = genes.p[feat.order]
  }


  return(genes.p)
}

#'Extract q-value from SAM
#'@param obj object as returned by \code{\link{compute_sam}}
#'@param feat.order (optional) order of output features
#'@return q-value
#'@keywords internal
extract_qval_sam = function(obj,
                            feat.order = NULL,
                            resp.type,
                            coef = NULL){


  if(identical(resp.type, "mgaussian")){
    out = lapply(X = obj, FUN = function(x, feat.order){

      extract_qval_sam_default(obj = x, feat.order = feat.order);

    }, feat.order=feat.order);

    #bind
    out = do.call(what = cbind, args = out)

    if(!is.null(coef)){
      out = out[,coef];
    }
  } else {
    out = extract_qval_sam_default(obj = obj, feat.order = feat.order);
  }

  return(out)
}


# extract_qval_sam = function(obj,
#                             feat.order = NULL,
#                             resp.type,
#                             coef = NULL){
#
#
#   if(identical(resp.type, "mgaussian")){
#     out = lapply(X = obj, FUN = function(x, feat.order){
#
#       extract_qval_sam_default(obj = x, feat.order = feat.order);
#
#     }, feat.order=feat.order);
#
#     #bind
#     out = do.call(what = cbind, args = out)
#
#     if(!is.null(coef)){
#       out = out[,coef];
#     } else {
#       #If no coefficients has been pre-selected, get the least significant value
#       #(i.e. max p-value) across coefficients
#       out = apply(X = out, MARGIN = 1, FUN = max)
#     }
#   } else {
#     out = extract_qval_sam_default(obj = obj, feat.order = feat.order);
#   }
#
#   return(out)
# }















#'Compute significant genes table (with one bug corrected)
#'@description Computes significant genes table, starting with samr object "samr.obj" and delta.table "delta.table"
#'Modified from \code{\link[samr]{samr.compute.siggenes.table}} to correct a bug
#'@inheritParams samr::samr.compute.siggenes.table
#'@keywords internal
samr.compute.siggenes.table.mod = function (samr.obj, del, data, delta.table, min.foldchange = 0,
                                            all.genes = FALSE, compute.localfdr = FALSE)
{
  if (is.null(data$geneid)) {
    data$geneid = paste("g", 1:nrow(data$x), sep = "")
  }
  if (is.null(data$genenames)) {
    data$genenames = paste("g", 1:nrow(data$x), sep = "")
  }
  if (!all.genes) {
    sig = samr:::detec.slab(samr.obj, del, min.foldchange)
  }
  if (all.genes) {
    p = length(samr.obj$tt)
    pup = (1:p)[samr.obj$tt >= 0]
    plo = (1:p)[samr.obj$tt < 0]
    sig = list(pup = pup, plo = plo)
  }
  if (compute.localfdr) {
    aa = samr:::localfdr(samr.obj, min.foldchange)
    if (length(sig$pup) > 0) {
      fdr.up = samr:::predictlocalfdr(aa$smooth.object, samr.obj$tt[sig$pup])
    }
    if (length(sig$plo) > 0) {
      fdr.lo = samr:::predictlocalfdr(aa$smooth.object, samr.obj$tt[sig$plo])
    }
  }
  qvalues = NULL
  if (length(sig$pup) > 0 | length(sig$plo) > 0) {
    qvalues = samr:::qvalue.func(samr.obj, sig, delta.table)
  }
  res.up = NULL
  res.lo = NULL
  done = FALSE
  if ((samr.obj$resp.type == samr::samr.const.twoclass.unpaired.response |
       samr.obj$resp.type == samr::samr.const.twoclass.paired.response)) {
    if (!is.null(sig$pup)) {
      res.up = cbind(sig$pup + 1, data$genenames[sig$pup],
                     data$geneid[sig$pup], samr.obj$tt[sig$pup],
                     samr.obj$numer[sig$pup], samr.obj$sd[sig$pup],
                     samr.obj$foldchange[sig$pup], qvalues$qvalue.up)
      if (compute.localfdr) {
        res.up = cbind(res.up, fdr.up)
      }
      temp.names = list(NULL, c("Row", "Gene ID", "Gene Name",
                                "Score(d)", "Numerator(r)", "Denominator(s+s0)",
                                "Fold Change", "q-value(%)"))
      if (compute.localfdr) {
        temp.names[[2]] = c(temp.names[[2]], "localfdr(%)")
      }
      dimnames(res.up) = temp.names
    }
    if (!is.null(sig$plo)) {
      res.lo = cbind(sig$plo + 1, data$genenames[sig$plo],
                     data$geneid[sig$plo], samr.obj$tt[sig$plo],
                     samr.obj$numer[sig$plo], samr.obj$sd[sig$plo],
                     samr.obj$foldchange[sig$plo], qvalues$qvalue.lo)
      if (compute.localfdr) {
        res.lo = cbind(res.lo, fdr.lo)
      }
      temp.names = list(NULL, c("Row", "Gene ID", "Gene Name",
                                "Score(d)", "Numerator(r)", "Denominator(s+s0)",
                                "Fold Change", "q-value(%)"))
      if (compute.localfdr) {
        temp.names[[2]] = c(temp.names[[2]], "localfdr(%)")
      }
      dimnames(res.lo) = temp.names
    }
    done = TRUE
  }
  if (samr.obj$resp.type == samr::samr.const.multiclass.response) {
    if (!is.null(sig$pup)) {
      res.up = cbind(sig$pup + 1, data$genenames[sig$pup],
                     data$geneid[sig$pup], samr.obj$tt[sig$pup],
                     samr.obj$numer[sig$pup], samr.obj$sd[sig$pup],
                     samr.obj$stand.contrasts[sig$pup, ,drop=F], qvalues$qvalue.up)
      if (compute.localfdr) {
        res.up = cbind(res.up, fdr.up)
      }
      collabs.contrast = paste("contrast-", as.character(1:ncol(samr.obj$stand.contrasts)),
                               sep = "")
      temp.names = list(NULL, c("Row", "Gene ID", "Gene Name",
                                "Score(d)", "Numerator(r)", "Denominator(s+s0)",
                                collabs.contrast, "q-value(%)"))
      if (compute.localfdr) {
        temp.names[[2]] = c(temp.names[[2]], "localfdr(%)")
      }
      dimnames(res.up) = temp.names
    }
    res.lo = NULL
    done = TRUE
  }
  if (!done) {
    if (!is.null(sig$pup)) {
      res.up = cbind(sig$pup + 1, data$genenames[sig$pup],
                     data$geneid[sig$pup], samr.obj$tt[sig$pup],
                     samr.obj$numer[sig$pup], samr.obj$sd[sig$pup],
                     samr.obj$foldchange[sig$pup], qvalues$qvalue.up)
      if (compute.localfdr) {
        res.up = cbind(res.up, fdr.up)
      }
      temp.names = list(NULL, c("Row", "Gene ID", "Gene Name",
                                "Score(d)", "Numerator(r)", "Denominator(s+s0)",
                                "q-value(%)"))
      if (compute.localfdr) {
        temp.names[[2]] = c(temp.names[[2]], "localfdr(%)")
      }
      dimnames(res.up) = temp.names
    }
    if (!is.null(sig$plo)) {
      res.lo = cbind(sig$plo + 1, data$genenames[sig$plo],
                     data$geneid[sig$plo], samr.obj$tt[sig$plo],
                     samr.obj$numer[sig$plo], samr.obj$sd[sig$plo],
                     samr.obj$foldchange[sig$plo], qvalues$qvalue.lo)
      if (compute.localfdr) {
        res.lo = cbind(res.lo, fdr.lo)
      }
      temp.names = list(NULL, c("Row", "Gene ID", "Gene Name",
                                "Score(d)", "Numerator(r)", "Denominator(s+s0)",
                                "q-value(%)"))
      if (compute.localfdr) {
        temp.names[[2]] = c(temp.names[[2]], "localfdr(%)")
      }
      dimnames(res.lo) = temp.names
    }
    done = TRUE
  }
  if (!is.null(res.up)) {
    o1 = order(-samr.obj$tt[sig$pup])
    res.up = res.up[o1, , drop = F]
  }
  if (!is.null(res.lo)) {
    o2 = order(samr.obj$tt[sig$plo])
    res.lo = res.lo[o2, , drop = F]
  }
  color.ind.for.multi = NULL
  if (samr.obj$resp.type == samr::samr.const.multiclass.response &
      !is.null(sig$pup)) {
    color.ind.for.multi = 1 * (samr.obj$stand.contrasts[sig$pup,] > samr.obj$stand.contrasts.95[2]) +
      (-1) * (samr.obj$stand.contrasts[sig$pup,] < samr.obj$stand.contrasts.95[1])
  }
  ngenes.up = nrow(res.up)
  if (is.null(ngenes.up)) {
    ngenes.up = 0
  }
  ngenes.lo = nrow(res.lo)
  if (is.null(ngenes.lo)) {
    ngenes.lo = 0
  }
  return(list(genes.up = res.up, genes.lo = res.lo, color.ind.for.multi = color.ind.for.multi,
              ngenes.up = ngenes.up, ngenes.lo = ngenes.lo))
}





#'Significance analysis of microarrays - simple user interface
#'@description Modified from \code{\link[samr]{SAM}} to solve a bug.
#'Correlates a large number of features (eg genes) with an outcome variable,
#'such as a group indicator, quantitative variable or survival time.
#'This is a simple user interface for the samr function applied to array data.
#'For sequencing data applications, see the function SAMseq
#'@inheritParams samr::SAM
#'@param all.genes Should all genes be listed? Default FALSE
#'@keywords internal
SAM.mod <- function (x, y = NULL, censoring.status = NULL,
                     resp.type = c("Quantitative",
                                   "Two class unpaired", "Survival", "Multiclass", "One class",
                                   "Two class paired", "Two class unpaired timecourse", "One class timecourse",
                                   "Two class paired timecourse", "Pattern discovery"), geneid = NULL,
                     genenames = NULL, s0 = NULL, s0.perc = NULL, nperms = 100,
                     center.arrays = FALSE, testStatistic = c("standard", "wilcoxon"),
                     time.summary.type = c("slope", "signed.area"),
                     regression.method = c("standard", "ranks"),
                     return.x = TRUE, knn.neighbors = 10, random.seed = NULL,
                     logged2 = FALSE, fdr.output = 0.2,
                     eigengene.number = 1,
                     all.genes = FALSE)
{
  this.call <- match.call()
  xl.mode = "regular"
  xl.time = NULL
  xl.prevfit = NULL
  if (fdr.output < 0 | fdr.output > 1) {
    stop("Error: fdr.output must be between 0 and 1")
  }
  if (is.null(geneid)) {
    geneid = as.character(1:nrow(x))
  }
  if (is.null(genenames)) {
    genenames = paste("g", as.character(1:nrow(x)), sep = "")
  }
  data = list(x = x, y = y, censoring.status = censoring.status,
              geneid = geneid, genenames = genenames, logged2 = logged2,
              eigengene.number = eigengene.number)
  samr.obj = samr::samr(data, resp.type = resp.type, assay.type = "array",
                        s0 = s0, s0.perc = s0.perc, nperms = nperms, center.arrays = center.arrays,
                        testStatistic = testStatistic, time.summary.type = time.summary.type,
                        regression.method = regression.method, return.x = return.x,
                        knn.neighbors = knn.neighbors, random.seed = random.seed)
  delta.table <- samr::samr.compute.delta.table(samr.obj)
  siggenes.table <- del <- NULL
  delta.table <- delta.table[delta.table[, "# called"] > 0,
                             , drop = FALSE]
  if (nrow(delta.table) > 0) {
    oo <- which(delta.table[, "median FDR"] >= fdr.output)
    if (length(oo) > 0) {
      oo <- oo[length(oo)]
    }
    else {
      oo <- 1
    }
    delta.table <- delta.table[oo:nrow(delta.table), , drop = FALSE]
    del <- delta.table[1, "delta"]
    siggenes.table <- samr.compute.siggenes.table.mod(samr.obj = samr.obj,
                                                      del = del,
                                                      data = data,
                                                      delta.table = delta.table,
                                                      all.genes = all.genes)
    rang = 4:8
    if (resp.type == "Multiclass") {
      nclass = length(table(y))
      rang = 3:(ncol(siggenes.table$genes.up))
    }
    if (resp.type == "Quantitative" | resp.type == "Pattern discovery" |
        resp.type == "Survival") {
      rang = 4:7
    }
    siggenes.table$genes.up[, rang] = round(as.numeric(siggenes.table$genes.up[, rang]), 3)
    siggenes.table$genes.lo[, rang] = round(as.numeric(siggenes.table$genes.lo[, rang]), 3)
    siggenes.table$genes.up = siggenes.table$genes.up[, -1]
    siggenes.table$genes.lo = siggenes.table$genes.lo[, -1]
  }
  out = list(samr.obj = samr.obj, del = del, delta.table = delta.table,
             siggenes.table = siggenes.table)
  out$call = this.call
  class(out) = "SAMoutput"
  return(out)
}
