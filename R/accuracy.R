#'@keywords internal
roc <- function(true, pred){
  precrec::evalmod(scores = pred, labels = true)
  #toplot
  #ggplot2::autoplot()
  ggplot2::ggplot(mapping = ggplot2::aes(d = true, m = pred)) + plotROC::geom_roc(n.cuts = 8) + plotROC::style_roc(theme = ggplot2::theme_grey, xlab = "1 - Specificity", ylab = "Sensitivity")
}






#'@keywords internal
binomial_deviance <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw")){

  prob_min = 1e-05
  prob_max = 1 - prob_min

  #get N
  N = if(!is.null(nrow(true))){nrow(true)} else {length(true)}

  #check weights
  if (missing(weights)) {weights = rep(1, N)}

  pred = 1/(1 + exp(-pred))

  ywt = apply(true, 1, sum)
  true = true/ywt
  weights = weights * ywt

  pred = pmin(pmax(pred, prob_min), prob_max)
  lp = true[, 1] * log(1 - pred) + true[, 2] * log(pred)
  ly = log(true)
  ly[true == 0] = 0
  ly = drop((true * ly) %*% c(1, 1))
  acc.raw = 2 * (ly - lp)

  #If multi-response, sum the residues
  if(is.multi){
    if(identical(multi, "raw")){
      #Apply the weighted mean
      acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)#to use when multi-config
    }else{
      if(identical(multi, "sum")){
        #sum
        acc.raw = rowSums(x = acc.raw, na.rm = T)
      } else {
        #average
        acc.raw = rowMeans(x = acc.raw, na.rm = T)
      }
      #Apply the weighted mean
      acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
    }
  } else {
    #Apply the weighted mean
    acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
  }

  #return
  return(acc)

}
#'@keywords internal
multinomial_deviance <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw")){
  prob_min = 1e-05
  prob_max = 1 - prob_min

  ywt = apply(true, 1, sum)
  true = true/ywt
  weights = weights * ywt

  pred = exp(pred)

  predtot = apply(pred, 2, sum)
  pred = pred/predtot

  pred = pmin(pmax(pred, prob_min), prob_max)

  lp = true * log(pred)
  ly = true * log(true)
  ly[true == 0] = 0
  acc.raw = 2 * (ly - lp)

  #If multi-response, sum the residues
  if(is.multi){
    if(identical(multi, "raw")){
      #Apply the weighted mean
      acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)#to use when multi-config
    }else{
      if(identical(multi, "sum")){
        #sum
        acc.raw = rowSums(x = acc.raw, na.rm = T)
      } else {
        #average
        acc.raw = rowMeans(x = acc.raw, na.rm = T)
      }
      #Apply the weighted mean
      acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
    }
  } else {
    #Apply the weighted mean
    acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
  }

  #return
  return(acc)
}
#'@keywords internal
cox_deviance <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw")){}
#'@keywords internal
poisson_deviance <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw")){}
#'@keywords internal
deviance <- function(resp.type, true, pred, weights = NULL, multi = c("average", "sum", "raw")){

  #select function
  fun = switch(
    resp.type,
    "gaussian"    = mean_squared_error,
    "mgaussian"   = mean_squared_error,
    "binomial"    = binomial_deviance,
    "multinomial" = multinomial_deviance,
    "cox"         = cox_deviance,
    "poisson"     = poisson_deviance
  )

  #call
  out = do.call(what = fun, args = list(true = true, pred = pred, weights = weights, multi = multi))

  #return
  return(out)
}

#'@keywords internal
check_confusion_matrix <- function(x){

  #Check the order
  if(identical(x = which.max(x = dim(x)), y = 1)){
    all_labels = rownames(x)
    subset_labels = colnames(x)
  }else{
    all_labels = colnames(x)
    subset_labels = rownames(x)
  }

  missing_labels = setdiff(all_labels, subset_labels)

  newx$`1` = 0

}

#'Check the accuracy measure
#'@description Check if the selected accuracy measure is supported for the considered
#'response type.
#'@param type.measure the accuracy measure
#'@param resp.type the response type
#'@return Character string, the checked accuracy measure.
#'@keywords internal
#'@author Alessandro Barberis
check_type_measure = function (
  type.measure = "mse",
  resp.type = "gaussian")
{

  type.measure = type.measure[1]

  #Accuracy measures
  type.measures = c("mse", "mdev", "mclass", "auc", "mae", "C", "rmse", "mape", "r2", "msle",
                    "squared_error", "deviance", "class", "absolute_error", "ape", "sle")

  #Deviance name
  typenames = unlist(sapply(X = type.measures, FUN = get_measure_name, resp.type = resp.type, USE.NAMES = F))

  #supported measures
  subclass.ch = switch(
    resp.type,
    gaussian    = c(1, 2, 5, 7, 8, 9, 10, 11, 12, 14, 15, 16),
    binomial    = c(2, 3, 4, 1, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
    poisson     = c(2, 1, 5),
    cox         = c(2, 6),
    multinomial = c(2, 3, 1, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
    mgaussian   = c(1, 2, 5, 7, 8, 9, 10, 11, 12, 14, 15, 16)
  )

  #subset names
  subclass.type = type.measures[subclass.ch]

  if (type.measure == "default") type.measure = subclass.type[1]

  #model name
  model.name = get_model_name(resp.type)

  if (!match(type.measure, subclass.type, FALSE)) {
    type.measure = subclass.type[1]
    warning(paste("Only ", paste(subclass.type, collapse = ", "),
                  " available as type.measure for ", model.name, " models; ",
                  type.measure, " used instead", sep = ""), call. = FALSE)
  }
  names(type.measure) = typenames[type.measure]
  return(type.measure)
}


#'Available accuracy measures
#'@description Get the available performance measures for the selected response type.
#'@param resp.type the response type
#'@return Character vector containing the available accuracy measures for the selected \code{resp.type}.
#'@keywords internal
#'@author Alessandro Barberis
get_available_measures <- function(resp.type){

  #available measures
  type.measures = c("mse", "mdev", "mclass", "auc", "mae", "C", "rmse", "mape", "r2", "msle",
                    "squared_error", "deviance", "class", "absolute_error", "ape", "sle",
                    "f1s", "acc", "precision", "sensitivity", "jaccard",
                    "true_pred", "false_pred")

  if(!missing(resp.type)){
    #match measure with response type
    subclass = switch(
      resp.type,
      gaussian    = c(1, 2, 5, 7, 8, 9, 10, 11, 12, 14, 15, 16),
      binomial    = c(2, 3, 4, 1, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
      poisson     = c(2, 1, 5),
      cox         = c(2, 6),
      multinomial = c(2, 3, 1, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
      mgaussian   = c(1, 2, 5, 7, 8, 9, 10, 11, 12, 14, 15, 16)
    )

    #subset
    out = type.measures[subclass]

    #get names
    measure.names = unlist(sapply(X = out, FUN = get_measure_name, resp.type = resp.type, USE.NAMES = F))

    #set names
    names(out) = measure.names
  }

  #return
  return(out)
}

#'@keywords internal
get_available_summary_statistics <- function(resp.type){

  #available measures
  type.measures = c("mse", "mdev", "class", "auc", "mae", "C", "rmse", "mape", "r2", "msle",
                    "f1s", "acc", "precision", "sensitivity", "jaccard")

  if(!missing(resp.type)){
    #match measure with response type
    subclass = switch(
      resp.type,
      gaussian    = c(1, 2, 5, 7, 8, 9, 10),
      binomial    = c(2, 3, 4, 1, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15),
      poisson     = c(2, 1, 5),
      cox         = c(2, 6),
      multinomial = c(2, 3, 1, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15),
      mgaussian   = c(1, 2, 5, 7, 8, 9, 10)
    )

    #subset
    out = type.measures[subclass]

    #get names
    measure.names = unlist(sapply(X = out, FUN = get_measure_name, resp.type = resp.type, USE.NAMES = F))

    #set names
    names(out) = measure.names
  }

  #return
  return(out)
}


#'Accuracy measure name
#'@description Get the accuracy measure name given the response type.
#'@param type.measure the accuracy measure
#'@param resp.type the response type
#'@return Character string containing the accuracy measure name for the selected \code{resp.type}.
#'@keywords internal
#'@author Alessandro Barberis
get_measure_name <- function(
  type.measure = c("mse", "mdev", "mclass", "auc", "mae", "C", "rmse", "mape", "r2", "msle",
                   "squared_error", "deviance", "class", "absolute_error", "ape", "sle",
                   "f1s", "acc", "precision", "sensitivity", "jaccard"),
  resp.type = NULL
  ){

  type.measure = match.arg(type.measure)



  devname = switch(resp.type,
                   gaussian    = "Squared Error",
                   binomial    = "Binomial Deviance",
                   poisson     = "Poisson Deviance",
                   cox         = "Partial Likelihood Deviance",
                   multinomial = "Multinomial Deviance",
                   mgaussian   = "Squared Error",
                   "Deviance")

  mdevname = switch(resp.type,
                   gaussian    = "Mean-squared Error",
                   binomial    = "Mean Binomial Deviance",
                   poisson     = "Mean Poisson Deviance",
                   cox         = "Partial Likelihood Deviance",
                   multinomial = "Mean Multinomial Deviance",
                   mgaussian   = "Mean-squared Error",
                   "Deviance")

  typenames = c(
    deviance = devname,
    mdev  = mdevname,
    mse   = "Mean-squared Error",
    mae   = "Mean Absolute Error",
    auc   = "AUC",
    class = "Misclassification Error",
    C     = "C-index",
    rmse  = "Root-mean-square Error",
    mape  = "Mean Absolute Percentage Error",
    r2    = "R2",
    msle  = "Mean-squared Logarithmic Error",
    squared_error = "Squared Error",
    absolute_error = "Absolute Error",
    ape   = "Absolute Percentage Error",
    sle   = "Squared Logarithmic Error",
    prevalence           = "Prevalence",
    ACC                  = "Accuracy",
    BA                   = "Balanced Accuracy",
    F1                   = "F1 Score",
    PPV                  = "Positive Predictive Value",
    NPV                  = "Negative Predictive Value",
    FDR                  = "False Discovery Rate",
    FOR                  = "False Omission Rate",
    TPR                  = "Sensitivity",#TPR = "True Positive Rate"
    FNR                  = "False Negative Rate",#miss rate
    TNR                  = "Specificity",#True Negative Rate, selectivity
    LRpos                = "Positive likelihood ratio",
    LRneg                = "Negative likelihood ratio",
    DOR                  = "Diagnostic Odds Ratio",
    TS                   = "Threat Score", #Jaccard index, Critical Success Index
    MCC                  = "Matthews Correlation Coefficient",
    MK                   = "Markedness",#deltaP
    FM                   = "Fowlkes-Mallows Index",
    BM                   = "Bookmaker Informedness",#Informedness
    PT                   = "Prevalence Threshold",
    f1s                  = "F1 Score",
    acc                  = "Accuracy",
    precision            = "Precision",
    sensitivity          = "Sensitivity",
    jaccard              = "Jaccard Index"
  )

  out = typenames[type.measure]

  return(out)

}
#'@keywords internal
get_measure_names <- function(
  type.measure = c("mse", "mdev", "mclass", "auc", "mae", "C", "rmse", "mape", "r2", "msle",
                   "squared_error", "deviance", "class", "absolute_error", "ape", "sle")
){

  # if(type.measure %in% supported_scoring_methods()){
    typenames = c(
      binomial_deviance    = "Binomial Deviance",
      multinomial_deviance = "Multinomial Deviance",
      poisson_deviance     = "Poisson Deviance",
      cox_deviance         = "Partial Likelihood Deviance",
      gaussian_deviance    = "Mean-squared Error",
      mgaussian_deviance   = "Mean-squared Error",
      mse                  = "Mean-squared Error",
      mae                  = "Mean Absolute Error",
      auc                  = "AUC",
      class                = "Misclassification Error",
      C                    = "C-index",
      rmse                 = "Root-mean-square Error",
      mape                 = "Mean Absolute Percentage Error",
      r2                   = "R2",
      msle                 = "Mean-squared Logarithmic Error",
      squared_error        = "Squared Error",
      absolute_error       = "Absolute Error",
      ape                  = "Absolute Percentage Error",
      sle                  = "Squared Logarithmic Error",
      prevalence           = "Prevalence",
      ACC                  = "Accuracy",
      ERR                  = "Classification Error Rate",
      # BA                   = "Balanced Accuracy",
      BAS                  = "Balanced Accuracy",
      F1                   = "F1 Score",
      F1S                  = "F1 Score",
      FBS                  = "F-Score",
      PPV                  = "Positive Predictive Value",
      NPV                  = "Negative Predictive Value",
      FDR                  = "False Discovery Rate",
      FOR                  = "False Omission Rate",
      TPR                  = "Sensitivity",#TPR = "True Positive Rate"
      SEN                  = "Sensitivity",#TPR = "True Positive Rate"
      FPR                  = "False Positive Rate",
      FNR                  = "False Negative Rate",#miss rate
      TNR                  = "Specificity",#True Negative Rate, selectivity
      # LRpos                = "Positive likelihood ratio",
      # LRneg                = "Negative likelihood ratio",
      PLR                  = "Positive likelihood ratio",
      NLR                  = "Negative likelihood ratio",
      DOR                  = "Diagnostic Odds Ratio",
      TS                   = "Threat Score", #Jaccard index, Critical Success Index
      CSI                  = "Critical Success Index", #Jaccard index,
      MCC                  = "Matthews Correlation Coefficient",
      # MK                   = "Markedness",#deltaP
      MKD                  = "Markedness",#deltaP
      # FM                   = "Fowlkes-Mallows Index",
      FMI                  = "Fowlkes-Mallows Index",
      # BM                   = "Bookmaker Informedness",#Informedness
      BMI                  = "Bookmaker Informedness",#Informedness
      PRV                  = "Prevalence",
      PRT                  = "Prevalence Threshold",
      # PT                   = "Prevalence Threshold",
      f1s                  = "F1 Score",
      acc                  = "Accuracy",
      precision            = "Precision",
      sensitivity          = "Sensitivity",
      jaccard              = "Jaccard Index"
    )

    out = typenames[type.measure]
  # } else {
  #   stop("Measure not supported. Please, check provided 'type.measure'.\n")
  # }

  return(out)

}

#'Model name
#'@description Get the model name
#'@param resp.type the response type
#'@return Character string containing the model name.
#'@keywords internal
#'@author Alessandro Barberis
get_model_name <- function(resp.type){

  #get name
  model.name = switch(resp.type,
                      gaussian    = "Gaussian",
                      binomial    = "Binomial",
                      poisson     = "Poisson",
                      cox         = "Cox",
                      multinomial = "Multinomial",
                      mgaussian   = "Multi-response Gaussian")

  return(model.name)
}


# #'Accuracy measure for the selected model family
# #'@description This is a generic function for computing the accuracy of the results
# #'of various model fitting functions.
# #'@param truth Response test variable
# #'@param pred Response predicted variable
# #'@param resp.type response type
# #'@inheritParams glmnet::cv.glmnet
# #'@param ...  further arguments to accuracy function
# #'@return The accuracy measure
# #'@keywords internal
# accuracy <- function(truth, pred,
#                      method = "glmnet",
#                      resp.type = c("gaussian", "binomial", "multinomial"),
#                      type.measure = c("deviance", "mse", "mae", "auc", "class"),
#                      ...){
#   #Get the list of parameters
#   args <- c(as.list(environment()), list(...));
#
#   #Remove the method
#   args["method"] = NULL;
#
#   #Call the appropriate method
#   params = switch(method,
#                   glmnet = do.call(what = accuracy.glmnet, args = c(args)))
#
#   #return
#   return(params);
# }




#Code from glmnet
#Check the accuracy measure
#'@keywords internal
check.type.measure = function (type.measure = "mse", resp.type = "gaussian")
{
  type.measures = c("mse", "deviance", "class", "auc", "mae", "C")

  subclass = switch(resp.type,
                    gaussian    = "elnet",
                    mgaussian   = "mrelnet",
                    binomial    = "lognet",
                    multinomial = "multnet",
                    cox         = "coxnet",
                    poisson     = "fishnet")

  devname = switch(subclass,
                   elnet = "Mean-squared Error",
                   lognet = "Binomial Deviance",
                   fishnet = "Poisson Deviance",
                   coxnet = "Partial Likelihood Deviance",
                   multnet = "Multinomial Deviance",
                   mrelnet = "Mean-squared Error")
  typenames = c(deviance = devname,
                mse = "Mean-squared Error",
                mae = "Mean Absolute Error",
                auc = "AUC",
                class = "Misclassification Error",
                C = "C-index")
  subclass.ch = switch(subclass,
                       elnet = c(1, 2, 5),
                       lognet = c(2, 3, 4, 1, 5),
                       fishnet = c(2, 1, 5),
                       coxnet = c(2, 6),
                       multnet = c(2, 3, 1, 5),
                       mrelnet = c(1, 2, 5))

  subclass.type = type.measures[subclass.ch]

  if (type.measure == "default") type.measure = subclass.type[1]

  model.name = switch(subclass,
                      elnet = "Gaussian",
                      lognet = "Binomial",
                      fishnet = "Poisson",
                      coxnet = "Cox",
                      multnet = "Multinomial",
                      mrelnet = "Multi-response Gaussian")

  if (!match(type.measure, subclass.type, FALSE)) {
    type.measure = subclass.type[1]
    warning(paste("Only ", paste(subclass.type, collapse = ", "),
                  " available as type.measure for ", model.name, " models; ",
                  type.measure, " used instead", sep = ""), call. = FALSE)
  }
  names(type.measure) = typenames[type.measure]
  return(type.measure)
}
