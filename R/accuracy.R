
get_min_as_opt <- function(error, type.measure){

  measures = c("C", "c", "auc", "r2", "acc", "f1s", "jaccard",
               "sensitivity",
               "precision",
               "specificity")

  if(type.measure %in% measures){
    error = 1 - error
  }

  return(error)
}

# get_pred_type <- function(
#   type.measure = c("mse", "deviance", "class", "auc", "mae", "C", "rmse", "mape", "r2", "msle"),
#   resp.type){
#
#   deviance = switch(
#     resp.type,
#     'cox' = "coefficients",
#     'link'
#   )
#
#   out = switch(
#     type.measure,
#     'mse'      = 'link',
#     'msle'     = 'link',
#     'rmse'     = 'link',
#     'mae'      = 'link',
#     'mape'     = 'link',
#     'r2'       = 'link',
#     'deviance' = deviance,
#     'class'    = 'class',
#     'C'        = 'link',
#     'auc'      = 'link',
#     'acc'         = 'class',
#     'f1s'         = 'class',
#     'precision'   = 'class',
#     'sensitivity' = 'class',
#     'jaccard'     = 'class'
#   )
#
#   return(out)
# }

#Get scorer
#@description Get a scorer function producing the desired measure for the selected response type
#@param type.measure the accuracy measure
#@param resp.type the respnse type
#@return a scorer function
#@author Alessandro Barberis
# get_scorer_function <- function(
#   type.measure = c("mse", "deviance", "class", "auc", "mae", "C", "rmse", "mape", "r2", "msle"),
#   resp.type){
#
#   #set misclassification
#   misclassification_error = switch(
#     resp.type,
#     binomial = binomial_misclassification,
#     multinomial = multinomial_misclassification,
#     binomial_misclassification
#   )
#
#   #set function
#   fun = switch(
#     type.measure,
#     'mse'      = mean_squared_error,
#     'msle'     = mean_squared_log_error,
#     'rmse'     = root_mean_squared_error,
#     'mae'      = mean_absolute_error,
#     'mape'     = mean_absolute_percentage_error,
#     'r2'       = r2_score,
#     'deviance' = deviance,
#     'class'    = misclassification_error,
#     'C'        = c_index,
#     'auc'      = area_under_curve,
#
#     'squared_error' = squared_error,
#     'absolute_error'= absolute_error,
#     'ape'           = absolute_percentage_error,
#     'sle'           = squared_log_error
#   )
#
#   return(fun)
# }

# get_score_function <- function(metric = c("mse", "deviance", "class", "auc", "mae", "C", "rmse", "mape", "r2", "msle")){
#   #set function
#   fun = switch(
#     metric,
#     'mse'      = mean_squared_error,
#     'msle'     = mean_squared_log_error,
#     'rmse'     = root_mean_squared_error,
#     'mape'     = mean_absolute_percentage_error,
#     'r2'       = r2_score,
#     'deviance' = deviance,
#     'class'    = misclassification_error,
#     'C'        = c_index,
#     'auc'      = area_under_curve
#   )
#
#   return(fun)
# }

# score <- function(
#   true, pred, weights = NULL, multi = c("average", "sum", "raw"),
#   metric = c("mse", "deviance", "class", "auc", "mae", "C", "rmse", "mape", "r2", "msle"),
#   resp.type
#   ){
#
#   #set function
#   fun = switch(
#     metric,
#     'mse'      = mean_squared_error,
#     'msle'     = mean_squared_log_error,
#     'rmse'     = root_mean_squared_error,
#     'mape'     = mean_absolute_percentage_error,
#     'r2'       = r2_score,
#     'deviance' = deviance,
#     'class'    = misclassification_error,
#     'C'        = c_index,
#     'auc'      = area_under_curve
#   )
#
#   #compute score
#   out = do.call(what = fun, args = list(true = true, pred = pred, weights = weights, multi = multi))
#
#   #return
#   return(out)
# }













tot_sum_squares <- function(true, pred){

}




roc <- function(true, pred){
  precrec::evalmod(scores = pred, labels = true)
  #toplot
  #ggplot2::autoplot()
  ggplot2::ggplot(mapping = ggplot2::aes(d = true, m = pred)) + plotROC::geom_roc(n.cuts = 8) + plotROC::style_roc(theme = ggplot2::theme_grey, xlab = "1 - Specificity", ylab = "Sensitivity")
}









# jaccard_score_raw <- function(true, pred, confusion = NULL, weights = NULL, multi = c("average", "sum", "raw"))


#Jaccard Score
#@description The best possible score is one.
#@param confusion confusion matrix
#@param positive index of 'positive' class
# jaccard_score <- function(true, pred, confusion = NULL, weights = NULL, positive = 1){
#
#   #--------------------------------------------------------------------------------------------#
#   #Check positive
#   if(!(is.integer(positive) & isTRUE(positive > 0) & isTRUE(positive <= nc))){
#     positive = match(x = positive, table = classes)
#     if(is.na(positive)){stop("\nProvided argument 'positive' not valid. Please, check your input.\n")}
#   }
#
#   #--------------------------------------------------------------------------------------------#
#   #set positive class
#   pos_class = classes[positive]
#   neg_class = classes[-positive]
#
#   #--------------------------------------------------------------------------------------------#
#   #Extract info
#   #--------------------------------------------------------------------------------------------#
#   #Class
#   TP = confusion[pos_class, pos_class]
#   # TN = sum(confusion[neg_class, neg_class])
#   FN = sum(confusion[pos_class, neg_class])
#   FP = sum(confusion[neg_class, pos_class])
#
#   out = TP / (TP + FN + FP)
#
#   #--------------------------------------------------------------------------------------------#
#   return(out)
# }







# misclassification <- function(true, pred, ...){return(as.integer(true != pred))}
# classification <- function(true, pred, ...){as.integer(true == pred)}





#'Jaccard Score
threat_score = critical_success_index = jaccard_index <- function(
  true, pred, weights = NULL, multi, ...
){
  #Compute score
  out = multiresponse_classification_metric(true = true, pred = pred, weights = weights, multi = multi, metric = "CSI", ...)
  #return
  return(out)
}










# false_positive_rate <- function(TP, TN, FP, FN){out = FP / (FP + TN);return(out)}
# true_negative_rate  <- function(TP, TN, FP, FN){out = TN / (FP + TN);return(out)}
# specificity <- true_negative_rate
# false_negative_rate <- function(TP, TN, FP, FN){out = FN / (TP + FN);return(out)}
# true_positive_rate  <- function(TP, TN, FP, FN){out = TP / (TP + FN);return(out)}
# false_discovery_rate  <- function(TP, TN, FP, FN){out = FP / (TP + FP);return(out)}
#
# positive_predictive_value <- function(TP, TN, FP, FN){out = TP / (TP + FP);return(out)}
# false_omission_rate <- function(TP, TN, FP, FN){out = FN / (FN + TN);return(out)}
# negative_predictive_value <- function(TP, TN, FP, FN){out = TN / (FN + TN);return(out)}
#
# positive_likelihood_ratio <- function(TP, TN, FP, FN){out = true_positive_rate(TP, TN, FP, FN) / false_positive_rate(TP, TN, FP, FN);return(out)}
# negative_likelihood_ratio <- function(TP, TN, FP, FN){out = false_negative_rate(TP, TN, FP, FN) / true_negative_rate(TP, TN, FP, FN);return(out)}
#
# markedness  <- function(TP, TN, FP, FN){out = positive_predictive_value(TP, TN, FP, FN) + negative_predictive_value(TP, TN, FP, FN) - 1;return(out)}
# diagnostic_odds_ratio  <- function(TP, TN, FP, FN){out = positive_likelihood_ratio(TP, TN, FP, FN) / negative_likelihood_ratio(TP, TN, FP, FN);return(out)}
#
# prevalence <- function(TP, TN, FP, FN){out = (TP + FN) / (TP + FN + FP + TN);return(out)}
# accuracy   <- function(TP, TN, FP, FN){out = (TP + TN) / (TP + FN + FP + TN);return(out)}
# balanced_accuracy   <- function(TP, TN, FP, FN){out = (true_positive_rate(TP, TN, FP, FN) + true_negative_rate(TP, TN, FP, FN)) / 2;return(out)}
# f1_score   <- function(TP, TN, FP, FN){out = 2*TP / (2*TP + FP + FN);return(out)}
# fowlkes_mallows_index <- function(TP, TN, FP, FN){out = sqrt(x = (positive_predictive_value(TP, TN, FP, FN)*true_positive_rate(TP, TN, FP, FN)));return(out)}
#
# threat_score <- function(TP, TN, FP, FN){out = TP / (TP + FN + FP);return(out)}
# critical_success_index = jaccard_index = threat_score
# informedness <- function(TP, TN, FP, FN){out = true_positive_rate(TP, TN, FP, FN) + true_negative_rate(TP, TN, FP, FN) - 1;return(out)}
# prevalence_threshold <- function(TP, TN, FP, FN){
#   TPR = true_positive_rate(TP, TN, FP, FN)
#   FPR = false_positive_rate(TP, TN, FP, FN)
#   out = (sqrt(TPR*FPR) - FPR ) / (TPR - FPR);
#   return(out)
# }
# matthews_correlation_coefficient <- function(TP, TN, FP, FN){
#   TPR = true_positive_rate(TP, TN, FP, FN)
#   TNR = true_negative_rate(TP, TN, FP, FN)
#   PPV = positive_predictive_value(TP, TN, FP, FN)
#   NPV = negative_predictive_value(TP, TN, FP, FN)
#
#   FNR = false_negative_rate(TP, TN, FP, FN)
#   FPR = false_positive_rate(TP, TN, FP, FN)
#   FOR = false_omission_rate(TP, TN, FP, FN)
#   FDR = false_discovery_rate(TP, TN, FP, FN)
#
#   out = sqrt(TPR*TNR*PPV*NPV) - sqrt(x = FNR*FPR*FOR*FDR);
#   return(out)
# }













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

cox_deviance <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw")){}
poisson_deviance <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw")){}

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


binomial_misclassification <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw"), rweights = NULL){
  y[, 1] * (predmat > 0.5) + y[, 2] * (predmat <= 0.5)
}


multinomial_misclassification <- function(){
  classid = as.numeric(apply(predmat, 3, glmnet::glmnet_softmax, ignore_labels = TRUE))
  yperm = matrix(aperm(bigY, c(1, 3, 2)), ncol = nc)
  matrix(1 - yperm[cbind(seq(classid), classid)], ncol = ncol(predtot))
}



classification_accuracy <- function(confusion){
  confusion = as.matrix(out)

  #Get names
  ndn = names(dimnames(confusion))
  #Check total
  rowtot = rowSums(confusion)
  coltot = colSums(confusion)
  tot = sum(coltot)

  ncorrect = sum(diag(confusion))

  correct = (ncorrect)/tot
  x = cbind(x, Total = rowtot)
  x = rbind(x, Total = c(coltot, tot))

  #Update names
  dn = dimnames(x)
  names(dn) = ndn
  dimnames(x) = dn

}

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



# accuracy = function(truth, pred, family=c("gaussian","binomial", "multinomial")) {
#
#   if(identical(tolower(family), "gaussian")){
#     accuracy <- mean((truth - pred)^2)
#   } else if((identical(tolower(family), "binomial")) || (identical(tolower(family), "multinomial"))){
#     # confusion matrix
#     cmat = table(truth, pred)
#     accuracy = sum(diag(cmat))/sum(cmat)
#   }
#
#   return(accuracy);
# }

# #'Accuracy measure for the selected model family
# #'@keywords internal
# accuracy.glmnet = function(truth, pred, weights,
#                            resp.type = c("gaussian", "mgaussian","binomial", "multinomial", "cox", "poisson"),
#                            type.measure = c("deviance", "mse", "mae", "auc", "class"), lambda,
#                            x, ...){
#
#   #Get the list of parameters
#   args <- c(as.list(environment()), list(...));
#
#   #remove resp.type
#   args[c("resp.type", "x")] = NULL;
#
#
#   # N = nrow(x)
#   N = if(!is.null(nrow(truth))){nrow(truth)} else {length(truth)}
#   if (missing(weights)) {args$weights = rep(1, N)}
#
#   acc = switch(resp.type,
#                gaussian    = do.call(what = accuracy.glmnet.gaussian   , args = c(args)),
#                mgaussian   = do.call(what = accuracy.glmnet.mgaussian  , args = c(args)),
#                binomial    = do.call(what = accuracy.glmnet.binomial   , args = c(args)),
#                multinomial = do.call(what = accuracy.glmnet.multinomial, args = c(args)),
#                cox         = do.call(what = accuracy.glmnet.cox        , args = c(args, list(x=x))),
#                poisson     = do.call(what = accuracy.glmnet.poisson    , args = c(args))
#   )
#
#   return(acc);
# }
#
# #'Accuracy measure for the gaussian family
# #'@keywords internal
# accuracy.glmnet.gaussian = function(truth, pred, weights, type.measure = c("mse", "deviance", "mae"), lambda){
#   # acc = switch(type.measure,
#   #        mse = (y - predmat)^2,
#   #        deviance = (y - predmat)^2,
#   #        mae = abs(y - predmat))
#
#   truth = as.vector(truth)
#
#   N = if(!is.null(nrow(truth))){nrow(truth)} else {length(truth)}
#   if (missing(weights)) {weights = rep(1, N)}
#
#
#   if(!missing(lambda)){nlambda = length(lambda);} else if(!is.null(dim(pred))){nlambda = dim(pred)[2]} else {nlambda = 1;}
#
#   predmat = matrix(NA, length(truth), nlambda)
#   predmat[,seq(nlambda)] = pred;
#
#   #Compute the accuracy measure
#   acc.raw = switch(type.measure,
#                    mse      = (truth - predmat)^2,
#                    deviance = (truth - predmat)^2,
#                    mae      = abs(truth - predmat))
#
#   #Apply the weighted mean
#   acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)
#   #Assign a name (for plotting purpose)
#   # if(is.null(dim(acc))){names(acc) = type.measure;} else {row.names(acc) = type.measure}
#   names(acc) = rep(type.measure, length(acc));
#   #return
#   return(acc);
# }
#
# #'Accuracy measure for the multivariate gaussian family
# #'@keywords internal
# accuracy.glmnet.mgaussian = function(truth, pred, weights, type.measure = c("mse", "deviance", "mae"), lambda){
#
#   #Get dimension
#   ndim = dim(truth)
#   nc = ndim[2]
#   nobs = ndim[1]
#
#   if(!missing(lambda)){nlambda = length(lambda);} else {nlambda = 1;}
#
#   predmat = array(NA, c(nobs, nc, nlambda))
#   predmat[, , seq(nlambda)] = pred
#
#   bigY = array(truth, dim(predmat))
#
#   acc.raw = switch(type.measure,
#                    mse      = apply((bigY - predmat)^2, c(1, 3), sum),
#                    deviance = apply((bigY - predmat)^2, c(1, 3), sum),
#                    mae      = apply(abs(bigY - predmat), c(1, 3), sum));
#
#   acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)
#
#   return(acc)
# }
#
#
# #'Accuracy measure for the binomial family
# #'@keywords internal
# accuracy.glmnet.binomial = function(truth, pred, weights,
#                                     type.measure = c("mse", "deviance", "mae", "auc", "class"), lambda){
#
#   N = if(!is.null(nrow(truth))){nrow(truth)} else {length(truth)}
#   if(missing(weights)) {weights = rep(1, N)}
#
#   prob_min = 1e-05;
#   prob_max = 1 - prob_min;
#
#   y = truth;
#   nc = dim(y)
#
#   if (is.null(nc)) {
#     y = as.factor(y)
#     ntab = table(y)
#     nc = as.integer(length(ntab))
#     y = diag(nc)[as.numeric(y), ]
#   }
#
#   if(!missing(lambda)){nlambda = length(lambda);} else {nlambda = 1;}
#
#   predmat = matrix(NA, nrow = get_num_of_observations(y = y, resp.type = "binomial"), ncol = nlambda)
#   predmat[,seq(nlambda)] = pred;
#
#   if(type.measure=="auc"){
#     # acc = glmnet::auc(y = y, prob = pred)
#
#     acc.raw = matrix(NA, 1, nlambda);
#     for(i in seq(nlambda)){
#       acc.raw[1,i] = glmnet::auc.mat(y=y, prob=predmat[, i], weights = weights);
#     }
#
#
#     weights = tapply(X = weights, INDEX = rep(1, N), sum)
#   }else{
#
#     ywt = apply(y, 1, sum);
#     y = y/ywt;
#     weights = weights * ywt;
#
#     # N = nrow(y) - apply(is.na(predmat), 2, sum)
#
#     acc.raw   = switch(type.measure,
#                        mse = (y[, 1] - (1 - predmat))^2 + (y[, 2] - predmat)^2,
#                        mae = abs(y[, 1] - (1 - predmat)) + abs(y[, 2] - predmat),
#                        deviance = {
#                          predmat = pmin(pmax(predmat, prob_min), prob_max);
#                          lp = y[, 1] * log(1 - predmat) + y[, 2] * log(predmat);
#                          ly = log(y);
#                          ly[y == 0] = 0;
#                          ly = drop((y * ly) %*% c(1, 1));
#                          2 * (ly - lp)
#                        },
#                        class = y[, 1] * (predmat > 0.5) + y[, 2] * (predmat <= 0.5))
#   }
#
#   acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)
#
#   #Assign a name (for plotting purpose)
#   # if(is.null(dim(acc))){names(acc) = type.measure;} else {row.names(acc) = type.measure}
#   names(acc) = rep(type.measure, length(acc));
#
#   return(acc);
# }
#
#
# #Prediction type must be set to "response"
# #'Accuracy measure for the multinomial family
# #'@keywords internal
# accuracy.glmnet.multinomial = function(truth, pred, weights, type.measure = c("mse", "deviance", "mae", "class"), lambda){
#
#   N = if(!is.null(nrow(truth))){nrow(truth)} else {length(truth)}
#   if(missing(weights)) {weights = rep(1, N)}
#
#   #Set the variables used in the code from glmnet v2.0-16
#   y = truth;
#
#   #Code from glmnet (adapted for 1 lambda)
#
#   prob_min = 1e-05
#   prob_max = 1 - prob_min
#
#   nc = dim(y)
#   if (is.null(nc)) {
#     y = as.factor(y)
#     ntab = table(y)
#     nc = as.integer(length(ntab))
#     y = diag(nc)[as.numeric(y), ]
#   }
#   else nc = nc[2]
#
#   #Set the matrix containing the prediction response for 1 value of lambda
#   if(!missing(lambda)){nlambda = length(lambda);} else {nlambda = 1;}
#   predmat = array(data = NA, dim = c(get_num_of_observations(y = y, resp.type = "multinomial"), nc, nlambda))
#   predmat[, , seq(nlambda)] = pred;
#
#
#   ywt = apply(y, 1, sum);
#   y = y/ywt;
#   weights = weights * ywt
#
#   # N = nrow(y) - apply(is.na(predmat[, 1, ]), 2, sum)
#   bigY = array(y, dim(predmat))
#   acc.raw = switch(type.measure,
#                    mse = apply((bigY - predmat)^2, c(1, 3), sum),
#                    mae = apply(abs(bigY - predmat), c(1, 3), sum),
#                    deviance = {
#                      predmat = pmin(pmax(predmat, prob_min), prob_max)
#                      lp = bigY * log(predmat)
#                      ly = bigY * log(bigY)
#                      ly[bigY == 0] = 0
#                      apply(2 * (ly - lp), c(1, 3), sum)
#                    },
#                    class = {
#                      classid = as.numeric(apply(predmat, 3, glmnet::glmnet_softmax))
#                      yperm = matrix(aperm(bigY, c(1, 3, 2)), ncol = nc)
#                      matrix(1 - yperm[cbind(seq(classid), classid)], ncol = nlambda)
#                    })
#   acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE);
#
#   #Assign a name (for plotting purpose)
#   # if(is.null(dim(acc))){names(acc) = type.measure;} else {row.names(acc) = type.measure}
#   names(acc) = rep(type.measure, length(acc));
#
#   return(acc);
# }
#
#
# #'Accuracy measure for the cox family
# #Prediction type must be set to "coef"
# #'@keywords internal
# accuracy.glmnet.cox = function(truth, pred, x, weights, type.measure = "deviance", lambda){
#
#   N = if(!is.null(nrow(truth))){nrow(truth)} else {length(truth)}
#   if(missing(weights)) {weights = rep(1, N)}
#
#   y = truth;
#   coefmat = pred;
#
#   if(!missing(lambda)){nlambda = length(lambda);} else {nlambda = 1;}
#   acc.raw = matrix(NA, 1, nlambda)
#
#   #Code from glmnet v2.0-16
#   plk = glmnet::coxnet.deviance(x = x, y = y, weights = weights, beta = coefmat)
#   # cvraw[i, seq(along = plk)] = plk
#   # acc.raw = plk;
#   acc.raw[1, seq(along = plk)] = plk
#
#   status = y[, "status"]
#   # N = nfolds - apply(is.na(cvraw), 2, sum)
#   # weights = as.vector(tapply(X = weights * status, INDEX = foldid, sum))
#   weights = as.vector(tapply(X = weights * status, INDEX = rep(1,N), sum))
#   # weights = sum(weights * status, na.rm = TRUE)#test to make it work
#   acc.raw = acc.raw/weights
#   acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE);
#
#   #Name
#   names(acc) = rep(type.measure, length(acc));
#
#   return(acc);
#
# }
#
# #'Accuracy measure for the poisson family
# #Prediction using default
# #'@keywords internal
# accuracy.glmnet.poisson = function(truth, pred, weights, type.measure = c("mse", "mae","deviance"), lambda){
#
#   truth = as.vector(truth)
#
#   N = if(!is.null(nrow(truth))){nrow(truth)} else {length(truth)}
#   if(missing(weights)) {weights = rep(1, N)}
#
#   #Set the variables used in the code from glmnet v2.0-16
#   y = truth;
#
#   #Code from glmnet (adapted for 1 lambda)
#
#   devi = function(y, eta) {
#     deveta = y * eta - exp(eta)
#     devy = y * log(y) - y
#     devy[y == 0] = 0
#     2 * (devy - deveta)
#   }
#
#   if(!missing(lambda)){nlambda = length(lambda);} else {nlambda = 1;}
#   predmat = matrix(NA, nrow = get_num_of_observations(y = y, resp.type = "poisson"), ncol = nlambda)
#   predmat[,seq(nlambda)] = pred;
#
#   acc.raw = switch(type.measure,
#                    mse = (y - exp(predmat))^2,
#                    mae = abs(y - exp(predmat)),
#                    deviance = devi(y, predmat));
#
#   acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)
#   names(acc) = rep(type.measure, length(acc));
#
#   return(acc);
# }
#
# #'Mean Squared Error as accuracy measure
# #'@keywords internal
# computeAccuracy.mse <- function(truth, pred){
#   accuracy <- mean((truth - pred)^2);
#   return(accuracy);
# }
#
# #'Mean Absolute Error as accuracy measure
# #'@keywords internal
# computeAccuracy.mae <- function(truth, pred){
#   accuracy <- abs(truth - pred);
#   return(accuracy);
# }




#Code from glmnet
#Check the accuracy measure
#@keywords internal
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






# classification_report_binomial <- function(confusion, positive = 1){
#
#   #--------------------------------------------------------------------------------------------#
#   #classes
#   classes = rownames(confusion)
#   nc = length(classes)
#
#   #--------------------------------------------------------------------------------------------#
#   #Check positive
#   if(!(is.integer(positive) & isTRUE(positive > 0) & isTRUE(positive <= nc))){
#     positive = match(x = positive, table = classes)
#     if(is.na(positive)){stop("\nProvided argument 'positive' not valid. Please, check your input.\n")}
#   }
#
#   #--------------------------------------------------------------------------------------------#
#   #set positive class
#   pos_class = classes[positive]
#   neg_class = classes[-positive]
#
#   #--------------------------------------------------------------------------------------------#
#   #Extract info
#   #--------------------------------------------------------------------------------------------#
#   #Class
#   TP = confusion[pos_class, pos_class]
#   # TN = sum(diag(x = confusion[neg_class, neg_class]))
#   TN = sum(confusion[neg_class, neg_class])
#   FN = sum(confusion[pos_class, neg_class])
#   FP = sum(confusion[neg_class, pos_class])
#   #Actual condition
#   P = TP + FN
#   N = FP + TN
#   #Predicted condition
#   PP = TP + FP
#   PN = FN + TN
#
#   #--------------------------------------------------------------------------------------------#
#   #Rates
#   #True positive rate, recall, sensitivity, probability of detection, power, hit rate
#   TPR = TP / P #(1 - FNR)
#   #False positive rate, probability of false alarm, fall-out
#   FPR = FP / N #(1 - TNR)
#   #False negative rate, miss rate
#   FNR = FN / P #(1 - TPR)
#   #True negative rate, specificity (SPC), selectivity
#   TNR = TN / N #(1 - FPR)
#
#   #False discovery rate
#   FDR = FP / PP #(1 - PPV)
#   #Positive predictive value
#   PPV = TP / PP #(1 - FDR)
#   #False omission rate
#   FOR = FN / PN #(1 - NPV)
#   #Negative predictive value
#   NPV = TN / PN #(1 - FOR)
#
#   #--------------------------------------------------------------------------------------------#
#   #Likelihood
#   #Positive likelihood ratio (LR+)
#   LRpos = TPR / FPR
#   #Negative likelihood ratio (LR-)
#   LRneg = FNR / TNR
#
#   #--------------------------------------------------------------------------------------------#
#   #Markedness (MK),deltaP
#   MK = PPV + NPV - 1
#   #Diagnostic odds ratio
#   DOR = LRpos/LRneg
#
#   #--------------------------------------------------------------------------------------------#
#   #Prevalence
#   prevalence = P / (P + N)
#   #Accuracy
#   ACC = (TP + TN) / (P + N)
#   #Balanced accuracy
#   BA = (TPR + TNR) / 2
#   #F1 score
#   F1 = 2*TP / (2*TP + FP + FN) #(2*PPV/(PPV + TPR))
#   #Fowlkes-Mallows index
#   FM = sqrt(x = (PPV*TPR))
#   #Matthews correlation coefficient
#   MCC = sqrt(TPR*TNR*PPV*NPV) - sqrt(x = FNR*FPR*FOR*FDR)
#   #Threat score (TS), critical success index (CSI), Jaccard index
#   TS = TP / (TP + FN + FP)
#
#   #Informedness, bookmaker informedness (BM)
#   BM = TPR + TNR - 1
#   #Prevalence threshold (PT)
#   PT = (sqrt(TPR*FPR) - FPR ) / (TPR - FPR)
#
#   #--------------------------------------------------------------------------------------------#
#   # #Overall statistics
#   # #Total population
#   # TOT = sum(confusion)
#   # #Accuracy
#   # TOTACC = (TP + TN) / TOT
#
#   #--------------------------------------------------------------------------------------------#
#   #Create output
#   #--------------------------------------------------------------------------------------------#
#   out = data.frame(
#     pos = pos_class,
#     TP, TN, FN, FP,
#     P ,  N, PP, PN,
#     TPR, FPR, FNR, TNR,
#     FDR, PPV, FOR, NPV,
#     LRpos, LRneg, MK, DOR,
#     prevalence, ACC, BA,
#     F1, FM, MCC, TS, BM, PT,
#     # TOT, TOTACC,
#     stringsAsFactors = F
#   )
#
#   out = as.list(out)
#
#   class(out) = c("BinaryClassificationReport", class(out))
#   #--------------------------------------------------------------------------------------------#
#   #return
#   return(out)
# }


# mean_squared_log_error <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw"), root = FALSE){
#
#   #get N
#   N = if(!is.null(nrow(true))){nrow(true)} else {length(true)}
#
#   #check weights
#   if(missing(weights) || is.null(weights)){weights = rep(1, N)}
#
#   #Raw accuracy
#   acc.raw = (log(1 + true) - log(1 + pred))^2
#
#   #check if multi-response
#   is.multi = !is.null(dim(acc.raw))
#
#   #If multi-response, sum the residues
#   if(is.multi){
#     if(identical(multi, "raw")){
#       #Apply the weighted mean
#       acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)#to use when multi-config
#     }else{
#       if(identical(multi, "sum")){
#         #sum
#         acc.raw = rowSums(x = acc.raw, na.rm = T)
#       } else {
#         #average
#         acc.raw = rowMeans(x = acc.raw, na.rm = T)
#       }
#       #Apply the weighted mean
#       acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
#     }
#   } else {
#     #Apply the weighted mean
#     acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
#   }
#
#   #root
#   if(root){
#     acc = sqrt(acc)
#   }
#
#   #return
#   return(acc)
# }
#
# squared_log_error <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw"), root = FALSE){
#
#   #Raw accuracy
#   acc.raw = (log(1 + true) - log(1 + pred))^2
#
#   #check if multi-response
#   is.multi = !is.null(dim(acc.raw))
#
#   #multi
#   multi = match.arg(multi)
#
#   #get N
#   N = if(is.multi){ncol(acc.raw)} else {1}
#
#   #check weights
#   if(missing(weights) || is.null(weights)) {weights = rep(1, N)}
#
#   #If multi-response, sum the residues
#   if(is.multi){
#     if(identical(multi, "raw")){
#       #Apply the weights
#       acc = sweep(x = acc.raw, MARGIN = 2, STATS = weights, FUN = `*`)
#     }else{
#       if(identical(multi, "sum")){
#         #Apply the weights
#         acc.raw = sweep(x = acc.raw, MARGIN = 2, STATS = weights, FUN = `*`)
#         #sum
#         acc = rowSums(x = acc.raw, na.rm = T)
#       } else {
#         #average
#         acc = apply(X = acc.raw, MARGIN = 1, FUN = stats::weighted.mean, w = weights, na.rm = TRUE)
#       }
#     }
#   } else {
#     acc = acc.raw
#     rm(acc.raw)
#   }
#
#   #root
#   if(root){
#     acc = sqrt(acc)
#   }
#
#   #return
#   return(acc)
# }

# mean_squared_error <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw"), root = FALSE){
#
#   #get N
#   N = if(!is.null(nrow(true))){nrow(true)} else {length(true)}
#
#   #check weights
#   if(missing(weights) || is.null(weights)){weights = rep(1, N)}
#
#   #Raw accuracy
#   acc.raw = (true - pred)^2
#
#   #check if multi-response
#   is.multi = !is.null(dim(acc.raw))
#
#   #If multi-response, sum the residues
#   if(is.multi){
#     if(identical(multi, "raw")){
#       #Apply the weighted mean
#       acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)#to use when multi-config
#     }else{
#       if(identical(multi, "sum")){
#         #sum
#         acc.raw = rowSums(x = acc.raw, na.rm = T)
#       } else {
#         #average
#         acc.raw = rowMeans(x = acc.raw, na.rm = T)
#       }
#       #Apply the weighted mean
#       acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
#     }
#   } else {
#     #Apply the weighted mean
#     acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
#   }
#
#   #root
#   if(root){
#     acc = sqrt(acc)
#   }
#
#   #return
#   return(acc)
# }
#
# squared_error <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw"), root = FALSE){
#
#   #Raw accuracy
#   acc.raw = (true - pred)^2
#
#   #check if multi-response
#   is.multi = !is.null(dim(acc.raw))
#
#   #multi
#   multi = match.arg(multi)
#
#   #get N
#   N = if(is.multi){ncol(acc.raw)} else {1}
#
#   #check weights
#   if(missing(weights) || is.null(weights)) {weights = rep(1, N)}
#
#   #If multi-response, sum the residues
#   if(is.multi){
#     if(identical(multi, "raw")){
#       #Apply the weights
#       acc = sweep(x = acc.raw, MARGIN = 2, STATS = weights, FUN = `*`)
#     }else{
#       if(identical(multi, "sum")){
#         #Apply the weights
#         acc.raw = sweep(x = acc.raw, MARGIN = 2, STATS = weights, FUN = `*`)
#         #sum
#         acc = rowSums(x = acc.raw, na.rm = T)
#       } else {
#         #average
#         # acc.raw = rowMeans(x = acc.raw, na.rm = T)
#         acc = apply(X = acc.raw, MARGIN = 1, FUN = stats::weighted.mean, w = weights, na.rm = TRUE)
#       }
#     }
#   } else {
#     acc = acc.raw
#     rm(acc.raw)
#   }
#
#   #root
#   if(root){
#     acc = sqrt(acc)
#   }
#
#   #return
#   return(acc)
# }

# mean_absolute_percentage_error <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw"), eps = 1e-10){
#
#   #multi
#   multi = match.arg(multi)
#
#   #get N
#   N = if(!is.null(nrow(true))){nrow(true)} else {length(true)}
#
#   #check weights
#   if(missing(weights) || is.null(weights)) {weights = rep(1, N)}
#
#   #Raw accuracy
#   acc.raw = abs(true - pred) / pmax(abs(true), eps)
#
#   #check if multi-response
#   is.multi = !is.null(dim(acc.raw))
#
#   #If multi-response, sum the residues
#   if(is.multi){
#     if(identical(multi, "raw")){
#       #Apply the weighted mean
#       acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)#to use when multi-config
#     }else{
#       if(identical(multi, "sum")){
#         #sum
#         acc.raw = rowSums(x = acc.raw, na.rm = T)
#       } else {
#         #average
#         acc.raw = rowMeans(x = acc.raw, na.rm = T)
#       }
#       #Apply the weighted mean
#       acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
#     }
#   } else {
#     #Apply the weighted mean
#     acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
#   }
#
#   #return
#   return(acc)
# }
#
# absolute_percentage_error <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw"), eps = 1e-10){
#
#   #Raw accuracy
#   acc.raw = abs(true - pred) / pmax(abs(true), eps)
#
#   #check if multi-response
#   is.multi = !is.null(dim(acc.raw))
#
#   #multi
#   multi = match.arg(multi)
#
#   #get N
#   N = if(is.multi){ncol(acc.raw)} else {1}
#
#   #check weights
#   if(missing(weights) || is.null(weights)) {weights = rep(1, N)}
#
#   #If multi-response, sum the residues
#   if(is.multi){
#     if(identical(multi, "raw")){
#       #Apply the weights
#       acc = sweep(x = acc.raw, MARGIN = 2, STATS = weights, FUN = `*`)
#     }else{
#       if(identical(multi, "sum")){
#         #Apply the weights
#         acc.raw = sweep(x = acc.raw, MARGIN = 2, STATS = weights, FUN = `*`)
#         #sum
#         acc = rowSums(x = acc.raw, na.rm = T)
#       } else {
#         #average
#         # acc.raw = rowMeans(x = acc.raw, na.rm = T)
#         acc = apply(X = acc.raw, MARGIN = 1, FUN = stats::weighted.mean, w = weights, na.rm = TRUE)
#       }
#     }
#   } else {
#     acc = acc.raw
#     rm(acc.raw)
#   }
#
#   #return
#   return(acc)
# }



# absolute_error <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw")){
#
#   #Raw accuracy
#   acc.raw = abs(true - pred)
#
#   #multi
#   multi = match.arg(multi)
#
#   #check if multi-response
#   is.multi = !is.null(dim(acc.raw))
#
#   #get N
#   N = if(is.multi){ncol(acc.raw)} else {1}
#
#   #check weights
#   if(missing(weights) || is.null(weights)) {weights = rep(1, N)}
#
#   #If multi-response, sum the residues
#   if(is.multi){
#     if(identical(multi, "raw")){
#       #Apply the weights
#       acc = sweep(x = acc.raw, MARGIN = 2, STATS = weights, FUN = `*`)
#     }else{
#       if(identical(multi, "sum")){
#         #Apply the weights
#         acc.raw = sweep(x = acc.raw, MARGIN = 2, STATS = weights, FUN = `*`)
#         #sum
#         acc = rowSums(x = acc.raw, na.rm = T)
#       } else {
#         #average
#         # acc.raw = rowMeans(x = acc.raw, na.rm = T)
#         acc = apply(X = acc.raw, MARGIN = 1, FUN = stats::weighted.mean, w = weights, na.rm = TRUE)
#       }
#     }
#   } else {
#     acc = acc.raw
#     rm(acc.raw)
#   }
#
#   #return
#   return(acc)
# }


# mean_absolute_error <- function(true, pred, weights = NULL, multi = c("average", "sum", "raw")){
#
#   #multi
#   multi = match.arg(multi)
#
#   #get N
#   N = if(!is.null(nrow(true))){nrow(true)} else {length(true)}
#
#   #check weights
#   if(missing(weights) || is.null(weights)) {weights = rep(1, N)}
#
#   #Raw accuracy
#   acc.raw = abs(true - pred)
#
#   #check if multi-response
#   is.multi = !is.null(dim(acc.raw))
#
#   #If multi-response, sum the residues
#   if(is.multi){
#     if(identical(multi, "raw")){
#       #Apply the weighted mean
#       acc = apply(acc.raw, 2, stats::weighted.mean, w = weights, na.rm = TRUE)#to use when multi-config
#     }else{
#       if(identical(multi, "sum")){
#         #sum
#         acc.raw = rowSums(x = acc.raw, na.rm = T)
#       } else {
#         #average
#         acc.raw = rowMeans(x = acc.raw, na.rm = T)
#       }
#       #Apply the weighted mean
#       acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
#     }
#   } else {
#     #Apply the weighted mean
#     acc = stats::weighted.mean(x = acc.raw, w = weights, na.rm = TRUE)
#   }
#
#   #return
#   return(acc)
# }

# get_available_measures = function(resp.type = c("gaussian", "mgaussian", "binomial", "multinomial", "poisson", "cox")){
#
#   resp.type = match.arg(resp.type)
#
#   #Available measures
#   type.measures = c("mse", "deviance", "class", "auc", "mae", "C");
#
#   #Get subclass
#   subclass = switch(resp.type,
#                     gaussian    = "elnet",
#                     mgaussian   = "mrelnet",
#                     binomial    = "lognet",
#                     multinomial = "multnet",
#                     cox         = "coxnet",
#                     poisson     = "fishnet")
#
#   devname = switch(subclass,
#                    elnet = "Mean-squared Error",
#                    lognet = "Binomial Deviance",
#                    fishnet = "Poisson Deviance",
#                    coxnet = "Partial Likelihood Deviance",
#                    multnet = "Multinomial Deviance",
#                    mrelnet = "Mean-squared Error")
#
#   typenames = c(deviance = devname,
#                 mse = "Mean-squared Error",
#                 mae = "Mean Absolute Error",
#                 auc = "AUC",
#                 class = "Misclassification Error",
#                 C = "C-index")
#
#
#   #Get supported measures
#   subclass.ch = switch(subclass,
#                        # elnet   = c(1, 2, 5),#DEVIANCE IS MSE
#                        # mrelnet = c(1, 2, 5),#DEVIANCE IS MSE
#                        elnet   = c(1, 5),#DEVIANCE IS MSE
#                        mrelnet = c(1, 5),#DEVIANCE IS MSE
#                        lognet  = c(2, 3, 4, 1, 5),
#                        fishnet = c(2, 1, 5),
#                        coxnet  = c(2, 6),
#                        multnet = c(2, 3, 1, 5));
#
#   subclass.type = type.measures[subclass.ch]
#
#   names(subclass.type) = typenames[subclass.type];
#
#   return(subclass.type)
# }
