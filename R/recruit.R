#'Probability of recruitment
#'@param object list of object of class \code{renoir.trained}
#'@param best.config which configuration to consider
#'@return a \code{data.frame} containing the probability of recruitment for each coefficient
#'(number of times coefficient is nonzero / number of runs), or a list of \code{data.frame}
#'if response type is \code{"mgaussian"} or ungrouped \code{"multinomial"}.
#'@keywords internal
#'@author Alessandro Barberis
recruited <- function(object, best.config = c("min", "1se")){

  #----------------------------------------------------------------------#
  best.config  = match.arg(arg = best.config)
  # print(best.config)
  #----------------------------------------------------------------------#
  #For each resample iteration, select the model with best configuration
  #and extract the index of nonzero coefficients

  coefs = lapply(X = object, FUN = function(x, ...){
    coef.index = stats::predict(object = x, best.config = best.config, type = "coefficients");
  }, best.config = best.config)

  #----------------------------------------------------------------------#
  #Reshape the list, needed in case of multinomial response.
  #The following code returns a list where each element correspond to a class
  #and consists of a list of vectors containing the index of nonzero coefficients.
  #Each vector corresponds to a run in the resampling

  if(is.list(coefs[[1]])){
    coefs = do.call(what = Map, args = c(c, coefs))

    #----------------------------------------------------------------------#
    #Merge the coefficients from different runs in one matrix
    coefs = lapply(X = coefs, FUN = function(x){do.call(what = cbind, args = x)})
    # View(as.matrix(coefs[[1]]))

    #----------------------------------------------------------------------#
    #Which coefficient is different from 0?
    test = lapply(X = coefs, FUN = function(x){as.matrix(x)!=0})

    #----------------------------------------------------------------------#
    #Get frequency of recruitment (range is between 0 and 1)
    freq = lapply(X = test, FUN = function(x){data.frame(p = rowSums(x)/ncol(x))})

  } else {
    #Merge the coefficients from different runs in one matrix
    coefs = do.call(what = cbind, args = coefs)
    # View(as.matrix(coefs[[1]]))

    #----------------------------------------------------------------------#
    #Which coefficient is different from 0?
    test = as.matrix(coefs)!=0

    #----------------------------------------------------------------------#
    #Get frequency of recruitment (range is between 0 and 1)
    freq = data.frame(p = rowSums(test)/ncol(test))
  }

  #----------------------------------------------------------------------#
  #Return probability of occurrence

  return(freq)
}


#'Features Importance
#'@description This function computes a significance score for each feature.
#'@section Details:
#'A feature significance score is computed as the sum of the normalised
#'coefficient of the feature in the model multiplied by a weight for such model.
#'The formula for the i-th feature is:
#'
#'\deqn{score_{i} = \frac{1}{s}\sum_{j=1}^{n} weight_{j}*coefficient_{j} ,}
#'
#'where \eqn{n} is the total number of models computed across the different
#'training set sizes, \eqn{coefficient_{j}} is the normalised coefficient
#'(i.e. the coefficient of the feature divided by the maximum absolute value
#'of the coefficients of the features for that model so as to have a
#'value ranging \eqn{[-1,1]}), and \eqn{s} is the sum of weights.
#'The \eqn{weight} for a model is obtained by dividing
#'the multiplicative inverse of the squared prediction error of the model by
#'the max value across all models, so that models with better prediction
#'accuracy have higher weight:
#'
#'\deqn{weight_{j} = \frac{w_{j}}{w_{best}},}
#'
#'where \eqn{w_{j}} is the multiplicative inverse of the squared prediction error of
#'the model:
#'
#'\deqn{w_{j} = \frac{1}{err_{j}^2},}
#'
#'and
#'
#'\deqn{w_{best} = \max{w_{1},...,w_{j},...,w_{n}}.}
#'
#'Note that for the \code{auc} accuracy measure, we consider the prediction error
#'as \eqn{err_{j} = 1 - auc_{j}}.
#'
#'By selecting \code{use = "presence"}, the \eqn{coefficient_{j}}
#'term in the computation of the significance score is exchanged
#'for \eqn{presence_{j}*sign_{j}}, where \eqn{presence_{j}} stands for the
#'presence of the feature in the j-th model and \eqn{sign_{j}}
#'is the sign of the coefficient of the i-th feature in the j-th model.
#'@param object a list of \code{renoir.tat} objects
#'@param best.config which configuration to consider
#'@param recruitment.term whether to use a binary variable (\code{presence = 0} or \code{presence = 1})
#'multiplied by the sign of the coefficient, or to use directly the coefficient of a feature
#'in the computation of the significance score.
#'@param set the set of data used for the computation of the accuracy measures. For example,
#'if \code{set = "test.set"} the accuracy measures to be used in the computation of the
#'features importance are the ones calculated on the test set.
#'@return For single response, it is a list of vectors, where each vector is associate to an accuracy measure, and contains
#'measures of significance for each feature. For multi-response, it is a list of lists.
features_importance <- function(object, best.config = c("min", "1se"),
                                recruitment.term = c("coefficient", "presence"),
                                set = c('test.set', 'full.set', 'train.set')){

  #--------------------------------------------------------------------------------------#
  recruitment.term = match.arg(arg = recruitment.term)
  best.config = match.arg(arg = best.config)
  set = match.arg(set);
  #--------------------------------------------------------------------------------------#
  #1) Get the performance measures for each training set size
  performance = lapply(X = object, FUN = function(tat, set){

    #get accuracy
    assessment = tat[[get_name_slot_tat_assess()]][[set]][[get_name_slot_best_config(best.config)]][[get_name_slot_tat_assess_measures()]]

  }, set = set)

  #reshape the list
  performance = do.call(what = Map, args = c(c, performance))

  #--------------------------------------------------------------------------------------#

  #2) Compute the weights
  weights = sapply(X = seq(length(performance)), FUN = function(i, performance){

    #measure name
    name = names(performance)[i]

    #weights
    w = performance[[i]]

    #If AUC measure, compute 1 - auc so to have a metric where 0
    #corresponds to best performance
    if(identical(name, "auc")){ w = 1 - w}

    #get squared error
    w = w^2;

    #Check if any is 0
    test = which(w == 0)

    #Set min value (needed to avoid 1/0 in next computation)
    # w[test] = .Machine$double.xmin;
    if(length(test)>0) {w[test] = min(min(abs(w[-test] - 0.001), na.rm = T), 1e-03, na.rm = T)};

    #reverse
    w = 1 / w;

    #get best performance measure (as we computed 1/err,
    #the higher the value, the better the prediction)
    b = max(w, na.rm = TRUE);

    #divide weights by best measure to have weights range between 0 and 1
    w = w / b;

    out = list()

    out[[name]] = w;

    return(out)
  }, performance = performance)


  #--------------------------------------------------------------------------------------#
  #3) Get all coefficients
  all_coefs = lapply(X = object, FUN = function(tat){

    #get coefficients from the models obtained during the different steps
    #of the resampling method for the selected training set size
    coefs = lapply(X = tat$train, FUN = function(x, best.config, ...){
      coef = stats::predict(object = x, best.config = best.config, type = "coefficients");
    },best.config = best.config)
  })

  #Get them as a list, where each element is the result of a run
  all_coefs = unlist(x = all_coefs, recursive = F)

  #--------------------------------------------------------------------------------------#
  #4) Check if we have a list of coefficients (one for each response in multi-response analysis)
  is.multi.response = if(is.list(all_coefs[[1]])){TRUE}else{FALSE}

  #--------------------------------------------------------------------------------------#
  #5) Compute the recruitment term
  if(is.multi.response){
    #Reshape the list so that each element is the result of a response (i.e. a list of models
    #for such response)
    all_coefs = do.call(what = Map, args = c(c, all_coefs))
    #Compute the recruitment term
    recruitment_term = lapply(X = all_coefs, FUN = compute_significance_score_recuitment_term, recruitment.term = recruitment.term)
  } else {
    #Compute the recruitment term
    recruitment_term = compute_significance_score_recuitment_term(coefs = all_coefs, recruitment.term = recruitment.term)
  }


  #--------------------------------------------------------------------------------------#
  #4) Compute the scores, considering each accuracy measure
  if(is.multi.response){
    scores = lapply(X = recruitment_term, FUN = function(r, weights){
      s = compute_significance_score(weights = weights, recruitment = r);
      return(s)
    }, weights = weights)
  } else {
    scores = compute_significance_score(weights = weights, recruitment = recruitment_term);
  }

  #--------------------------------------------------------------------------------------#
  return(scores);
}

#'Features Importance
#'@description This function computes a significance score for each feature.
#'See \code{\link{features_importance}} for more details.
#'@inheritParams features_importance
#'@return Features Importance
get_features_importance <- function(object, recruitment.term = c("coefficient", "presence")){

  #--------------------------------------------------------------------------------------#
  recruitment.term = match.arg(arg = recruitment.term)

  #--------------------------------------------------------------------------------------------#
  #Test set
  test.best.config.min = features_importance(object = object, best.config = "min", recruitment.term = recruitment.term, set = "test.set")
  test.best.config.1se = features_importance(object = object, best.config = "1se", recruitment.term = recruitment.term, set = "test.set")

  test.list = list()
  test.list[[get_name_slot_best_config(best.config = 'min')]] = test.best.config.min;
  test.list[[get_name_slot_best_config(best.config = '1se')]] = test.best.config.1se;
  #--------------------------------------------------------------------------------------------#
  #Full set
  full.best.config.min = features_importance(object = object, best.config = "min", recruitment.term = recruitment.term, set = "full.set")
  full.best.config.1se = features_importance(object = object, best.config = "1se", recruitment.term = recruitment.term, set = "full.set")

  full.list = list()
  full.list[[get_name_slot_best_config(best.config = 'min')]] = full.best.config.min;
  full.list[[get_name_slot_best_config(best.config = '1se')]] = full.best.config.1se;
  #--------------------------------------------------------------------------------------------#
  outlist = list()

  outlist[['test.set']] = test.list;
  outlist[['full.set']] = full.list;
  #--------------------------------------------------------------------------------------------#
  return(outlist)
}

#'Compute Significance Score Term
#'@description This function computes the normalised coefficients or the
#'presence term for a single repsonse type (e.g. gaussian, binomial,
#'poisson, cox).
#'@param coefs list of coefficients from all the runs
#'@inheritParams features_importance
#'@return a Matrix with the significance score terms for each feature,
#'for each model
#'
#'@keywords internal
#'
compute_significance_score_recuitment_term <- function(coefs, recruitment.term = c("coefficient", "presence")){

  recruitment.term = match.arg(arg = recruitment.term)

  #----------------------------------------------------------------------#

  #1)Merge the coefficients from different runs in one matrix
  coefs = do.call(what = cbind, args = coefs)

  #----------------------------------------------------------------------#
  #2) Compute presence term or normalised coefficients
  if(identical(x = recruitment.term, y = "presence")){
    #Which coefficient is greatere than 0?
    test = as.matrix(coefs)>0
    #----------------------------------------------------------------------#
    #Set recruited feature coefficient to one
    coefs[test] = 1;
    #Which coefficient is greatere than 0?
    test = as.matrix(coefs)<0
    #----------------------------------------------------------------------#
    #Set recruited feature coefficient to one
    coefs[test] = -1;
  } else {
    #Divide each coefficient for the maximum coefficients absolute value, for each model
    coefs = sweep(x = coefs, MARGIN = 2, STATS = apply(X = abs(coefs), MARGIN = 2, FUN = max, na.rm = TRUE), FUN = "/")
  }

  #----------------------------------------------------------------------#
  #return
  return(coefs)

}


#'Compute Significance Score
#'@param weights the weights for the computation
#'@param recruitment the recruitment terms
#'@return the significance score for each feature
#'@keywords internal
compute_significance_score <- function(weights, recruitment){

  #Compute the scores
  scores = lapply(X = weights, FUN = function(w, recruitment){

    scaling.factor = sum(w, na.rm = T)

    # r = recruitment[1,,drop=F]
    score = apply(X = recruitment, MARGIN = 1, FUN = function(r, w, n){

      #multiply the weight by the presence in the model
      test = as.vector(r * w);

      #sum scores
      s = sum(test, na.rm = T);

      #divide by sum of weights to have a score in the range [0,1]
      s = s / n;

      return(s);
    }, w = w, n = scaling.factor)

    return(score)
  }, recruitment = recruitment)

  return(scores)
}


#'Compute Rank Product/Rank Sum
#'@description Computes the rank product of the features importance to summarise
#'the results obtained by using from different configuration and accuracy measures.
#'Note that \code{AUC} measure is not considered.
#'@param object a \code{renoir} object
#'@param set the set of data used for the computation of the accuracy measures
#'@param ... further arguments to \code{\link{compute_rank_product}}
#'@return a data frame or a list of data frames containing the statistics and
#'associated estimated significance. See \code{\link{compute_rank_product}}
#'for more details.
#'
#'@keywords internal
get_rank_product <- function(object,
                             # set = c('full.set', 'test.set', 'train.set'),
                             set = c('full.set', 'test.set'),
                             ...){

  #----------------------------------------------------------------------#
  set = match.arg(set);

  #----------------------------------------------------------------------#
  get_rank_prod_default <- function(feat.imp, ...){

    #select auc measures
    rm = grep(pattern = "auc", x = names(feat.imp), value = T)

    if(length(rm)>0){
      #remove
      feat.imp[rm] = NULL
    }

    #bind as data frame
    fi.df = cbind.data.frame(feat.imp)

    #set as matrix
    fi.matrix = as.matrix(fi.df)

    #get RP
    out = compute_rank_product(data = fi.matrix, ...)
  }

  #----------------------------------------------------------------------#
  #extract data
  feat.imp  = object[[get_name_slot_feat_importance()]][[set]]
  resp.type = object[[get_name_slot_resp_type()]]

  #----------------------------------------------------------------------#
  if(!is.null(feat.imp)){

    if(!identical(resp.type, "multinomial") && !identical(resp.type, "mgaussian")){
      #unlist
      feat.imp = unlist(x = feat.imp, recursive = F)
      #compute
      out = get_rank_prod_default(feat.imp = feat.imp, ...)
    } else {
      #reshape data
      feat.imp = do.call(what = Map, args = c(c, feat.imp))

      #compute
      out = lapply(X = feat.imp, FUN = get_rank_prod_default, ... = ...)
    }

  } else {
    out = NA
  }

  #----------------------------------------------------------------------#
  return(out)

}


#'Compute Rank Product/Rank Sum
#'@param data a matrix vars x obs
#'@inheritParams RankProd::RankProducts
#'@param ... further arguments to \code{\link[RankProd]{RankProducts}}
#'@return a dataframe containing different elements
#'\describe{
#'   \item{code{pfp_}}{estimated percentage of false positive predictions for each feature
#'   positively and negatively correlated with the outcome}
#'   \item{code{p_val}}{estimated p-value for each feature
#'   positively and negatively correlated with the outcome}
#'   \item{code{Rank_Stat_}}{the rank-product/rank-sum statistics evaluated per each gene
#'   positively and negatively correlated with the outcome}
#'
#'}
#'@keywords internal
compute_rank_product <- function(data,
                                 cl = rep(1, times = ncol(data)),
                                 logged = T,
                                 gene.names = rownames(data),
                                 ...){

  #compute the rank product
  rank.prod <- RankProd::RankProducts(data = data,
                                      cl = cl,
                                      # origin = rep(1, length(colnames(data))),
                                      logged = logged,
                                      gene.names = gene.names,
                                      ...)

  #create an output table
  out <- cbind(rank.prod$pfp,
               rank.prod$pval,
               rank.prod$RPs)

  #setup column names
  colnames(out) <- c(paste0("pfp_",       colnames(rank.prod$pfp)),
                     paste0("p_val_",     colnames(rank.prod$pval)),
                     paste0("Rank_Stat_", colnames(rank.prod$RPs)))

  #update names
  colnames(out) = gsub(pattern = "class1 < class2", replacement = "neg", x = colnames(out), fixed = F)
  colnames(out) = gsub(pattern = "class1 > class2", replacement = "pos", x = colnames(out), fixed = F)

  #return
  return(out)

}
