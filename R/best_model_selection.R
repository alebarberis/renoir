#'Which is the best training set size
#'@description This function returns the index of the best training set size from a
#'list of \code{renoir.tat} object.
#'@param object a list of objects of class \code{renoir.tat}, where each element
#'correspons to a different training set size
#'@param set which set of assessment data to consider for the selection of the best training set size.
#'The available options are \code{'test.set'} (default), \code{'train.set'} and \code{'full.set'}
#'(which consists of \code{test.set + train.set})
#'@return the index of the best training set size
#'@keywords internal
which_best_training_set_size = function(object, set = c('test.set', 'train.set', 'full.set')){

  #--------------------------------------------------------------------------------------------#
  set = match.arg(set)

  #--------------------------------------------------------------------------------------------#
  which.best = function(object, best.config){
    #Get the upper or lower confidence limit depending on the accuracy measure
    best.conf.limit = lapply(X = object, FUN = function(x, set, best.config, ...){
      ci.list = x[["assess"]][[set]][[best.config]][["ci"]];
      # lapply(X = ci.list, FUN = function(y){y[["up"]]})
      out = list()
      name_measure = names(ci.list);
      for(i in 1L:length(ci.list)){
        nm = name_measure[i];
        out[[nm]] = if(identical(nm, "auc")){
                      ci.list[[i]][["low"]]
                    } else {
                      ci.list[[i]][["up"]]
                    }
      }
      return(out);
    }, set = set, best.config = best.config)

    #--------------------------------------------------------------------------------------------#
    if(is.list(best.conf.limit[[1]]))
      best.conf.limit = do.call(what = Map, args = c(c, best.conf.limit))

    #--------------------------------------------------------------------------------------------#
    #Which training set size has the minimum upper confidence limit
    # which.up.min = lapply(X = upper.conf.limit, FUN = which.min)
    name_measures = names(best.conf.limit);
    which.best.ci = lapply(X = seq(length(best.conf.limit)), FUN = function(i, name_measures, best.conf.limit){
      nm = name_measures[i];
      if(identical(x = nm, y = "auc")){
        which.max(x = best.conf.limit[[i]]);
      } else {
        which.min(x = best.conf.limit[[i]]);
      }

    }, name_measures = name_measures, best.conf.limit = best.conf.limit)
    names(which.best.ci) = name_measures;
    #--------------------------------------------------------------------------------------------#
    return(which.best.ci)
  }

  #--------------------------------------------------------------------------------------------#

  best.config.min = which.best(object = object, best.config = "best.config.min")
  best.config.1se = which.best(object = object, best.config = "best.config.1se")

  #--------------------------------------------------------------------------------------------#
  outlist = list(best.config.min = best.config.min,
                 best.config.1se = best.config.1se)
  #--------------------------------------------------------------------------------------------#

  return(outlist)

}

#'Which is the best training set size
#'@description This function returns the indices of the best training set size from a
#'list of \code{renoir.tat} objects.
#'@inheritParams which_best_training_set_size
#'@return a list of lists containing the indices of the best training set size for each
#'set of data ('test.set', 'train.set' and 'full.set')
#'\describe{
#'   \item{\code{test.set}}{a list of 2 elements (one for best configuration),
#'   each being a list where each element is the index of the best training set size
#'   associated to a specific accuracy measure computed by using the test.set}
#'   \item{\code{train.set}}{a list of 2 elements (one for best configuration),
#'   each being a list where each element is the index of the best training set size
#'   associated to a specific accuracy measure computed by using the train.set}
#'   \item{\code{full.set}}{a list of 2 elements (one for best configuration),
#'   each being a list where each element is the index of the best training set size
#'   associated to a specific accuracy measure computed by using the full.set}
#'}
#'@keywords internal
which_best_training_set_sizes <- function(object){
  test.set = which_best_training_set_size(object = object, set = 'test.set')
  train.set = which_best_training_set_size(object = object, set = 'train.set')
  full.set = which_best_training_set_size(object = object, set = 'full.set')

  out = list(test.set = test.set,
             train.set = train.set,
             full.set = full.set)

  return(out)

}


#'Which is the best model
#'@description This function returns the index of the best model over a list
#'of models obtained via a resampling procedure.
#'The function looks over the performance measures and
#'for each of them select the best model, i.e. the model which minimises
#'or maximises the measure. If a tolerance is given, the function compares
#'all models having an accuracy measure within the range
#'\eqn{[error - tolerance*error, error + tolerance*error]}
#'and selects the one with less coefficients.
#'@param trained a list of objects of class \code{RenoirTrainedAndTested}, where each element
#'corresponds to a different run over the same training set size
#'@param assessed a list of assessment measures, one for each model in \code{trained}
#'@param learning.method method for fitting the data
#'@param tolerance numeric (range [0,1]), the tolerance in the selection of the best model
#'@return the index of the best model for each accuracy measure, for both best configurations
#'@keywords internal
which_best_resampling_iteration <- function(trained,
                                            assessed,
                                            learning.method,
                                            best.config = c("min", "1se"),
                                            tolerance = 0){

  best.config = match.arg(best.config);

  if(tolerance<0 || tolerance>1 || is.null(tolerance) || is.na(tolerance)) {
    warning("'tolerance' out of range. Set to 0.")
    tolerance = 0
  }

  #--------------------------------------------------------------------------------------------#

  best.config.slot = get_name_slot_best_config(best.config = best.config)

  #Get the performance measures for the fit
  # measures = lapply(X = assessed, FUN = function(x){x[["measures"]]})
  measures = assessed[[best.config.slot]][["measures"]]

  #Get the names of the measures
  name_measures = names(measures);

  #If AUC measure, compute 1 - auc so to have a metric where 0
  #corresponds to best performance
  # if(!is.null(measures[['auc']])){ measures[['auc']] = 1 - measures[['auc']]}
  #
  # #Get best measure
  # best.measures = lapply(X = measures, FUN = min, na.rm = TRUE)

  best.measures = lapply(X = seq(length(measures)), FUN = function(i, measures, name_measures){

    name_measure = name_measures[i];
    measure = measures[[i]]
    best = get_best_measure(measures = measure, type.measure = name_measure);
    return(best)

  }, measures = measures, name_measures = name_measures)

  #Compute tolerance
  tols = lapply(X = best.measures, FUN = function(measure, tolerance){tolerance*abs(x = measure)}, tolerance = tolerance);

  #Get interval
  tols.range = list()
  for(i in seq(length(best.measures))){
    tols.range[[i]] = c(best.measures[[i]] - tols[[i]], best.measures[[i]] + tols[[i]])
  }

  #List to store the result
  out = list();

  #Loop over the accuracy measures
  i = 1
  for(i in seq(length(measures))){

    #measure
    measure = measures[[i]];

    #Name of measure
    nm = name_measures[i];

    #best measure
    best.measure = best.measures[[i]];

    #If tolerance was provided, scan the models having an accuracy measure
    #in the range defined by the tolerance and select the best model with the
    #minimum number of coefficients
    if(tolerance!=0){
      #tol
      tol.range = tols.range[[i]];

      #Index of the model
      index_best_model_w_tolerance = which(x = ((measure >= min(tol.range, na.rm=T)) & (measure <= max(tol.range, na.rm=T))) );

      #select one model
      best = which_best(object = trained[index_best_model_w_tolerance],
                        learning.method = learning.method,
                        best.config = best.config,
                        measure = measure[index_best_model_w_tolerance],
                        type.measure = nm);

      #Select
      index_best_model = index_best_model_w_tolerance[best];
    } else {
      #Index of the model
      index_best_model = which( measure == best.measure);

      #Check only one model has been selected
      if(length(index_best_model) > 1){
        best = which_best(object = trained[index_best_model], learning.method = learning.method, best.config = best.config);

        #Select
        index_best_model = index_best_model[best];
      }
    }

    #Store
    out[[nm]] = index_best_model;

  }#END LOOP FOR

  #----------------------------------------------------------------------#

  return(out)
}






#'Which is the best model
#'@description Given a list of models, it returns the best fit according to the
#'criterium that a model with less coefficients is better
#'@param object a list of \code{renoir.trained} objects
#'@param learning.method method for fitting the data
#'@param best.config which configuration to consider
#'@return the index of the best model
#'@keywords internal
# which_best <- function(object, learning.method, best.config = c("min", "1se"), measure, type.measure){
#
#   index = switch(learning.method,
#                  glmnet = which_best_glmnet(object = object, best.config = best.config,
#                                             measure = measure, type.measure = type.measure));
#
#   return(index)
#
# }


#'Which is the best model
#'@description Given a list of models, it returns the best fit
#'@param object a list of \code{renoir.trained} objects
#'@param best.config which configuration to consider
#'@param measure a vector containing accuracy measures
#'@param type.measure the type of accuracy measure
#'@return the index of the best model
#'@keywords internal
which_best_glmnet <- function(object, best.config = c("min", "1se"), measure, type.measure){

  #Get a list of CVM errors
  min.train.error = sapply(X = object, FUN = function(x, best.config) {

    best.config.slot = renoir::get_name_slot_best_config(best.config = best.config)

    cvm = x$fit$cvm[which(x$fit$lambda==x[[best.config.slot]]$lambda)]

    return(cvm)
  }, best.config = best.config)


  #Get name of accuracy measure used in CV from 1st object
  cv.type.measure = names(object[[1]][["fit"]][["name"]])

  #Get the index of model with best CV mean error
  index = which_best_measure(measures = min.train.error, type.measure = cv.type.measure)

  if(length(index)>1){
    #subset
    object = object[index]

    #Get the number of models
    n = length(object);

    #Get a list of the indices of nonzero coefficients
    nz = sapply(X = object, FUN = function(x) {
      nonzero = stats::predict(object = x, best.config = best.config, type = "nonzero")

      #Unlist and select unique indices
      nonzero = unique(unlist(nonzero))

      #Get the number of nonzero elements
      nonzero = if(is.null(nonzero)){0} else {length(nonzero)}

      return(nonzero)
    })

    #Get the index of models with 0 coeff
    i = which(nz == 0)

    #If number of models = number of models with 0 coeff, just return one
    if(n == length(i)){

      if(!missing(measure)){
        #get the measures
        measure.tmp = measure[i]

        #get index of model with best accuracy measure
        index = which_best_measure(measures = measure.tmp, type.measure = type.measure)

        #Be sure just one is selected
        index = index[1];

        #get index
        index = i[index];
      } else {
        index = 1;
      }

    } else {
      #Get the minimum number of coefficients
      min = if(length(i)>0){min(nz[-i], na.rm = TRUE)}else {min(nz, na.rm = TRUE)}

      #Get the index of model with min number of coeff
      i = which(nz == min)

      #if more than one model with min number of coeff
      if(length(i)>1 && !missing(measure)){

        #get the measures
        measure.tmp = measure[i]

        #get index of model with best accuracy measure
        index = which_best_measure(measures = measure.tmp, type.measure = type.measure)

        #Be sure just one is selected
        index = index[1];

        #get index
        i = i[index];
      }

      #Be sure just one is selected
      index = i[1]

    }
  }

  return(index)

}




#'Best model selection
#'@param object object to consider for extracting the best model
#'@param ... further arguments
bm <- function(object, ...){
  UseMethod("bm")
}

#'Best model selection
#'@description This function extracts the best model from a \code{renoir.tat} object
#'given the configuration and the type measure to consider.
#'@param object a \code{renoir.tat} object
#'@param best.config which configuration to consider
#'@param type.measure the performance measure to consider for the selection
#'@param set which set of assessment data to consider for the selection of the best model
#'(\code{'test.set'} by default)
#'@return a list of length 2, containing
#'\describe{
#'   \item{\code{trained}}{the best \code{renoir.trained} model}
#'   \item{\code{itrain}}{a vector containing the observations indices for the resampling iteration}
#'}
#'@keywords internal
bm.renoir.tat <- function(object, best.config = c("min", "1se"),
                          type.measure = c("mse", "deviance", "class", "auc", "mae", "C"),
                          set = c('test.set', 'train.set', 'full.set')){


  best.config = match.arg(best.config);
  type.measure = match.arg(type.measure);
  set = match.arg(set);

  #Get the index of the best model
  index = switch(best.config,
                 'min' = object[["ibest"]][[set]][["best.config.min"]][[type.measure]],
                 '1se' = object[["ibest"]][[set]][["best.config.1se"]][[type.measure]])


  trained = object[["train"]][[index]];
  itrain = object[["itrain"]][[index]];

  return(list(trained = trained, itrain = itrain));

}




#'Best model selection
#'@description This function extracts the best model from a \code{renoir} object
#'given the configuration and the type measure to consider.
#'@param object a \code{renoir} object
#'@param best.config which configuration to consider
#'@param type.measure the performance measure to consider for the selection
#'@param set which set of assessment data to consider for the selection of the best model.
#'The available options are \code{'test.set'} (default), \code{'train.set'} and \code{'full.set'}
#'(which consists of \code{test.set + train.set})
#'@inheritParams update_best_model_selection
#'@return an object of class \code{renoir.best} containing different elements.
#'\describe{
#'   \item{\code{model}}{the best \code{renoir.trained} model}
#'   \item{\code{assess}}{a list of length 2
#'        \describe{
#'            \item{\code{ci}}{a list of confidence intervals for the mean value of each performance measure}
#'            \item{\code{measures}}{a list of performance measures, where each element is a vector of measures}
#'        }
#'   }
#'   \item{\code{precruit}}{a data frame containing the probability of recruitment for each feature of the model}
#'   \item{\code{importance}}{the features importance}
#'   \item{\code{resampling.method}}{the resampling method}
#'   \item{\code{itrain}}{a vector containing the observations indices for the resampling iteration}
#'   \item{\code{best.config}}{the considered configuration}
#'   \item{\code{measure}}{the performance measure considered for the selection}
#'   \item{\code{resp.type}}{the response type}
#'   \item{\code{set}}{the set of data used for the computation of the accuracy measures
#'   considered for the selection of the best training set size and then the best model}
#'   \item{\code{tolerance}}{the tolerance used in the selection of the best model}
#'   \item{\code{confidence}}{the confidence used in the computation of the confidence intervals for the mean
#'   resampling measure of accuracy. The confidence limits are used in the selection
#'   of the best training set size}
#'   \item{\code{call}}{(optional) the arguments of the call that produced this object}
#'}
#'@keywords internal
bm.renoir <- function(object,
                      best.config = c("min", "1se"),
                      type.measure = c("mse", "deviance", "class", "auc", "mae", "C"),
                      tolerance = 0,
                      set = c('test.set', 'train.set', 'full.set')){


  #--------------------------------------------------------------------------------------------#
  #Check input
  type.measure = match.arg(type.measure);
  best.config  = match.arg(best.config);
  set = match.arg(set);

  #--------------------------------------------------------------------------------------------#

  best.config.name = get_name_slot_best_config(best.config = best.config)

  #--------------------------------------------------------------------------------------------#
  update.obj = FALSE;
  if(!missing(tolerance) && check_function_arg(tolerance)){
    #get tolerance used in the computation
    obj.tolerance = object[[get_name_slot_tolerance()]];

    #check if equivalent to tolerance argument, update object otherwise
    if(!identical(x = obj.tolerance, y = tolerance)){update.obj = TRUE}
  }

  if(update.obj){
    object = update_best_model_selection(object = object, tolerance = tolerance, set = set)
  }


  #--------------------------------------------------------------------------------------------#
  #Get best training set size
  ibest = object[[get_name_slot_ibest()]][[set]][[best.config.name]][[type.measure]]

  #--------------------------------------------------------------------------------------------#
  #Get the best training and testing object
  tat = object[[get_name_slot_tat()]][[ibest]];

  #--------------------------------------------------------------------------------------------#
  #Select the best model
  #a) Get the  best model
  model = bm(object = tat, best.config = best.config, type.measure = type.measure, set = set)
  #b) Select assessment measures
  assess = lapply(X = tat[[get_name_slot_tat_assess()]][[set]][[best.config.name]], FUN = function(x){x[[type.measure]]})
  #set the measures name as type.measure
  names(assess) = c("ci", type.measure)
  #c) Select probability of recruitment
  precruit = tat[[get_name_slot_precruitment()]][[best.config.name]]
  #d) Select the features importance
  fimportance.t = object[[get_name_slot_feat_importance()]][[get_name_slot_test_set()]][[get_name_slot_best_config(best.config = best.config)]]
  fimportance.w = object[[get_name_slot_feat_importance()]][[get_name_slot_full_set()]][[get_name_slot_best_config(best.config = best.config)]]
  #e) Select only the features recruited in the best model
  if(is.data.frame(precruit)){
    importance = data.frame(score_t = fimportance.t[[type.measure]][rownames(precruit)],
                            score_w = fimportance.w[[type.measure]][rownames(precruit)])
  } else {
    importance = list()
    for(i in seq(length(precruit))){
      # importance[[i]] = fimportance[[i]][[type.measure]][rownames(precruit[[i]])]
      importance[[i]] = data.frame(score_t = fimportance.t[[i]][[type.measure]][rownames(precruit[[i]])],
                                   score_w = fimportance.w[[i]][[type.measure]][rownames(precruit[[i]])])
    }
    names(importance) = names(precruit)
  }

  #--------------------------------------------------------------------------------------------#
  #Create output object
  out = list(model = model$trained,
             assess = assess,
             precruit = precruit,
             importance = importance,
             resampling.method = object[[get_name_slot_resampling_method()]],
             itrain = model$itrain,
             best.config = best.config,
             measure = type.measure,
             resp.type = object[['resp.type']],
             set = set,
             tolerance = tolerance,
             confidence = object[[get_name_slot_confidence()]])

  class(out) = paste(get_name_package(), "best", sep = ".")

  #--------------------------------------------------------------------------------------------#

  return(out);

}


