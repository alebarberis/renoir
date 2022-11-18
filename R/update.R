#'Update best model selection
#'@param object an object of class \code{renoir}
#'@param tolerance the new tolerance to use for the selection of the best model
#'@param set which set of assessment data to consider for the selection of the best model
#'(\code{'test.set'} by default)
#'@return an updated \code{renoir} object
#'@keywords internal
update_best_model_selection <- function(object,
                                        tolerance = 0,
                                        set = c('test.set', 'train.set', 'full.set')){

  set = match.arg(set)

  #Get tat list
  tat = object[[get_name_slot_tat()]];

  learning.method = object[['learning.method']]

  tat.updated = lapply(X = tat, FUN = function(x){
    #Get list of trained objects
    trained = x[[get_name_slot_tat_train()]]
    #Get list of accuracy measures
    assessed = x[[get_name_slot_tat_assess()]][[set]]
    #update
    ibest = index_best_model_multirandom(object = object, set = set, tolerance = tolerance);
    #store
    x[[get_name_slot_ibest()]][[set]] = ibest;
    #return
    return(x)
  })

  #update
  object[[get_name_slot_tat()]] = tat.updated;
  object[[get_name_slot_tolerance()]] = tolerance;

  #return
  return(object)
}

#'Update best model selection
#'@param object an object of class \code{renoir}
#'@inheritParams get_features_importance
#'@return an updated \code{renoir} object
#'@keywords internal
update_features_importance <- function(object, recruitment.term = c("coefficient", "presence")){

  recruitment.term = match.arg(recruitment.term)

  #get a list of renoir.tat objects
  tat = object[[get_name_slot_tat()]];

  #Compute the new significance score
  fimportance = get_features_importance(object = tat, recruitment.term = recruitment.term);

  #update renoir object
  object[[get_name_slot_feat_importance()]] = fimportance;
  object[[get_name_slot_importance_term()]] = recruitment.term;

  #return
  return(object)
}
