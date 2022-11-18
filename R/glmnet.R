#'@keywords internal
get_name_measure = function (type.measure = "mse", resp.type = "gaussian") {

  subclass = switch(resp.type,
                    gaussian    = "elnet",
                    mgaussian   = "mrelnet",
                    binomial    = "lognet",
                    multinomial = "multnet",
                    cox         = "coxnet",
                    poisson     = "fishnet")



  type.measures = c("mse", "deviance", "class", "auc", "mae", "C")

  devname = switch(subclass,
                   elnet = "Mean-squared Error",
                   lognet = "Binomial Deviance",
                   fishnet = "Poisson Deviance",
                   coxnet = "Partial Likelihood Deviance",
                   multnet = "Multinomial Deviance",
                   mrelnet = "Mean-squared Error")
  typename = switch(type.measure,
                    deviance = devname,
                    mse = "Mean-squared Error",
                    mae = "Mean Absolute Error",
                    auc = "AUC",
                    class = "Misclassification Error",
                    C = "C-index")

  return(typename);
}


#'Clean glmnet objects
#'@description This function remove the \code{x} from the \code{call}
#'slots of the trained object. It is needed to avoid that the final
#'object's size raises too much when the initial set of features and observations is big.
#'@param object a glmnet object
#'@return the cleaned glmnet object
#'@keywords internal
clean_glmnet <- function(object, rm.call = F){

  class = class(object);

  if(rm.call){
    object$call = NULL

    if(!is.null(object$relaxed)){
      object$relaxed$call = NULL
    }
  } else {
    if(!identical(x = typeof(object[['call']][['x']]), y = "symbol")){
      if(!is.null(object[['call']][['x']]))
        object[['call']][['x']] = NULL;
    }

    if(!identical(x = typeof(object[['glmnet.fit']][['call']][['x']]), y = "symbol")){
      if(!is.null(object[['glmnet.fit']][['call']][['x']]))
        object[['glmnet.fit']][['call']][['x']] = NULL;
    }

    if(!identical(x = typeof(object[['relaxed']][['call']][['x']]), y = "symbol")){
      if(!is.null(object[['relaxed']][['call']][['x']]))
        object[['relaxed']][['call']][['x']] = NULL;
    }
  }

  class(object) = class;

  return(object)
}



glmnet_blend_relaxed = function (fit, gamma, extend = TRUE, epsilon = 1e-05)
{
  if (gamma == 1) {
    class(fit) = class(fit)[-1]
    fit$relaxed = NULL
    return(fit)
  }
  gamma = max(gamma, epsilon)
  which = match(fit$lambda, fit$relaxed$lambda, 0)
  if (extend)
    which[which == 0] = max(which)
  beta = fit$beta
  a0 = fit$a0
  fitr = fit$relaxed
  betar = fitr$beta
  a0r = fitr$a0
  islistbeta = is.list(beta)
  if (!islistbeta) {
    fitr$beta = gamma * beta[, which > 0, drop = F] + (1 - gamma) * betar[, which, drop = F]
    fitr$a0 = gamma * a0[which > 0] + (1 - gamma) * a0r[which]
  }
  else {
    ngroups = length(beta)
    for (i in 1:ngroups) beta[[i]] = gamma * beta[[i]][, which > 0, drop = F] + (1 - gamma) * betar[[i]][, which, drop = F]
    fitr$beta = beta
    fitr$a0 = gamma * a0[, which > 0, drop = F] + (1 - gamma) * a0r[,which, drop = F]
  }
  fitr$dev.ratio = gamma * fit$dev.ratio[which > 0] + (1 - gamma) * fitr$dev.ratio[which]
  fitr$df = fit$df[which > 0]
  fitr$lambda = fit$lambda[which > 0]
  fitr
}

# glmnet:::predict.cv.glmnet()
# glmnet:::predict.cv.relaxed()
# glmnet:::predict.glmnet()
# glmnet:::predict.relaxed()
# glmnet:::predict.glmnetfit()


#'Predict
#'@description Corrects a bug in blend.relaxed
#'
#'@export predict.relaxed
predict.relaxed = function (object, newx, s = NULL, gamma = 1, type = c("link", "response", "coefficients", "nonzero", "class"), exact = FALSE,
          newoffset, ...)
{
  exact = FALSE
  type = match.arg(type)
  gamma = glmnet:::checkgamma.relax(gamma)
  if (length(gamma) > 1) {
    ng = length(gamma)
    outlist = as.list(length(ng))
    names(outlist(format(round(gamma, 2))))
    for (i in 1:ng) outlist[[i]] = stats::predict(object, newx,
                                           s, gamma = gamma[i], exact, newoffset, ...)
    return(outlist)
  }
  if (gamma == 1)
    return(NextMethod("predict"))
  stats::predict(glmnet_blend_relaxed(object, gamma), newx, s, type, exact,
          newoffset, ...)
}
#
# unlockBinding(sym = "predict.relaxed", env = getNamespace('glmnet'))
# utils::assignInNamespace(x = "predict.relaxed", value = predict.relaxed, ns = asNamespace("glmnet"))


renoir_predict_relaxed = function (object, newx, s = NULL, gamma = 1, type = c("link", "response", "coefficients", "nonzero", "class"), exact = FALSE,
                            newoffset, ...)
{
  exact = FALSE
  type = match.arg(type)
  gamma = glmnet:::checkgamma.relax(gamma)
  if (length(gamma) > 1) {
    ng = length(gamma)
    outlist = as.list(length(ng))
    names(outlist(format(round(gamma, 2))))
    for (i in 1:ng) outlist[[i]] = predict(object, newx,
                                           s, gamma = gamma[i], exact, newoffset, ...)
    return(outlist)
  }
  if (isTRUE(gamma == 1)){

    # index = match(x = class(object), table = "relaxed")
    # index = index[!is.na(index)]
    # if(length(index)>0){
    #   tmp = object
    #   #update
    #   class(tmp) = class(tmp)[-index]
    #   #store
    #   object = tmp
    # }
    #
    # stats::predict()

    return(NextMethod("predict"))
  }

  predict(glmnet_blend_relaxed(object, gamma), newx, s, type, exact,
          newoffset, ...)
}
