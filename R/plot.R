#'Plot the Evaluation of a Learning Method
#'
#'@description This function creates a plot representing
#'the evaluation of a learning method across different
#'training-set sizes.
#'
#'@details A plot showing the mean performance and the related
#'95\% confidence interval of a learning method
#'across different training-set sizes is produced.
#'The evaluated element is identified by the \code{name}
#'column in the \code{data}. If a unique key is found then
#'\code{\link{plot_single_evaluation}} is dispatched, while if
#'multiple keys are found then the dispatched method is
#'\code{\link{plot_multi_evaluation}}. In the latter case,
#'multiple evaluations are reported in the same plot.
#'
#'@param data \code{data.frame} containing the summary of an object of class
#'\linkS4class{Renoir} as returned by \code{\link{summary_table()}}.
#'This function expects specific columns:
#'\describe{
#'   \item{\code{training_set_size}}{contains the considered training-set sizes}
#'   \item{\code{score}}{contains the performance metric for each model}
#'   \item{\code{mean_score}}{contains the mean performance metric for the specific training-set size}
#'   \item{\code{lower_ci}}{contains the lower bound of the confidence interval for the mean score}
#'   \item{\code{upper_ci}}{contains the upper bound of the confidence interval for the mean score}
#'   \item{\code{best_resample}}{contains the index of the automatically selected optimal training-set size}
#'   \item{\code{best_model}}{contains the index of the best model for the optimal training-set size}
#'   \item{\code{name}}{contains a grouping key, e.g. the learning method}
#'}
#'The \code{name} column is used to identify the
#'number of considered evaluations
#'@param ... further arguments to \code{\link{plot_single_evaluation}}
#'or \code{\link{plot_multi_evaluation}}
#'
#'@return An object of class \code{ggplot}
#'
#'@seealso
#'\code{\link{plot_single_evaluation}},
#'\code{\link{plot_multi_evaluation}}
#'
#'@author Alessandro Barberis
plot_evaluation <- function(data, ...){

  #--------------------------------------------------------------------------------------------#
  #Check if multi
  #--------------------------------------------------------------------------------------------#
  is.multi = length(levels(factor(data$name)))>1

  #--------------------------------------------------------------------------------------------#
  #Plot
  #--------------------------------------------------------------------------------------------#
  if(is.multi){
    p = plot_multi_evaluation(data = data, ...)
  } else {
    p = plot_single_evaluation(data = data, ...)
  }

  #--------------------------------------------------------------------------------------------#
  #return
  return(p)
}

#'Plot the Evaluation of a Learning Method
#'
#'@description This function creates a plot representing
#'the evaluation of a learning method across different
#'training-set sizes.
#'
#'@details A plot showing the mean performance and the related
#'95\% confidence interval of a learning method
#'across different training-set sizes is produced.
#'Individual scores and summary metrics in the form of boxplots
#'can be also added (default) via the \code{add.scores} and
#'\code{add.boxplot} arguments, respectively.
#'
#'@param data \code{data.frame} containing the data to plot. The function expects
#'specific columns:
#'\describe{
#'   \item{\code{training_set_size}}{contains the considered training-set sizes}
#'   \item{\code{score}}{contains the performance metric for each model}
#'   \item{\code{mean_score}}{contains the mean performance metric for the specific training-set size}
#'   \item{\code{lower_ci}}{contains the lower bound of the confidence interval for the mean score}
#'   \item{\code{upper_ci}}{contains the upper bound of the confidence interval for the mean score}
#'   \item{\code{best_resample}}{contains the index of the automatically selected optimal training-set size}
#'   \item{\code{best_model}}{contains the index of the best model for the optimal training-set size}
#'   \item{\code{name}}{contains a grouping key, e.g. the learning method}
#'}
#'@param thr numerical value, if provided it is used to draw an horizontal line
#'@param colour character string, containing the colour for the performance estimate
#'@param add.uncertainty logical, whether to include the quantified uncertainty of the
#'performance estimate in the plot
#'@param add.boxplot logical, whether to include a boxplot in the figure
#'@param colour.box character string, the colour of the boxplot
#'@param add.scores logical, whether to add the performance metric of
#'individual models as points in the plot
#'@param colour.point character string, the colour of the points
#'@param add.best logical, whether to add a point indicating the performance
#'of what is reported as best model in \code{data}
#'@param colour.best character string, the colour of the best model
#'@param shape.best integer, \code{shape} aesthetic passed to \code{\link[ggplot2]{geom_point}}
#'@param size.best integer, \code{size} aesthetic passed to \code{\link[ggplot2]{geom_point}}
#'@param scale.x logical, whether to force the scaling of the x-axis
#'@param title character string, the title of the plot
#'@param subtitle character string, the subtitle of the plot
#'@param caption character string, the caption of the plot
#'@param xlab,ylab character string, axes labels
#'@param ... further arguments to \code{\link[ggplot2]{ggplot}}
#'
#'@return A \code{\link[ggplot2]{ggplot}} object
#'
#'@seealso
#'\code{\link{plot_multi_evaluation}}
#'
#'@author Alessandro Barberis
plot_single_evaluation <- function(
  data,
  thr = NULL,

  #Performance estimate
  colour  = "darksalmon",
  name.ms = 'mean score',
  add.uncertainty = T,

  #Performance metrics
  add.boxplot = T,
  colour.box  = "darkgreen",

  add.scores = T,
  colour.point = "darkgreen",

  #Best model
  add.best = T,
  colour.best = "red",
  shape.best = 22,
  size.best  = 3,

  #Force scale
  scale.x = F,

  #Layout
  title = "Evaluation",
  subtitle = ggplot2::waiver(),
  caption  = ggplot2::waiver(),
  xlab = "Training-set size",
  ylab = "Performance",

  #Further args
  ...
){


  #--------------------------------------------------------------------------------------------#
  #Colours
  #--------------------------------------------------------------------------------------------#
  colours = stats::setNames(object = c(colour, colour.point, colour.best), nm = c(name.ms, "model score", "best model"))

  #--------------------------------------------------------------------------------------------#
  #Initiate visualisation
  p = ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = training_set_size, y = score), ...)

  #--------------------------------------------------------------------------------------------#
  #Add boxplot
  if(add.boxplot){
    p = p + ggplot2::geom_boxplot(alpha = 0, mapping = ggplot2::aes(group = training_set_size), colour = colour.box, show.legend = F)
    # p
  }

  #--------------------------------------------------------------------------------------------#
  #Add jitter
  if(add.scores){
    p = p + ggplot2::geom_jitter(alpha = 0.3, mapping = ggplot2::aes(colour = "model score", group = name), fill = colour.point)
    #p
  }

  #--------------------------------------------------------------------------------------------#
  #Set mean performance metric
  p = p + ggplot2::geom_line(mapping = ggplot2::aes(y = mean_score, colour = "mean score", group = name), show.legend = T)
  # p

  #--------------------------------------------------------------------------------------------#
  #Set confidence
  if(add.uncertainty){
    p = p + ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        ymin = lower_ci,
        ymax = upper_ci
        ,group = name
        # , alpha = 0.9
        # ,fill = "darksalmon95"
        # ,fill = "95% confidence"
      ),
      fill = colour,
      alpha = 0.25
      # ,show.legend = T
    )
    # p
  }

  #--------------------------------------------------------------------------------------------#
  #Add best model
  if(add.best){
    ibest = data$best_resample & data$best_model
    p = p + ggplot2::geom_point(
      data = data,
      mapping = ggplot2::aes(
        x = training_set_size[ibest],
        y = score[ibest],
        colour = "best model",
        group = name),
      shape = shape.best,
      size  = size.best
    )
    #p
  }

  #--------------------------------------------------------------------------------------------#
  #Add threshold
  if(!is.null(thr)){
    p = p + ggplot2::geom_hline(yintercept = thr)
  }

  #--------------------------------------------------------------------------------------------#
  # LEGEND
  #--------------------------------------------------------------------------------------------#
  p = p + ggplot2::scale_color_manual(values = colours, name = "Legend")

  #--------------------------------------------------------------------------------------------#
  # AXIS AND LAYOUT
  #--------------------------------------------------------------------------------------------#
  #Set axes
  p = p + ggplot2::labs(
    title    = title,
    subtitle = subtitle,
    caption  = caption,
    # tag = "Figure 1",
    colour = "Legend",
    x = xlab,
    y = ylab
  )
  # p

  #--------------------------------------------------------------------------------------------#
  #Scale X
  if(scale.x){
    p = p + ggplot2::scale_x_continuous(
      name = xlab,
      labels = as.character(data$training_set_size),
      breaks = data$training_set_size)
  }

  #--------------------------------------------------------------------------------------------#
  #Set theme
  p = p + ggplot2::theme_bw()

  #--------------------------------------------------------------------------------------------#
  #return
  return(p)
}

#'Plot Multiple Evaluations
#'
#'@description This function creates a plot representing
#'multiple evaluations of a learning method across different
#'training-set sizes.
#'
#'@details A plot showing the mean performance and the related
#'95\% confidence interval of learning methods
#'across different training-set sizes is produced.
#'Individual scores and summary metrics in the form of boxplots
#'can be also added (default) via the \code{add.scores} and
#'\code{add.boxplot} arguments, respectively.
#'
#'@inheritParams plot_single_evaluation
#'
#'@return A \code{\link[ggplot2]{ggplot}} object
#'
#'@seealso
#'\code{\link{plot_single_evaluation}}
#'
#'@author Alessandro Barberis
plot_multi_evaluation <- function(
  data,
  thr = NULL,

  #Performance estimate
  # colour = "darksalmon",
  add.uncertainty = T,

  #Performance metrics
  add.boxplot = T,
  # colour.box  = "darkgreen",

  #Jitter
  add.scores = T,
  # colour.point = "darkgreen",

  #Best model
  add.best = T,
  shape.best = 22,
  size.best  = 3,
  # colour.best = "red",

  #Force scale
  scale.x = F,

  #Layout
  title = "Evaluation",
  subtitle = ggplot2::waiver(),
  caption  = ggplot2::waiver(),
  xlab = "Training-set size",
  ylab = "Performance",

  #Further args
  ...
){


  #--------------------------------------------------------------------------------------------#
  #Initiate visualisation
  p = ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = training_set_size, y = score), ...)

  #--------------------------------------------------------------------------------------------#
  #Add boxplot
  if(add.boxplot){
    p = p + ggplot2::geom_boxplot(alpha = 0, mapping = ggplot2::aes(group = paste(training_set_size, name), colour = name), show.legend = F)
    # p
  }

  #--------------------------------------------------------------------------------------------#
  #Add jitter
  if(add.scores){
    # p = p + ggplot2::geom_jitter(alpha = 0.3, mapping = ggplot2::aes(colour = name, group = name))
    p = p + ggplot2::geom_point(alpha = 0.3, mapping = ggplot2::aes(colour = name, group = name))
    #p
  }

  #--------------------------------------------------------------------------------------------#
  #Set mean error
  p = p + ggplot2::geom_line(mapping = ggplot2::aes(y = mean_score, colour = name, group = name), show.legend = T)
  # p

  #--------------------------------------------------------------------------------------------#
  #Set confidence
  if(add.uncertainty){
    p = p + ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        ymin = lower_ci,
        ymax = upper_ci
        ,group = name
        ,fill = name
        # , alpha = 0.9
        # ,fill = "darksalmon95"
        # ,fill = "95% confidence"
      ),
      # fill = key,
      alpha = 0.25
      ,show.legend = F
    )
    # p
  }

  #--------------------------------------------------------------------------------------------#
  #Add best model
  if(add.best){
    ibest = data$best_resample & data$best_model
    p = p + ggplot2::geom_point(
      data = data[ibest,,drop=F],
      mapping = ggplot2::aes(
        # x = training_set_size[ibest],
        # y = error[ibest],
        x      = training_set_size,
        y      = score,
        colour = name,
        group  = name),
      shape = shape.best,
      size  = size.best
    )
    #p
  }

  #--------------------------------------------------------------------------------------------#
  #Add threshold
  if(!is.null(thr)){
    p = p + ggplot2::geom_hline(yintercept = thr)
  }

  #--------------------------------------------------------------------------------------------#
  # LEGEND
  #--------------------------------------------------------------------------------------------#
  # p = p + ggplot2::scale_color_manual(values = colours, name = "Legend")
  p = p + ggplot2::guides(group = FALSE, fill = FALSE)

  #--------------------------------------------------------------------------------------------#
  # AXIS AND LAYOUT
  #--------------------------------------------------------------------------------------------#
  #Set axes
  p = p + ggplot2::labs(
    title    = title,
    subtitle = subtitle,
    caption  = caption,
    # tag = "Figure 1",
    colour = "Legend",
    x = xlab,
    y = ylab
  )
  # p

  #--------------------------------------------------------------------------------------------#
  #Scale X
  if(scale.x){
    p = p + ggplot2::scale_x_continuous(
      name = xlab,
      labels = as.character(data$training_set_size),
      breaks = data$training_set_size)
  }

  #--------------------------------------------------------------------------------------------#
  #Set theme
  p = p + ggplot2::theme_bw()

  #--------------------------------------------------------------------------------------------#
  #return
  return(p)
}

create_dataframe_to_plot <- function(evaluatedlist, learner, ...){

  #--------------------------------------------------------------------------------------------#
  #Learning methods
  ids  = unlist(get_id(evaluatedlist))
  lids = unlist(get_id(learner))

  #--------------------------------------------------------------------------------------------#
  out = list()
  for(id in unique(ids)){
    #create
    if(!missing(learner) && is.Learner(learner) && isTRUE(id %in% lids)){
      out[[id]] = create_dataframe_to_plot_default(evaluatedlist = evaluatedlist[ids == id], learner = learner, ...)
    } else if(!missing(learner) && is.LearnerList(learner) && isTRUE(id %in% lids)){
      out[[id]] = create_dataframe_to_plot_default(evaluatedlist = evaluatedlist[ids == id], learner = learner[lids==id], ...)
    } else {
      out[[id]] = create_dataframe_to_plot_default(evaluatedlist = evaluatedlist[ids == id], ...)
    }

  }

  #--------------------------------------------------------------------------------------------#
  #Bind
  out = do.call(what = rbind, args = c(out, make.row.names = F, stringsAsFactors = F))

  #--------------------------------------------------------------------------------------------#
  return(out)
}

#Creates a dataframe from a list of Evaluated objects,
#having same response type. For each learning method/config
#a best training set size is computed
create_dataframe_to_plot_default <- function(
  evaluatedlist,
  measure,
  set,
  confidence = 0.95,
  distribution = "normal",
  learner,
  evaluator,
  show.config = F,
  key,
  ...){

  #--------------------------------------------------------------------------------------------#
  # #get size
  # x.plot = get_n(evaluatedlist)
  #
  # #order
  # ordered = order(x.plot, decreasing = F)
  #
  # #force order
  # evaluatedlist = evaluatedlist[ordered]

  #--------------------------------------------------------------------------------------------#
  #Extract data
  #--------------------------------------------------------------------------------------------#
  if(missing(key)){key = identify_primary_key_from_evaluated_objects(evaluatedlist)}

  data = lapply(X = evaluatedlist, FUN = create_dataframe_from_evaluated_object,
                set = set, measure = measure, confidence = confidence, distribution = distribution,
                learner = learner, show.config = show.config, key = key, ...)

  #--------------------------------------------------------------------------------------------#
  #reshape
  out = do.call(what = rbind, args = data)

  #--------------------------------------------------------------------------------------------#
  # BEST RESAMPLE
  #--------------------------------------------------------------------------------------------#
  #best
  ibest = which_best(
    object    = evaluatedlist,
    set       = set,
    measure   = measure,
    evaluator = evaluator,
    confidence = confidence,
    distribution = distribution
  )
  #as logical
  ibest = out$training_set_size == unique(out$training_set_size)[ibest]
  #update
  out$best_resample = ibest

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)

}


create_dataframe_from_evaluated_object <- function(
  evaluated, set, measure, confidence, distribution,
  learner, show.config = FALSE,
  key = c("id", "config", "response", "sampling"),
  best = c("opt", "1se")
){

  #--------------------------------------------------------------------------------------------#
  #match
  best = match.arg(best)

  #--------------------------------------------------------------------------------------------#
  #key
  if(missing(key)){key = "id"}
  key = create_key_from_evaluated_object(object = evaluated, key = key)

  #--------------------------------------------------------------------------------------------#
  #get training set size
  n = get_n(evaluated)

  #get repeats
  k = get_k(evaluated)

  #--------------------------------------------------------------------------------------------#
  #Get accuracy
  testedlist = get_performance(evaluated)
  #subset
  tested = subset_list(object = testedlist, set = set, measure = measure)

  #get error
  err  = get_score(tested)
  #Get mean error
  merr = get_mscore(tested)
  #Get standard error
  se   = get_sem(tested)

  #compute confidence interval
  tmp = ci(estimate = merr, se = se, confidence = confidence, distribution = distribution, n = k)


  #--------------------------------------------------------------------------------------------#
  # MODELS
  #--------------------------------------------------------------------------------------------#
  models  = get_models(evaluated)
  imodels = seq(length(models))
  #--------------------------------------------------------------------------------------------#
  # BEST MODEL
  #--------------------------------------------------------------------------------------------#
  ibest = switch(
    best,
    'opt' = get_opt(tested),
    '1se' = get_1se(tested)
  )

  ibest = imodels == ibest

  #--------------------------------------------------------------------------------------------#
  # TOOLTIP
  #--------------------------------------------------------------------------------------------#


  #create text for tooltip
  txt = sapply(X = imodels, FUN = function(i, models, ...){create_text_for_tooltip(model = models[[i]], index = i,...)},
               learner = learner, show.config = show.config, models = models)

  #--------------------------------------------------------------------------------------------#
  # DATAFRAME
  #--------------------------------------------------------------------------------------------#
  #out
  out = data.frame(
    key               = key,
    measure           = measure,
    training_set_size = n,
    k                 = length(err),
    imodel            = imodels,
    best_model        = ibest,
    score             = err,
    mean_score        = merr,
    standard_error    = se,
    upper_ci          = tmp['up'],
    lower_ci          = tmp['low'],
    text              = txt,
    stringsAsFactors  = F,
    row.names         = NULL
  )

  #--------------------------------------------------------------------------------------------#
  return(out)
}

#'@param model a \linkS4class{Tuned} or a \linkS4class{Trained} object
create_text_for_tooltip <- function(model, learner, show.config = FALSE, index = NULL){

  #--------------------------------------------------------------------------------------------#
  #Screening and Model
  if(is.Tuned(model)){
    #get screened
    screened = get_screened(model)
    #get number of features
    screened = length(get_index(screened))

    if(screened>0){
      #set text
      screened = paste("screened:", screened)
    } else {
      screened = NULL
    }

    #get trained
    model = get_model(model)
  } else {
    screened = NULL
  }

  #--------------------------------------------------------------------------------------------#
  #Number of used predictors
  # if(!missing(learner)){
  #   #recorder
  #   recorder = get_recorder(learner)
  #   #get recorded
  #   recorded = record(object = model, recorder = recorder)
  #   #unlist
  #   recorded = unlist(recorded)
  #   if(all(!is.na(unique(recorded)))){
  #     #remove NA
  #     recorded = recorded[!is.na(recorded)]
  #     #remove 0
  #     recorded = recorded[recorded!=0]
  #     #np
  #     np = length(recorded)
  #   } else {
  #     np = NA
  #   }
  # } else {
  #   np = NA
  # }
  if(!missing(learner)){
    np = nfeatures(object = model, recorder = get_recorder(learner))
  } else{
    np = get_nfeatures(model)
  }
  #text
  np = paste("nvar:", np)

  #--------------------------------------------------------------------------------------------#
  #Configuration
  if(show.config){
    config = get_config(model)
    config.names = names(config);
    config = paste(sapply(X = config.names, FUN = function(name, config){paste0(name, ": ", config[[name]])}, config = config), collapse = "<br>");
    config = paste("Configuration", config, sep = "<br>")
  } else {
    config = NULL
  }

  #--------------------------------------------------------------------------------------------#
  #Index
  if(!missing(index) && !S4Vectors::isEmpty(index)){
    index = paste("repeat:", index)
  } else {
    index = NULL
  }

  #--------------------------------------------------------------------------------------------#
  #output
  out = c(index, screened, np, config)
  #check
  out = out[!is.null(out)]
  #paste
  out = paste(out, collapse = "<br>")
  # out = paste(index, screened, np, config, sep = "<br>")

  #--------------------------------------------------------------------------------------------#
  return(out)
}

identify_primary_key_from_evaluated_objects <- function(object){

  #--------------------------------------------------------------------------------------------#
  #Default
  out = "id"

  #--------------------------------------------------------------------------------------------#
  #Extract data
  # id       = get_id(object)
  config   = get_config(object)
  response = get_response(object)
  sampling = get_sampling(object)

  #--------------------------------------------------------------------------------------------#
  #Check config
  if(!isTRUE(all(S4Vectors::isEmpty(config)))){
    config = unique(unlist(config))
    #rm empty
    config = config[!(S4Vectors::isEmpty(config))]
    #check
    if(length(config)>1){out = c(out, "config")}
  }

  #--------------------------------------------------------------------------------------------#
  #Check response
  response = unique(unlist(response))
  #check
  if(length(response)>1){out = c(out, "response")}

  #--------------------------------------------------------------------------------------------#
  #Check sampling
  sampling = unique(unlist(sampling))
  #check
  if(length(sampling)>1){out = c(out, "sampling")}

  #--------------------------------------------------------------------------------------------#
  return(out)
}

create_key_from_evaluated_object <- function(
  object,
  key = c("id", "config", "response", "sampling")
  ){

  #--------------------------------------------------------------------------------------------#
  #key
  if(missing(key)){key = "id"}

  id       = get_id(object)
  config   = get_config(object)
  response = get_response(object)
  sampling = get_sampling(object)

  #--------------------------------------------------------------------------------------------#
  #build key
  out = id

  if(("config" %in% key) && !S4Vectors::isEmpty(config)){
    out = paste0(out, " (",config, ")")
  }

  if("response" %in% key){
    out = paste0(out, " - ",response)
  }

  if("sampling" %in% key){
    out = paste0(out, " - ",sampling)
  }
  #--------------------------------------------------------------------------------------------#
  return(out)
}




create_dataframe_to_plot_for_single_learning_method <- function(
  evaluatedlist,
  measure,
  set,
  confidence = 0.95,
  distribution = "normal",
  learner,
  evaluator,
  show.config = F,
  key,
  ...){

  #--------------------------------------------------------------------------------------------#
  #get size
  x.plot = get_n(evaluatedlist)

  #order
  ordered = order(x.plot, decreasing = F)

  #force order
  evaluatedlist = evaluatedlist[ordered]

  #--------------------------------------------------------------------------------------------#
  #Extract data
  #--------------------------------------------------------------------------------------------#
  if(missing(key)){key = identify_primary_key_from_evaluated_objects(evaluatedlist)}

  data = lapply(X = evaluatedlist, FUN = create_dataframe_from_evaluated_object,
                set = set, measure = measure, confidence = confidence, distribution = distribution,
                learner = learner, show.config = show.config, key = key, ...)

  #--------------------------------------------------------------------------------------------#
  #reshape
  out = do.call(what = rbind, args = data)

  #--------------------------------------------------------------------------------------------#
  # BEST RESAMPLE
  #--------------------------------------------------------------------------------------------#
  ibest = which_best(
    object    = evaluatedlist,
    set       = set,
    measure   = measure,
    evaluator = evaluator)

  #as logical
  ibest = out$training_set_size == unique(out$training_set_size)[ibest]

  #update
  out$best_resample = ibest

  #--------------------------------------------------------------------------------------------#
  #return
  return(out)

}

#'@param tested a \linkS4class{TestedList} objects
extract_accuracy_to_plot <- function(testedlist){

  data = lapply(X = testedlist, FUN = function(tested){

    #get error
    err  = get_score(tested)
    #Get mean error
    merr = get_mscore(tested)
    #Get standard error
    se   = get_sem(tested)



    list(acc.ci   = acc.ci,
         measures = measures,
         repeats = length(measures),
         ncoeffs = nz_coeffs,
         configs = lapply(x[[get_name_slot_tat_train()]], FUN = function(obj){obj[[get_name_slot_best_config(best.config)]]})
    )
  }, type.measure = type.measure)

}


plot.EvaluatedList <- function(
  object,
  measure,
  set,
  confidence = 0.95,
  distribution = "normal",
  xlim = NULL, ylim = NULL,
  ...){

  #get size
  x.plot = get_n(object)

  #order
  ordered = order(x.plot, decreasing = F)

  #force order
  object = object[ordered]
  #--------------------------------------------------------------------------------------------#
  #X axis
  #--------------------------------------------------------------------------------------------#
  x.plot = x.plot[ordered]

  #--------------------------------------------------------------------------------------------#
  #Y axis
  #--------------------------------------------------------------------------------------------#
  #Get performance
  performance = get_performance(object)
  #subset
  performance = lapply(X = performance, FUN = subset_list, set = set, measure = measure)

  #Get mean error
  merr = sapply(performance, FUN = get_mscore)
  #Get standard error
  se = sapply(performance, FUN = get_sem)
  #Get N
  k  = sapply(object, FUN = get_k)

  #get
  ci.low = ci.up = c()
  for(i in seq(length(merr))){
    tmp = ci(estimate = merr[i], se = se[i], confidence = confidence, distribution = distribution, n = k[i])
    ci.low = c(ci.low, tmp['low'])
    ci.up  = c(ci.up,  tmp['up'])
  }

  #Create lines
  y.plot = list(y.plot.low = ci.low,
                y.plot.mean = merr,
                y.plot.up = ci.up)

  #--------------------------------------------------------------------------------------------#
  #axis range
  xrange = if(!is.null(xlim)){xlim}else{range(x.plot)}
  yrange = get_yrange(y = y.plot, ylim = ylim, measure = measure)

  #--------------------------------------------------------------------------------------------#
  #Get name
  ylab = get_measure_names(type.measure = measure)

  #--------------------------------------------------------------------------------------------#
  #Setup the plot
  plot(x = xrange, y = yrange, type="n", xlab="Training-set size", ylab=ylab, ...)

  if(identical(measure, "class")){
    #Get the number of classes
    nc = get_nout(object)
    #Compute the threshold
    thr = (nc - 1)/nc;

    #If plot is for logistic model, add line at threshold (e.g. is 0.5 for binomial)
    graphics::abline(h = thr, lty = "dashed", lwd = 1, col = "black")
  } else if(identical(measure, "auc")){
    graphics::abline(h = 0.5, lty = "dashed", lwd = 1, col = "black")
  }


  colors = c("darksalmon", "darkgreen", "darksalmon")

  # add lines
  for (i in 1:3) {
    graphics::lines(x = x.plot, y = y.plot[[i]], type="b", lwd=2,col=colors[i])
    # lty=linetype[i], col=colors[i], pch=plotchar[i])
  }
}

get_yrange <- function(y, ylim = NULL, measure){

  measures_w_fixed_range = c("class", "auc")

  if(!is.null(ylim)){
    out = ylim
  }else{
    if(measure %in% measures_w_fixed_range){
      out = c(0, 1)
    } else {
      out = range(y, na.rm = T);
    }
  }
  return(out)
}

#
# #Plot the training and validate object
# #@param object object of class \code{"renoir"}, obtained from
# #\code{\link{run}}
# #@param best.config which configuration to consider
# #@param type.measure the performance measure to consider
# #
# #@param ... further arguments to \code{\link[graphics]{plot}}
# plot.renoir <- function(object,
#                         best.config = c("min", "1se"),
#                         type.measure = c("mse", "deviance", "class", "auc", "mae", "C"),
#                         set = c('test.set', 'train.set', 'full.set'),
#                         ...){
#
#
#
#   args = list(...);
#
#   #--------------------------------------------------------------------------------------------#
#   #Check input
#   type.measure = match.arg(type.measure);
#   best.config  = match.arg(best.config);
#   set = match.arg(set);
#
#
#   #--------------------------------------------------------------------------------------------#
#
#   #Extract the training set sizes
#   train.sizes = object[[get_name_slot_train_sizes()]]
#
#   #Extract the accuracy
#   acc.ci = lapply(X = object[[get_name_slot_tat()]], FUN = function(x){x[[get_name_slot_tat_assess()]][[set]][[get_name_slot_best_config(best.config)]][[get_name_slot_tat_ci()]][[type.measure]]})
#
#   if(is.null(unique(unlist(acc.ci)))){
#     stop("The accuracy measures for the selected 'type.measure' is not present in the input object. Probably, it was not computed. Try with a different one.")
#   } else {
#     #Extract the name of the measure
#     name = get_name_measure(type.measure = type.measure, resp.type = object$resp.type)
#
#     #--------------------------------------------------------------------------------------------#
#
#
#     #Create mean line
#     x.plot = train.sizes
#     y.plot.mean = sapply(acc.ci, FUN = function(x){x$mean})
#
#     #Create upper line
#     y.plot.up = sapply(acc.ci, FUN = function(x){x$up})
#
#     #Create lower line
#     y.plot.low = sapply(acc.ci, FUN = function(x){x$low})
#
#     y.plot = list(y.plot.low = y.plot.low,
#                   y.plot.mean = y.plot.mean,
#                   y.plot.up = y.plot.up)
#
#     #-------------------------------------------------------------------#
#     # if(identical(x = type.measure, y =  "mse")){
#     #   #Take the square root
#     #   y.plot = lapply(X = y.plot, FUN = sqrt)
#     #   #Update the name
#     #   name = "RMSE";
#     # }
#
#     #-------------------------------------------------------------------#
#
#     xlim = args[["xlim"]]
#     ylim = args[["ylim"]]
#
#     xrange = if(!is.null(xlim)){xlim}else{range(x.plot)}
#     yrange = if(!is.null(ylim)){ylim}else{
#
#       if(identical(name, "Misclassification Error") || identical(name, "AUC")){
#         c(0, 1)
#         # yrange = c(0, max(0.7, max(yrange)))
#       } else {
#         range(y.plot, na.rm = T);
#       }
#     }
#
#     #-------------------------------------------------------------------#
#     #Setup the plot
#     plot(x = xrange, y = yrange, type="n", xlab="Training-set size", ylab=name, ...)
#
#     if(identical(name, "Misclassification Error")){
#       #Get the number of classes
#       nc =  if(is.null(dim(object$y))){length(levels(as.factor(object$y)))}else {ncol(object$y)}
#       #Compute the threshold
#       thr = (nc - 1)/nc;
#
#       #If plot is for logistic model, add line at threshold (e.g. is 0.5 for binomial)
#       graphics::abline(h = thr, lty = "dashed", lwd = 1, col = "black")
#     } else if(identical(name, "AUC")){
#       graphics::abline(h = 0.5, lty = "dashed", lwd = 1, col = "black")
#     }
#
#
#     colors = c("darksalmon", "darkgreen", "darksalmon")
#
#     # add lines
#     for (i in 1:3) {
#       graphics::lines(x = x.plot, y = y.plot[[i]], type="b", lwd=2,col=colors[i])
#       # lty=linetype[i], col=colors[i], pch=plotchar[i])
#     }
#   }
#
# }
#
#
#
# #@keywords internal
# adj_plot = function(object, type.measure, set = c('test.set', 'train.set', 'full.set')){
#
#   best.config = c("min", "1se")
#   set = match.arg(set);
#
#   if(identical(type.measure, "auc") || identical(type.measure, "class")){
#     plot(object = object, best.config = best.config[1], type.measure = type.measure, set = set, main = paste0("Best configuration (", best.config[1], ")"));
#     plot(object = object, best.config = best.config[2], type.measure = type.measure, set = set, main = paste0("Best configuration (", best.config[2], ")"));
#   } else {
#     ylim = range(sapply(X = object[[get_name_slot_tat()]], FUN = function(x){x[[get_name_slot_tat_assess()]][[set]][[get_name_slot_best_config(best.config[1])]][[get_name_slot_tat_ci()]][[type.measure]]}),
#                  sapply(X = object[[get_name_slot_tat()]], FUN = function(x){x[[get_name_slot_tat_assess()]][[set]][[get_name_slot_best_config(best.config[2])]][[get_name_slot_tat_ci()]][[type.measure]]}),
#                  na.rm = TRUE)
#
#     # if(identical(type.measure, "mse")){ylim = sqrt(x = ylim)}#because we plot RMSE
#
#     ylim[1] = ylim[1] - 0.1
#     ylim[2] = ylim[2] + 0.1
#
#     plot(object = object, best.config = best.config[1], type.measure = type.measure, set = set, main = paste0("Best configuration (", best.config[1], ")"), ylim = ylim);
#     plot(object = object, best.config = best.config[2], type.measure = type.measure, set = set, main = paste0("Best configuration (", best.config[2], ")"), ylim = ylim);
#
#   }
#
# }
#
# #Save accuracy plots
# #@param object object of class \code{"renoir"}, obtained from
# #\code{\link{run}}
# #@param y outcome variable
# #@param outdir directory where to store the output
# #@param filename the name of the file
# save_plots <- function(object, type.measure, set = c('test.set', 'train.set', 'full.set'), outdir, filename = "multir_accuracy"){
#
#   set = match.arg(set);
#
#   #If type.measure is not provided, we use all the supported ones
#   if(missing(type.measure)){type.measure = get_available_measures(resp.type = object[['resp.type']])}
#
#
#   grDevices::pdf(file = file.path(outdir, paste0(filename, ".pdf")))
#
#   for(measure in type.measure){
#     adj_plot(object = object, type.measure = measure, set = set)
#   }
#
#   grDevices::dev.off()
# }
#
# #Save accuracy plots
# #@param object object of class \code{"renoir"}, obtained from
# #\code{\link{run}}
# #@param outdir directory where to store the output
# #@param filename the name of the file
# save_plots_2xpage <- function(object, type.measure, set = c('test.set', 'train.set', 'full.set'), outdir, filename = "multir_accuracy_2xpage"){
#
#   set = match.arg(set);
#
#   if(missing(type.measure)){type.measure = get_available_measures(resp.type = object[['resp.type']])}
#
#
#   grDevices::pdf(file = file.path(outdir, paste0(filename, ".pdf")), width = 14)
#
#   par(mfrow=c(1,2))
#
#   for(measure in type.measure){
#     adj_plot(object = object, type.measure = measure, set = set)
#   }
#
#   grDevices::dev.off()
# }
#
#
#
#
#
#
#
#
# #Plot the accuracy measure over lambda values for each alpha
# #@param object object of class \code{"resample.glmnet"}, obtained from
# #\code{\link{tat_model_multirandom}}
# #@param cvm measure to select ("minimum" or "1 standard error from minimum")
# plot.resample.glmnet <- function(object, cvm = c("min", "1se")){
#
#   cvm = cvm[1];
#
#   #1) Extract the parameters from the input object
#   params = lapply(X = object$resample.res, FUN = function(x){x$train.res$params})
#
#   #2) Get the accuracy measure name
#   measure.name = names(object$type.measure)
#
#   #2b) Set the axis names
#   xlab = paste0("log(Lambda ",cvm,")");
#   ylab = measure.name;
#
#   #3) select unique values of alpha
#   alpha.min = sort(unique(sapply(X = params, FUN = function(x){x$alpha.min})))
#
#   # grDevices::pdf(file = file.path(outdir, "cv_min_parameters_acc.pdf"))
#   if(length(alpha.min)>1){
#     num_cols = 3;
#     num_rows = ceiling(length(alpha.min)/num_cols);
#     graphics::par(mfrow=c(num_rows,num_cols))
#   }
#
#   #) Extract the average error for each alpha and lambda.min
#   alpha.i=alpha.min[1]#debug
#   for(alpha.i in alpha.min){
#     cv = sapply(X = params, FUN = function(x, alpha){ if(x$alpha.min==alpha){
#                                                             c(acc    = x[[paste("cvm", cvm, sep=".")]],
#                                                               lambda = x[[paste("lambda", cvm, sep=".")]])
#                                                           }else{
#                                                             NA
#                                                           }
#                                                         }, alpha=alpha.i)
#     cv = cv[which(!is.na(cv))];
#
#     #Create a matrix
#     cv.m = do.call(what = rbind, args = cv)
#
#     #Plot the data
#     graphics::boxplot(acc ~ round(log(lambda), 4), data=cv.m,
#                       xlab = xlab, ylab=ylab,
#                       main=paste0("alpha = ", round(alpha.i, 4)),
#                       show.names = TRUE)#otherwise is not plotting x labels when n = 1
#
#   }
#
#   #2)
#   # grDevices::dev.off(); # to reset the graphics pars to defaults
# }
#
#
#
#
# #Plot the accuracy measure over lambda values for each alpha
# #@param object object of class \code{"tune_glmnet"}, obtained from
# #\code{\link{tune_glmnet}}
# plot.tune.glmnet <- function(object){
#
#   #Get alphas
#   alphas = object$params$alpha
#
#   #Set some graphics parameters
#   if(length(alphas)>1){
#     old.pars = graphics::par()
#
#     num_cols = 2;
#     num_rows = ceiling(length(alphas)/num_cols);
#
#     graphics::par(mfrow=c(num_rows,num_cols), mar = c(4, 4, 3, 1))
#   }
#
#   #Get the fits
#   cv.fits = object$cv.fits;
#
#   #Plot the cv fits
#   cv.fit = cv.fits[[1]]
#   for(cv.fit in cv.fits){
#     alpha.i = cv.fit$alpha;
#     fit = cv.fit$cv.glmnet;
#
#     plot(x = fit, main=paste0("alpha = ", alpha.i))
#
#   }
# }



#'Plot PCA
#'@param PCA object as returned by \code{\link[stats]{prcomp}}
#'@param groups data frame containing the groups (each column is a different group)
#'@param title plot title
#'@param grid.arrange.ncol number of columns to arrange plots in final figure
#'@param palette (optional) when a single value,
#'if a string, it is considered as a name of palette;
#'if a number, it is the index into the lists of palettes of appropriate type.
#'If \code{palette} is a single (unnamed) value, will be used for all the \code{groups}.
#'If is a named vector, each element matching a column in \code{groups} will be used.
#'The parameter is passed to \code{\link[ggplot2]{scale_colour_brewer}}
#'@param palette.type palette type. One of seq (sequential), div (diverging) or qual (qualitative)
#'@param order the order to force before arranging the list of plots in the grid.
#'If order is set to \code{order = 'auto'} and \code{grid.arrange.ncol=2}, the resulting grid
#'has PC components from same group across the columns,
#'while the same PC components for different groups are reported across the rows
#'@param width,height size of each plot in inches
#'@inheritParams gridExtra::arrangeGrob
#'@param ... further arguments to \code{\link[gridExtra]{arrangeGrob}}
#'@return a \code{ggplot2} object
#'@keywords internal
#'@author Alessandro Barberis
plot_PCA <- function(PCA,
                     groups,
                     title = '',
                     grid.arrange.ncol = 1,
                     palette = NULL,
                     palette.type = "qual",
                     order = NULL,
                     width = NULL,
                     height = NULL,
                     ...){

  #Compute variance percentage
  percentVar <- round(100*PCA$sdev^2/sum(PCA$sdev^2),1)

  npc = ncol(PCA$x)

  if(npc>1){
    #store rownames
    rnames = rownames(PCA$x);

    #create df
    if(!is.null(rnames)){
      df = data.frame(PC1 = PCA$x[rnames,1],
                      PC2 = PCA$x[rnames,2],
                      groups[rnames,,drop=F],
                      stringsAsFactors = FALSE)
    } else {
      df = data.frame(PC1 = PCA$x[,1],
                      PC2 = PCA$x[,2],
                      groups[,,drop=F],
                      stringsAsFactors = FALSE)
    }



    if(npc>3){
      if(!is.null(rnames)){
        df = data.frame(df,
                        PC3 = PCA$x[rnames,3],
                        PC4 = PCA$x[rnames,4],
                        stringsAsFactors = FALSE)
      } else {
        df = data.frame(df,
                        PC3 = PCA$x[,3],
                        PC4 = PCA$x[,4],
                        stringsAsFactors = FALSE)
      }

      n_plots_per_group = 2
    } else {
      n_plots_per_group = 1
    }

    #get n of groups
    n_groups = ncol(groups)

    #Create a list to store results
    p1 = p2 = list()

    for(i in seq(n_groups)){

      #group name
      group_name = names(groups)[i]
      #group name as variable
      col <- ggplot2::sym(x = group_name)#used to parametrise the color = !!col

      #When we assign a categorical variable to the color argument,
      #ggplot2 automatically assigns a different color to each category
      #and also adds a legend

      p1[[i]] <- ggplot2::ggplot(data = df, ggplot2::aes(x = PC1, y = PC2, color = !!col)) +
        # ggplot2::geom_point(size = I(2)) +
        ggplot2::geom_point() +
        ggplot2::labs(title="PC1 vs PC2",
                      # subtitle = subtitle,
                      x = paste0("PC1, VarExp:", round(percentVar[1],4)),
                      y = paste0("PC2, VarExp:", round(percentVar[2],4)))
      # + scale_colour_brewer(type="qual", palette=4)

      if(!is.null(palette)){
        palette_names = names(palette)

        if(!is.null(palette_names)){
          color_palette = palette[group_name];

          if(!is.null(color_palette) & !is.na(color_palette)){
            p1[[i]] <- p1[[i]] + ggplot2::scale_colour_brewer(type = palette.type,
                                                              palette = color_palette)
          }
        } else {
          color_palette = palette;

          p1[[i]] <- p1[[i]] + ggplot2::scale_colour_brewer(type = palette.type,
                                                            palette = color_palette)
        }
      }

      if(npc>3){
        p2[[i]] <- ggplot2::ggplot(data = df, ggplot2::aes(x = PC3, y = PC4, color = !!col)) +
          # ggplot2::geom_point(size = I(2)) +
          ggplot2::geom_point() +
          ggplot2::labs(title="PC3 vs PC4",
                        # subtitle = subtitle,
                        x = paste0("PC3, VarExp:", round(percentVar[3],4)),
                        y = paste0("PC4, VarExp:", round(percentVar[4],4)))

        if(!is.null(palette)){
          palette_names = names(palette)

          if(!is.null(palette_names)){
            color_palette = palette[group_name];

            if(!is.null(color_palette) & !is.na(color_palette)){
              p2[[i]] <- p2[[i]] + ggplot2::scale_colour_brewer(type = palette.type,
                                                                palette = color_palette)
            }
          } else {
            color_palette = palette;

            p2[[i]] <- p2[[i]] + ggplot2::scale_colour_brewer(type = palette.type,
                                                              palette = color_palette)
          }
        }

      }

    }

    #----------------------------------------------------------------------#
    p = c(p1, p2)

    #----------------------------------------------------------------------#
    if(!is.null(order)){
      if(identical(grid.arrange.ncol, 2) & identical(order, "auto")){
        if(n_plots_per_group>1){
          order = c(rbind(seq(from = 1, to = n_groups, by = 1),seq(from = n_groups+1, to = n_groups*n_plots_per_group, by = 1)))
        } else {
          order = seq(from = 1, to = n_groups, by = 1)
        }
        p = p[order]
      } else {
        if(is.numeric(order)){
          p = p[order]
        }
      }

    }


    #----------------------------------------------------------------------#

    grid.arrange.nrow = ceiling((n_groups*n_plots_per_group)/grid.arrange.ncol);

    #----------------------------------------------------------------------#
    #set heights and widths
    l = list(...)
    h = l[['heights']]
    w = l[['widths']]

    if(is.null(h) && !is.null(height)){
      l[['heights']] = rep(x = grid::unit(x = height, units = "inches"), times = grid.arrange.nrow)
    }

    if(is.null(w) && !is.null(width)){
      l[['widths']] = rep(x = grid::unit(x = width, units = "inches"), times = grid.arrange.ncol)
    }

    #----------------------------------------------------------------------#
    # out = gridExtra::grid.arrange(grobs = p,
    out = do.call(what = gridExtra::arrangeGrob,
                  args = c(list(grobs = p,
                                ncol = grid.arrange.ncol,
                                nrow = grid.arrange.nrow,
                                top = title),
                           l)
    )
  } else {
    out = NULL
  }



  return(out)

}


#'@param PCA object as returned by \code{\link[stats]{prcomp}}
#'@param groups data frame with 1 column containing the groups
#'@param pc vector with 2 elements, the principal components to plot
#'@param title plot title
#'@param palette (optional) when a single value,
#'if a string, it is considered as a name of palette;
#'if a number, it is the index into the lists of palettes of appropriate type.
#'The parameter is passed to \code{\link[ggplot2]{scale_colour_brewer}}
#'@param palette.type palette type. One of seq (sequential), div (diverging) or qual (qualitative)
#'@keywords internal
plot_pca <- function(PCA,
                     groups,
                     pc = c(1, 2),
                     title = NULL,
                     palette = NULL,
                     palette.type = "qual"
                     ){

  #Compute variance percentage
  percentVar <- round(100*PCA$sdev^2/sum(PCA$sdev^2),1)

  npc = ncol(PCA$x)

  if(npc>1 && all(is.element(el = pc, set = seq(npc)))){
    #store rownames
    rnames = rownames(PCA$x);

    #create df
    if(!is.null(rnames)){
      df = data.frame(PC1 = PCA$x[rnames,pc[1]],
                      PC2 = PCA$x[rnames,pc[2]],
                      groups[rnames,,drop=F],
                      stringsAsFactors = FALSE)
    } else {
      df = data.frame(PC1 = PCA$x[,pc[1]],
                      PC2 = PCA$x[,pc[2]],
                      groups[,,drop=F],
                      stringsAsFactors = FALSE)
    }


    #get n of groups
    n_groups = ncol(groups)


    #group name
    group_name = names(groups)[1]
    #group name as variable
    col <- ggplot2::sym(x = group_name)#used to parametrise the color = !!col

    #When we assign a categorical variable to the color argument,
    #ggplot2 automatically assigns a different color to each category
    #and also adds a legend

    out <- ggplot2::ggplot(data = df, ggplot2::aes(x = PC1, y = PC2, color = !!col)) +
      # ggplot2::geom_point(size = I(2)) +
      ggplot2::geom_point() +
      ggplot2::labs(title= if(!is.null(title)){title}else{paste0("PC", pc[1], " vs PC", pc[2])},
                    # subtitle = subtitle,
                    x = paste0("PC",pc[1],", VarExp:", round(percentVar[pc[1]],4)),
                    y = paste0("PC",pc[2],", VarExp:", round(percentVar[pc[2]],4)))
    # + scale_colour_brewer(type="qual", palette=4)

    if(!is.null(palette)){
      palette_names = names(palette)

      if(!is.null(palette_names)){
        color_palette = palette[group_name];

        if(!is.null(color_palette) & !is.na(color_palette)){
          out <- out + ggplot2::scale_colour_brewer(type = palette.type,
                                                            palette = color_palette)
        }
      } else {
        color_palette = palette;

        out <- out + ggplot2::scale_colour_brewer(type = palette.type,
                                                          palette = color_palette)
      }
    }




  } else {
    out = NULL
  }



  return(out)
}


#@param ... further arguments to geom_roc
#@source https://cran.r-project.org/web/packages/plotROC/vignettes/examples.html
plot_roc <- function(true, pred, name,
                     multi = c("grouping", "faceting"),
                     add.auc = F, interactive = F, prefix = "plot_roc_r",
                     n.cuts = 8, ...){

  #match args
  multi = match.arg(multi)

  #basic plot
  if(isTRUE(!missing(name) && !is.null(name) && identical(multi, "grouping"))){
    basicplot = ggplot2::ggplot(mapping = ggplot2::aes(d = true, m = pred, color = name))
  } else {
    basicplot = ggplot2::ggplot(mapping = ggplot2::aes(d = true, m = pred))
  }

  #add roc
  basicplot = basicplot + plotROC::geom_roc(n.cuts = n.cuts, ...)

  #style
  out = basicplot + plotROC::style_roc(theme = ggplot2::theme_grey, xlab = "1 - Specificity", ylab = "Sensitivity")

  #check facets
  if(isTRUE(!missing(name) && !is.null(name) && identical(multi, "faceting"))){
    out = out + ggplot2::facet_wrap(~ name)
  }

  if(add.auc){
    out = out + ggplot2::annotate(geom = "text", x = .75, y = .25,
                                  label = paste("AUC =", round(plotROC::calc_auc(basicplot)$AUC, 2)))
  }

  if(interactive){
    out = plotROC::export_interactive_roc(ggroc_p = out, prefix = prefix)
  }

  #return
  return(out)
}


create_violin_plot <- function(
  #Data
  df.data,
  df.test,

  #Plot
  add.boxplot  = T,
  add.jitter   = T,
  add.test     = F,
  add.facets   = T,

  #Axes
  xstring   = 'strata',
  ystring   = 'data',
  colstring = 'strata',
  xlab      = "Group",
  ylab,

  #Facet
  scales    = "free_x",
  facet.rows = NULL,
  facet.cols = NULL,

  #Legend
  legend.title = "Group",

  #Lim
  xlim = NULL,
  ylim = NULL,

  #Further args to ggplot
  ...
){

  #get ylim and add 2 for printing the text
  ymax = max(df.data[,ystring], na.rm = T)
  ymax = ymax + 3
  if(is.null(ylim)) {ylim = range(df.data[,ystring], ymax, na.rm = T)}else{ylim = range(ylim, ymax, na.rm=T)}

  #Create plot
  # p <- ggplot2::ggplot(data = df.data, mapping = ggplot2::aes(x = bmi_class, y = data, color = bmi_class))
  p <- ggplot2::ggplot(data = df.data, mapping = ggplot2::aes_string(x = xstring, y = ystring, color = colstring), ...)

  #Add violin
  p = p + ggplot2::geom_violin(trim = FALSE, na.rm = T, draw_quantiles = 0.5)

  #Expand limit
  # p = p + ggplot2::expand_limits(y = newylim)

  #Add boxplot
  if(add.boxplot){
    p = p + ggplot2::geom_boxplot(width = 0.1, outlier.shape = NA, na.rm = T)
  }

  #Add jitter
  if(add.jitter){
    p = p + ggplot2::geom_jitter(height = 0, width = 0.01, size = 0.5)
  }

  #Add test p-value
  if(isTRUE(add.test && !missing(df.test) && !is.null(df.test))){
    p = p + ggplot2::geom_text(
      data    = df.test,
      # mapping = ggplot2::aes(x = -Inf, y = Inf, label = string),
      # mapping = ggplot2::aes(x = -Inf, y = max(df.plot$data, na.rm = T), label = string),
      # mapping = ggplot2::aes(x = -Inf, y = ymax, label = string),
      mapping = ggplot2::aes(x = -Inf, y = ymax, label = string, color = NULL),
      hjust   = -0.1,
      # vjust   = -1,
      # vjust   = 1,
      show.legend = F
    )
  }


  #add facets
  if(add.facets){
    p = p + ggplot2::facet_grid(rows = facet.rows, cols = facet.cols, scales = scales)
  }

  p = p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = T)

  #theme
  p = p + ggplot2::theme_bw()

  #Labels
  p = p + ggplot2::labs(x = xlab, y = ylab)

  #Guides
  p = p + ggplot2::guides(color = ggplot2::guide_legend(title = legend.title, title.position = "top"))

  #Return
  return(p)
}
