#'Create DataTable
#'@description This function sets up some arguments before
#'a call to \code{\link[DT]{datatable}}. It is used
#'in the generation of the interactive report.
#'@param data a data object (either a matrix or a data frame)
#'@param col.filter logical, whether to use column filters
#'@param use.regex logical, whether to use regular expressions
#'for searching
#'@param rownames logical, whether to show \code{data} rownames. \code{FALSE} by default
#'@param scrollX logical, whether to enable horizontal scrolling. \code{FALSE} by default
#'@return an HTML widget to display rectangular data
#'@keywords internal
create_dt <- function(data,
                      col.filter = TRUE,
                      use.regex = TRUE,
                      scrollX = FALSE,
                      rownames = F){

  #Set the options for the table

  #1) Order of the table elements in the DOM
  #B stands for buttons,
  #l for length changing input control,
  #f for filtering input,
  #r for processing display element,
  #t for the table,
  #i for table information summary,
  #p for pagination display.
  dom = 'Blfrtip';

  # dom = paste("<'row'<'col-sm-12'B>>",
  #   "<'row'<'col-sm-12 col-md-6'l><'col-sm-12 col-md-6'f>>",
  #   "<'row'<'col-sm-12'tr>>",
  #   "<'row'<'col-sm-12 col-md-5'i><'col-sm-12 col-md-7'p>>")

  #2) Buttons to be presented to the end user.
  # Options are c('copy', 'csv', 'excel', 'pdf', 'print')
  # Example: buttons = c('copy', 'csv', 'excel');
  # Buttons can be listed in a collection
  buttons = list('copy',
                 list(extend = 'collection',
                      # buttons = c('csv', 'excel', 'pdf'),#it seems pdf raises an issue - 2020-04
                      buttons = c('csv', 'excel'),
                      text = 'Download')
  )

  #3) The entries in the length drop down select list that DataTables
  #shows when pagination is enabled. If a 2D array is provided,
  #a) the first inner array is used for the page length values
  #b) the second inner array is used as the displayed options. This is useful for language strings such as 'All'
  lengthMenu = list(c(10,25,50, 100, -1),
                    c(10,25,50, 100, "All"));

  #4) column filter
  #whether/where to use column filters;
  #none: no filters;
  #bottom/top: put column filters at the bottom/top of the table;
  #range sliders are used to filter numeric/date/time columns,
  #select lists are used for factor columns,
  #and text input boxes are used for character columns;
  filter = if(isFALSE(col.filter)){'none'}else{list(position = 'top', clear = FALSE)}

  #5) regex
  search = if(use.regex){list(regex = TRUE, caseInsensitive = FALSE)}else{list(regex = FALSE)}

  #Create datatable
  DT::datatable(data = data,
                filter = filter,
                extensions = 'Buttons',
                options = list(dom = dom,
                               buttons = buttons,
                               lengthMenu = lengthMenu,
                               search = search,
                               scrollX = scrollX),
                rownames = rownames,
                autoHideNavigation = TRUE,
                width = "100%",
                height = "auto"#needed to avoid datatable overplot in the next div
  )
}

#'Create a plotly density plot
#'@param data the data from which the estimate is to be computed. For the default method a numeric vector: long vectors are not supported.
#'@param groups a list of matrices, each representing a group
#'@param xaxis x axis' title
#'@param yaxis y axis' title
#'@param title.plot layout's title
#'@param bw the smoothing bandwidth as in \code{\link[stats]{density}}
#'@inheritParams plotly::add_trace
#'@return a plotly object
#'@keywords internal
plotly.density <- function(data,
                           groups,
                           # from = 0,
                           # bw = 'ucv',
                           bw = "nrd0",
                           fill = "tozeroy",
                           yaxis = "Density",
                           xaxis = "",
                           title.plot = "Density curve",
                           na.rm = TRUE){


  #Compute the density
  d = stats::density(x = data,
                     # from = from,
                     bw = bw,
                     na.rm = na.rm)

  #start plot
  p <- plotly::plot_ly(alpha = 0.5)

  #plot data
  p <- plotly::add_trace(p = p, x = d$x, y = d$y, fill = fill, type = 'scatter', mode = 'lines', name = 'all')

  #if groups are present, add them
  if(!missing(groups) && !is.null(groups) && is.list(groups)){
    name.groups = names(groups);

    for(i in seq(length(groups))){
      #Compute the density
      d = stats::density(x = groups[[i]],
                         # from = from,
                         bw = bw,
                         na.rm = na.rm);

      #add to plot
      p <- plotly::add_trace(p = p, x = d$x, y = d$y, fill = fill, type = 'scatter', mode = 'lines', name = name.groups[[i]])

    }
  }

  #Set layout
  p = plotly::layout(p = p,
                     title = title.plot,
                     paper_bgcolor='rgb(255,255,255)',
                     plot_bgcolor='rgb(229,229,229)',
                     xaxis = list(title = xaxis,
                                  gridcolor = 'rgb(255,255,255)',
                                  showgrid = TRUE,
                                  showline = FALSE,
                                  showticklabels = TRUE,
                                  tickcolor = 'rgb(127,127,127)',
                                  ticks = 'outside',
                                  zeroline = FALSE),
                     yaxis = list(title = yaxis,
                                  gridcolor = 'rgb(255,255,255)',
                                  showgrid = TRUE,
                                  showline = FALSE,
                                  showticklabels = TRUE,
                                  tickcolor = 'rgb(127,127,127)',
                                  ticks = 'outside',
                                  zeroline = FALSE))

  #return
  return(p)

}




#'Plot the Evaluation of a Learning Method
#'
#'@description This function creates an interactive plot representing
#'the evaluation of a learning method across different
#'training-set sizes.
#'
#'@details An interactive plot showing the mean performance and the related
#'95\% confidence interval of a learning method
#'across different training-set sizes is produced.
#'The evaluated element is identified by the \code{name}
#'column in the \code{data}. If a unique key is found then
#'\code{\link{plotly_single_evaluation}} is dispatched, while if
#'multiple keys are found then the dispatched method is
#'\code{\link{plotly_multi_evaluation}}. In the latter case,
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
#'@param tooltip a character vector, the subset of columns to select for creating a text for tooltip.
#'It is used unless a \code{tooltip} column is found in \code{data}
#'@param ... further arguments to \code{\link{plotly_single_evaluation}}
#'or \code{\link{plotly_multi_evaluation}}
#'
#'@return An object of class \code{plotly}
#'
#'@seealso
#'\code{\link{plotly_single_evaluation}},
#'\code{\link{plotly_multi_evaluation}}
#'
#'@author Alessandro Barberis
plotly_evaluation <- function(data, tooltip = c("imodel", "screened", "nfeatures", "configuration"), ...){

  #--------------------------------------------------------------------------------------------#
  #check if multi
  is.multi = length(levels(factor(data$name)))>1

  #--------------------------------------------------------------------------------------------#
  #check tooltip
  if(isTRUE(is.na(pmatch(x = "tooltip", table = colnames(data))))){
    #create a tooltip column
    ##match
    tooltip = match.arg(arg = tooltip, choices = colnames(data), several.ok = T)
    ##set sep.el
    sep.el = rep(x = ": ", times = length(tooltip))
    ##check if config is present
    m.config = pmatch(x = "configuration",table = tooltip)
    #set a specific separator
    if(isFALSE(is.na(m.config))){
      #update
      sep.el[m.config] = "<br>"
    }

    ##create string
    tooltip = sapply(
      X = seq(nrow(data)),
      FUN = function(i, data, tooltip){
        out = stts(object = data[i,tooltip,drop=F], sep.el = sep.el);
        return(out)
      },
      data = data, tooltip = tooltip, USE.NAMES = FALSE)

    ##add to data
    data$tooltip = tooltip
  }

  #--------------------------------------------------------------------------------------------#
  #plot
  if(is.multi){
    p = plotly_multi_evaluation(data = data, ...)
  } else {
    p = plotly_single_evaluation(data = data, ...)
  }
  #--------------------------------------------------------------------------------------------#
  #return
  return(p)
}


#'Plot the Evaluation of a Learning Method
#'
#'@description This function creates an interactive plot representing
#'the evaluation of a learning method across different
#'training-set sizes.
#'
#'@details An interactive plot showing the mean performance and the related
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
#'   \item{\code{tooltip}}{contains the hover text}
#'}
#'@param thr numerical value, if provided it is used to draw an horizontal line
#'@param colour character string, containing the colour for the performance estimate
#'@param add.uncertainty logical, whether to include the quantified uncertainty of the
#'performance estimate in the plot
#'@param color.ci character string, containing the colour for the confidence interval
#'@param alpha.ci alpha value for \code{color.ci}
#'@param add.boxplot logical, whether to include a boxplot in the figure
#'@param colour.box character string, the colour of the boxplot
#'@param colour.box.line character string, the colour of the lines in the boxplot
#'@param add.scores logical, whether to add the performance metric of
#'individual models as points in the plot
#'@param colour.point character string, the colour of the points
#'@param add.best logical, whether to add a point indicating the performance
#'of what is reported as best model in \code{data}
#'@param colour.best character string, the colour of the best model
#'@param colour.best.line character string, the colour of the line of the best model
#'@param shape.best integer, \code{shape} aesthetic passed to \code{\link[plotly]{add_trace}}
#'@param size.best integer, \code{size} aesthetic passed to \code{\link[plotly]{add_trace}}
#'@param title character string, the title of the plot
#@param subtitle character string, the subtitle of the plot
#@param caption character string, the caption of the plot
#'@param xlab,ylab character string, axes labels
#'@param ... further arguments to \code{\link[plotly]{plot_ly}}
#'
#'@return A \code{plotly} object.
#'
#'@seealso
#'\code{\link{plotly_multi_evaluation}}
#'
#'@author Alessandro Barberis
plotly_single_evaluation <- function(
  #data
  data,
  thr = NULL,

  #Performance estimate
  # colour = "darksalmon",
  name.ms = "mean score",
  colour  = "rgba(255, 127, 14, 1)",
  add.uncertainty = T,
  color.ci = I("gray95"),
  alpha.ci = NULL,

  #Performance scores
  add.boxplot = T,
  colour.box  = "rgba(136, 194, 136, 1)",
  colour.box.line = "rgba(44, 160, 44, 1)",

  add.scores = T,
  colour.point = "rgba(44, 160, 44, 1)",

  #Best model
  add.best = T,
  colour.best = 'rgba(255, 182, 193, .9)',
  colour.best.line = 'rgba(152, 0, 0, .8)',
  size.best = 10,
  shape.best = 0,

  #Layout
  title = "Evaluation",
  # subtitle,
  # caption,
  xlab = "Training-set size",
  ylab = "Performance",

  #Further args
  ...){

  #--------------------------------------------------------------------------------------------#
  #reorder data frame
  data = data[order(data$training_set_size, decreasing = F),,drop=F]

  #--------------------------------------------------------------------------------------------#
  #Initiate plotly visualisation
  p = plotly::plot_ly(data = data, x = ~training_set_size, y = ~score, ...)

  #--------------------------------------------------------------------------------------------#
  #Add Confidence Intervals
  if(add.uncertainty){
    p = plotly::add_ribbons(p = p, ymin = ~lower_ci, ymax = ~upper_ci, color = color.ci, alpha = alpha.ci, name = "95% confidence")
    # p
  }

  #--------------------------------------------------------------------------------------------#
  #Add mean error
  p = plotly::add_trace(p = p, y = ~mean_score, type = 'scatter', mode = 'lines+markers', name = name.ms,
                        line = list(color=colour), marker = list(color = colour))
  # p

  #--------------------------------------------------------------------------------------------#
  #Add boxplot
  if(add.boxplot){
    p = plotly::add_boxplot(
      p = p,
      y = ~score,
      # color = ~key,
      jitter = 0.3,
      pointpos = -1.8,
      boxpoints = ifelse(test = add.scores, yes = 'all', no = FALSE),
      # text = ~text,
      text = ~tooltip,
      name = ylab,
      marker = list(color = colour.point),
      line   = list(color = colour.box.line),
      fillcolor = colour.box
      # name = ~key,
      # showlegend = FALSE,
      # legendgroup = ~key
    )
    # p
  } else {
    if(add.scores){
      p = plotly::add_trace(
        p = p,
        y    = ~score,
        x    = ~training_set_size,
        # text = ~text,
        text = ~tooltip,
        type = 'scatter',
        mode = 'markers',
        # color = ~key,
        name  = ylab,
        # name = ~key,
        # showlegend = ifelse(test = add.boxplot, yes = FALSE, no = TRUE),
        # showlegend = FALSE,
        # legendgroup = ~key
        marker = list(
          # size = 10,
          # line = list(color = 'rgba(152, 0, 0, .8)', width = 2)
          color = colour.point
          )
      )
    }
  }

  #--------------------------------------------------------------------------------------------#
  #Add threshold
  if(!is.null(thr)){p = plotly::add_trace(p = p, y = thr, line = list(color = 'black', dash = 'dash'), type = 'scatter', mode = 'lines', name = 'threshold')}

  #--------------------------------------------------------------------------------------------#
  #Add best model
  if(add.best){
    ibest = data$best_resample & data$best_model
    p = plotly::add_trace(
      p       = p,
      y       = ~score[ibest],
      x       = ~training_set_size[ibest],
      # text    = ~text[ibest],
      text    = ~tooltip[ibest],
      type    = 'scatter',
      mode    = 'markers',
      name    = 'best model',
      marker  = list(
        size  = size.best,
        symbol = shape.best,
        color = colour.best,
        line  = list(
          color = colour.best.line,
          width = 2)
        )
      )
  }
  #--------------------------------------------------------------------------------------------#
  #Set layout
  p = plotly::layout(
    p = p,
    title = title,
    paper_bgcolor='rgb(255,255,255)',
    plot_bgcolor='rgb(229,229,229)',
    xaxis = list(title = xlab,
                gridcolor = 'rgb(255,255,255)',
                showgrid = TRUE,
                showline = FALSE,
                showticklabels = TRUE,
                tickcolor = 'rgb(127,127,127)',
                ticks = 'outside',
                zeroline = FALSE),
    yaxis = list(title = ylab,
                gridcolor = 'rgb(255,255,255)',
                showgrid = TRUE,
                showline = FALSE,
                showticklabels = TRUE,
                tickcolor = 'rgb(127,127,127)',
                ticks = 'outside',
                zeroline = FALSE))
  # p

  #--------------------------------------------------------------------------------------------#
  #return
  return(p)
}


#'Plot Multiple Evaluations
#'
#'@description This function creates an interactive plot representing
#'multiple evaluations of a learning method across different
#'training-set sizes.
#'
#'@details An interactive plot showing the mean performance and the related
#'95\% confidence interval of learning methods
#'across different training-set sizes is produced.
#'Individual scores and summary metrics in the form of boxplots
#'can be also added (default) via the \code{add.scores} and
#'\code{add.boxplot} arguments, respectively.
#'
#'@inheritParams plotly_single_evaluation
#'
#'@return A \code{plotly} object.
#'
#'@seealso
#'\code{\link{plotly_single_evaluation}}
#'
#'@author Alessandro Barberis
plotly_multi_evaluation <- function(

  #data
  data,
  thr = NULL,
  #CI
  add.uncertainty = T,
  # color.ci = I("gray95"),
  alpha.ci = NULL,

  #Performance scores
  add.boxplot = T,
  add.scores = T,

  #Best model
  add.best = T,
  shape.best = 0,#22,
  size.best  = 10,

  #Layout
  title = "Evaluation",
  # subtitle,
  # caption,
  xlab = "Training-set size",
  ylab = "Performance",

  #Further args
  ...){

  #--------------------------------------------------------------------------------------------#
  #reorder data frame
  data = data[order(data$training_set_size, decreasing = F),,drop=F]

  #--------------------------------------------------------------------------------------------#
  #Initiate plotly visualisation
  p = plotly::plot_ly(data = data, x = ~training_set_size, y = ~score, ...)

  #--------------------------------------------------------------------------------------------#
  #Add Confidence Intervals
  if(add.uncertainty){
    p = plotly::add_ribbons(
      p = p,
      ymin = ~lower_ci,
      ymax = ~upper_ci,
      color = ~name,
      alpha = alpha.ci,
      name = "95% confidence",
      legendgroup = ~name,
      showlegend = FALSE
    )
    # p
  }

  #--------------------------------------------------------------------------------------------#
  #Add mean error
  # showL = !(add.boxplot || add.errors)
  # showL = !add.boxplot
  showL = T
  if(showL){
    p = plotly::add_trace(
      p = p,
      y = ~mean_score,
      color = ~name,
      type = 'scatter',
      mode = 'lines+markers',
      name = ~name,
      legendgroup = ~name,
      showlegend = TRUE
    )
  } else {
    p = plotly::add_trace(
      p = p,
      y = ~mean_score,
      color = ~name,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'mean score',
      legendgroup = ~name,
      showlegend = FALSE
    )
  }

  # p = plotly::add_trace(p = p, y = ~mean_score, type = 'scatter', mode = 'lines+markers', name = 'mean error', color = "rgba(255, 127, 14, 1)")
  # p

  #--------------------------------------------------------------------------------------------#
  #Add boxplot
  if(add.boxplot){
    p = plotly::add_boxplot(
      p = p,
      y = ~score,
      color = ~name,
      jitter = 0.3,
      pointpos = -1.8,
      boxpoints = ifelse(test = add.scores, yes = 'all', no = FALSE),
      # text = ~text,
      text    = ~tooltip,
      # name = ylab
      name = ~name,
      showlegend = FALSE,
      legendgroup = ~name
    )
    # p
  } else {
    if(add.scores){
      p = plotly::add_trace(
        p = p,
        y    = ~score,
        x    = ~training_set_size,
        # text = ~text,
        text = ~tooltip,
        type = 'scatter',
        mode = 'markers',
        color = ~name,
        name = ~name,
        # showlegend = ifelse(test = add.boxplot, yes = FALSE, no = TRUE),
        showlegend = FALSE,
        legendgroup = ~name
      )
    }
  }

  #--------------------------------------------------------------------------------------------#
  #Add threshold
  #--------------------------------------------------------------------------------------------#
  if(!is.null(thr)){p = plotly::add_trace(p = p, y = thr, line = list(color = 'black', dash = 'dash'), type = 'scatter', mode = 'lines', name = 'threshold')}

  #--------------------------------------------------------------------------------------------#
  #Add best model
  #--------------------------------------------------------------------------------------------#
  if(add.best){
    ibest = data$best_resample & data$best_model
    p = plotly::add_trace(
      p = p,
      data = data[ibest,,drop=F],
      y    = ~score,
      x    = ~training_set_size,
      # text = ~text,
      text = ~tooltip,
      type = 'scatter',
      mode = 'markers',
      name = 'best model',
      color = ~name,
      # name = ~name,
      showlegend = FALSE,
      legendgroup = ~name,
      marker = list(
        # color  = ~name,
        symbol = shape.best,
        size   = size.best
        # color = 'rgba(255, 182, 193, .9)',
        # line = list(color = 'rgba(152, 0, 0, .8)', width = 2)
      )
    )
  }


  #--------------------------------------------------------------------------------------------#
  #Set layout
  #--------------------------------------------------------------------------------------------#
  p = plotly::layout(p = p,
                     title = title,
                     paper_bgcolor='rgb(255,255,255)',
                     plot_bgcolor='rgb(229,229,229)',
                     xaxis = list(title = xlab,
                                  gridcolor = 'rgb(255,255,255)',
                                  showgrid = TRUE,
                                  showline = FALSE,
                                  showticklabels = TRUE,
                                  tickcolor = 'rgb(127,127,127)',
                                  ticks = 'outside',
                                  zeroline = FALSE),
                     yaxis = list(title = ylab,
                                  gridcolor = 'rgb(255,255,255)',
                                  showgrid = TRUE,
                                  showline = FALSE,
                                  showticklabels = TRUE,
                                  tickcolor = 'rgb(127,127,127)',
                                  ticks = 'outside',
                                  zeroline = FALSE))
  # p

  #--------------------------------------------------------------------------------------------#
  #return
  return(p)
}


confusion_matrix_kable <- function(x, digits = max(3, getOption("digits") - 3), format = c('text', 'html'), ...){

  #Add total
  ndn = names(dimnames(x))
  rowtot = rowSums(x)
  coltot = colSums(x)
  tot = sum(coltot)
  ncorrect = sum(diag(x))
  correct = (ncorrect)/tot
  x = cbind(x, Total = rowtot)
  x = rbind(x, Total = c(coltot, tot))

  #Update names
  dn = dimnames(x)
  names(dn) = ndn
  dimnames(x) = dn

  #Generate kable obj
  tmp = knitr::kable(x = x, format = format, output = TRUE, align = 'c')

  #Set header
  tmp = kableExtra::row_spec(kable_input = tmp, row = 0, bold = F)

  #Set Total columns differently
  tmp = kableExtra::row_spec(kable_input = tmp, row = nrow(x), italic = T)
  tmp = kableExtra::column_spec(kable_input = tmp, column = ncol(x)+1, italic = T, bold = F)

  # tmp = kableExtra::row_spec(kable_input = tmp, row = nrow(x) - 2, hline_after = T)#NOT WORKING WITH HTML see https://stackoverflow.com/questions/53655983/row-spec-function-from-kableextra-does-not-create-a-horizontal-line-in-html-ou
  tmp = kableExtra::column_spec(kable_input = tmp, column = c(ncol(x)), border_right = T)
  tmp = kableExtra::row_spec(kable_input = tmp, row = nrow(x) - 1, extra_css = "border-bottom: 1px solid")

  #Add header
  tmp = kableExtra::add_header_above(
    kable_input = tmp,
    header = stats::setNames(object = c(1, ncol(x) -1 , 1), nm = c(" ", ndn[2] , " "))
  )

  #Add row header
  # tmp = kableExtra::pack_rows(kable_input = tmp, group_label = ndn[1], start_row = 1, end_row = nrow(x))

  #Add percent correct
  tmp = kableExtra::footnote(
    kable_input = tmp,
    general_title = "",
    general = paste("\n Percent Correct: ", format(round(correct, digits)), "\n"))

  # print(tmp)
  return(tmp)
}


#sources:
#https://rstudio.github.io/DT/
#https://datatables.net/manual/styling/classes
#https://help.displayr.com/hc/en-us/articles/360003127476-How-to-Create-Customized-Tables-Using-the-DT-R-Package
#https://rstudio.github.io/DT/005-bootstrap.html
#https://rstudio.github.io/DT/010-style.html
confusion_matrix_dt <- function(
  x,
  digits = max(3, getOption("digits") - 3),
  height = "auto"
){

  #classes
  classes = colnames(x)

  #nclasses
  nclasses = ncol(x)

  #Add total
  ndn = names(dimnames(x))
  rowtot = rowSums(x)
  coltot = colSums(x)
  tot = sum(coltot)
  ncorrect = sum(diag(x))
  correct = (ncorrect)/tot
  x = cbind(x, Total = rowtot)
  # x = rbind(x, Total = c(coltot, tot))

  #Update names
  dn = dimnames(x)
  names(dn) = ndn
  dimnames(x) = dn

  # #Simple
  # sketch = htmltools::withTags(code = table(
  #   class = 'display',
  #   thead(
  #     tr(
  #       th(rowspan = 2, ' '),
  #       th(colspan = nclasses, ndn[2]),
  #       th(rowspan = 2, 'Total')
  #     ),
  #     tr(
  #       lapply(classes, th)
  #     )
  #   ),
  #   tfoot(
  #     tr(
  #       th('Total'),
  #       lapply(c(coltot, tot), th)
  #     )
  #   )
  # ))

  sketch = htmltools::withTags(code = table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, ' ', style="border-right-style:solid;border-right-width:1px; border-right-color:#000000"),
        th(colspan = nclasses, ndn[2]),
        th(rowspan = 2, 'Total', style="border-left-style:solid;border-left-width:1px; border-left-color:#000000")
      ),
      tr(
        lapply(classes, th)
      )
    ),
    tfoot(
      tr(
        th('Total', style="border-right-style:solid;border-right-width:1px; border-right-color:#000000"),
        lapply(c(coltot), th),
        th(tot, style="border-left-style:solid;border-left-width:1px; border-left-color:#000000")
      )
    )
  ))
  # print(sketch)

  #Create table
  my.table = DT::datatable(
    data = x,
    container = sketch,
    rownames = T,
    filter = "none",
    # autoHideNavigation = TRUE,
    options = list(
      # pageLength = nclasses + 1,
      dom = 't',
      ordering = F
    ),
    # height = "auto",
    height = height,
    width = "100%",
    caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;',
      paste("\n\n Percent Correct: ", format(round(correct * 100, digits)))
      # 'Confusion Matrix: ',
      # htmltools::em(paste("\n\n Percent Correct: ", format(round(correct, digits))))
    )
  )

  #Row names
  my.table = DT::formatStyle(
    my.table,
    columns = " ",
    fontWeight = 'bold',
    backgroundColor = "#ffffff",
    # borderRightColor = "#ffffff",
    borderRightColor = "#000000",
    borderRightStyle = "solid",
    borderRightWidth = "1px",
  )

  #Final column
  my.table = DT::formatStyle(
    my.table,
    columns = "Total",
    fontWeight = 'bold',
    backgroundColor = "#ffffff",
    # borderRightColor = "#ffffff",
    borderLeftColor = "#000000",
    borderLeftStyle = "solid",
    borderLeftWidth = "1px",
  )

  # my.table

  return(my.table)
}



# plotly_violin <- function(
#   #Data
#   df.data,
#   df.test,
#
#   #Plot
#   add.boxplot  = T,
#   add.jitter   = T,
#   add.test     = F,
#   add.facets   = T,
#
#   #Axes
#   xstring   = 'strata',
#   ystring   = 'data',
#   colstring = 'strata',
#   xlab      = "Group",
#   ylab,
#
#   #Facet
#   scales    = "free_x",
#   facet.rows = NULL,
#   facet.cols = NULL,
#
#   #Legend
#   legend.title = "Group",
#
#   #Lim
#   xlim = NULL,
#   ylim = NULL,
#
#   #Further args to ggplot
#   ...
# ){
#   #get ylim and add 2 for printing the text
#   ymax = max(df.data[,ystring], na.rm = T)
#   ymax = ymax + 3
#   if(is.null(ylim)) {ylim = range(df.data[,ystring], ymax, na.rm = T)}else{ylim = range(ylim, ymax, na.rm=T)}
#
#   #Create plot
#   # p <- plotly::plot_ly(data = df.data, x = xstring, y = ystring, color = colstring, ..., type = 'violin')
#   p <- plotly::plot_ly(data = df.data, x = df.data[,xstring], y = df.data[,ystring], color = df.data[,colstring], type = 'violin', ...)
#   p <- plotly::plot_ly(data = df.data, x = df.data[,xstring], y = df.data[,ystring], color = df.data[,colstring], type = 'violin')
#
#   p <- plotly::plot_ly(data = df.data, x = df.data[,xstring], y = df.data[,ystring], color = df.data[,colstring], type = 'violin',
#   box = list(
#     visible = T
#   ),
#   meanline = list(
#     visible = T
#   )
#   )
#
#   plotly.plots <- lapply(unique(df.data$score), function(var) {
#     plotly::plot_ly(df.data, x = as.formula(paste0("~", xstring, "[df.data$score == '",var,"']")), y = as.formula(object = paste0("~", ystring, "[df.data$score == '",var,"']")), type = 'violin')
#   })
#   #Add violin
#   p = p + plotly::geom_violin(trim = FALSE, na.rm = T, draw_quantiles = 0.5)
#
#   #Add boxplot
#   if(add.boxplot){
#     p = plotly::add_boxplot(p = p, x = df.data[,xstring], y = df.data[,ystring], color = df.data[,colstring])
#     p = plotly::add_trace(
#       p = p,
#       x = df.data[,xstring],
#       y = df.data[,ystring],
#       legendgroup = df.data[,colstring],
#       # scalegroup = 'M',
#       # name = 'M',
#       box = list(
#         visible = T
#       ),
#       meanline = list(
#         visible = T
#       ),
#       color = df.data[,colstring]
#     )
#   }
#
#   #Add jitter
#   if(add.jitter){
#     p = p + ggplot2::geom_jitter(height = 0, width = 0.01, size = 0.5)
#   }
#
#   #Add test p-value
#   if(isTRUE(add.test && !missing(df.test) && !is.null(df.test))){
#     p = p + ggplot2::geom_text(
#       data    = df.test,
#       # mapping = ggplot2::aes(x = -Inf, y = Inf, label = string),
#       # mapping = ggplot2::aes(x = -Inf, y = max(df.plot$data, na.rm = T), label = string),
#       # mapping = ggplot2::aes(x = -Inf, y = ymax, label = string),
#       mapping = ggplot2::aes(x = -Inf, y = ymax, label = string, color = NULL),
#       hjust   = -0.1,
#       # vjust   = -1,
#       # vjust   = 1,
#       show.legend = F
#     )
#   }
#
#
#   #add facets
#   if(add.facets){
#     p = p + ggplot2::facet_grid(rows = facet.rows, cols = facet.cols, scales = scales)
#   }
#
#   p = p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = T)
#
#   #theme
#   p = p + ggplot2::theme_bw()
#
#   #Labels
#   p = p + ggplot2::labs(x = xlab, y = ylab)
#
#   #Guides
#   p = p + ggplot2::guides(color = ggplot2::guide_legend(title = legend.title, title.position = "top"))
#
#   #Return
#   return(p)
# }
