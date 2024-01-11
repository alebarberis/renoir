#' @include classes_generics.R
NULL

#'Get the number of responses
#'@param y response variable
#'@param resp.type response type
#'@keywords internal
get_num_of_responses = function(y, resp.type){


  n = switch(resp.type,
             gaussian    = 1L,
             binomial    = 2L,
             multinomial = if(!is.null(dim(y))){ncol(y)}else{length(levels(as.factor(y)))},
             mgaussian   = ncol(y),
             cox         = 1L,
             poisson     = 1L)

  return(n);
}

#'Is the response type multi-variable?
#'@param y response variable
#'@param resp.type response type
#'@return logical
#'@keywords internal
is_multi_response <- function(y, resp.type){
  nr = get_num_of_responses(y=y, resp.type = resp.type)

  is_mr = (nr>1);

  return(is_mr)
}

#'Get the number of observations
#'@param x the predictors matrix or the response variable
#'@param na.rm logical, whether to remove NAs. If x is response variable
#'with multiple output values, an observation is removed if NA
#'in all responses.
#'@return the number of observations
#'@keywords internal
get_nobs <- function(object, na.rm = FALSE){
  #check if multi response
  is.multi = !is.null(nrow(object))

  #get N
  if(is.multi){
    #check NA
    if(na.rm){
      #use first response
      object = object[,1,drop=T]
      #remove NA
      object = object[!is.na(object)]
      #get N
      N = length(object)
    } else {
      N = nrow(object)
    }
  } else {
    #check NA
    if(na.rm){
      object =  object[!is.na(object)]
    }
    #get N
    N = length(object)
  }
  return(N)
}


subset_observations_from_matrix <- function(object, which){
  if(!missing(which) && !is.null(which)){
    object = object[which, , drop = FALSE]
  }
  return(object)
}

subset_observations_from_vector <- function(object, which){
  if(!missing(which) && !is.null(which)){
    object = object[which]
  }
  return(object)
}

subset_features_from_matrix <- function(object, which){
  if(!missing(which) && !is.null(which)){
    object = object[, which, drop = FALSE]
  }
  return(object)
}

methods::setMethod(f = "subset_observations", signature = methods::signature(object ="matrix"),  definition = subset_observations_from_matrix)
methods::setMethod(f = "subset_observations", signature = methods::signature(object ="vector"),  definition = subset_observations_from_vector)
methods::setMethod(f = "subset_observations", signature = methods::signature(object ="missing"), definition = function(object){return(object)})
methods::setMethod(f = "subset_observations", signature = methods::signature(object ="NULL"), definition = function(object){return(object)})
methods::setMethod(f = "subset_features",     signature = methods::signature(object ="matrix"),  definition = subset_features_from_matrix)

methods::setMethod(f = "nobs", signature = methods::signature(object ="matrix"), definition = function(object){return(nrow(object))})
methods::setMethod(f = "nobs", signature = methods::signature(object ="data.frame"), definition = function(object){return(nrow(object))})
methods::setMethod(f = "nobs", signature = methods::signature(object ="vector"), definition = function(object){return(length(object))})
methods::setMethod(f = "nobs", signature = methods::signature(object ="factor"), definition = function(object){return(length(object))})

get_parameter = function(object, name, default = NA){

  #get parameters
  parameters = get_parameters(object)

  #match
  m = match(x = name, table = names(parameters))

  #check
  if(!is.na(m)){
    #get parameter
    parameter = parameters[[m]]
  } else {
    #default
    parameter = default
  }

  #return
  return(parameter)
}

check_parameters <- function(parameters, ...){

  #get further aguments
  args = list(...)

  #check if any provided and override
  m = match(x = names(parameters), table = names(args))

  #remove na
  m = m[!is.na(m)]

  #check
  if(length(m)>0){parameters = parameters[-m]}

  #return
  return(parameters)
}

check_parameters_and_dots <- function(parameters, ...){
  #get further aguments
  args = list(...)

  #check if any provided and override
  m = match(x = names(args), table = names(parameters))

  #remove na
  m = m[!is.na(m)]

  #check
  if(length(m)>0){
    args = args[-m]
  }

  #out
  out = c(parameters, args)

  #return
  return(out)
}


select_by_id <- function(object, id){
  if(!missing(id)){
    #match the learning method
    index = match(x = id, table = get_id(object))[1]

    #subset
    object = object[[index]]
  }

  return(object)
}

#
# subset_observations <- function(x, which){
#
#   if(!missing(which) && !is.null(which)){
#     if (!is.null(dim(x))){
#       x = x[which, , drop = FALSE]
#     } else {
#       x = x[which]
#     }
#   }
#
#   return(x)
# }
#
# subset_features <- function(x, which){
#   if(!missing(which) && !is.null(which)){
#     x = x[, which, drop = FALSE]
#   }
#   return(x)
# }



subset_observations_def <- function(x, which){

  if (!is.null(dim(x))){
    x = x[which, , drop = FALSE]
  } else {
    x = x[which]
  }

  return(x)
}

subset_features_def <- function(x, which){
  if(!is.null(which)){
    x = x[, which, drop = FALSE]
  }
  return(x)
}

#'Create a filename
#'
#'@param filename string, a file name
#'@param n sample size
#'
#'@return A string, the file name.
#'
#'@keywords internal
create_filename <- function(filename, n){
  out = filename


  out = paste(out, n, sep = "_")
  return(out)
}


# get_analysis_type <- function(resp.type, learning.method){
#   name = switch(resp.type,
#                 gaussian    = 'linear',
#                 mgaussian   = 'multi-response linear',
#                 binomial    = 'logistic',
#                 multinomial = 'multinomial',
#                 poisson     = 'poisson',
#                 cox         = 'Cox')
#
#
# }

#'@keywords internal
get_name_regression_model <- function(resp.type){

  name = switch(resp.type,
                gaussian    = 'linear',
                mgaussian   = 'multi-response linear',
                binomial    = 'logistic',
                multinomial = 'multinomial',
                poisson     = 'poisson',
                cox         = 'Cox',
                NULL)

  #create out
  out = c(name, "regression model")

  # #remove if NULL
  # out = out[!is.null(out)]

  #paste
  out = paste(out, collapse = " ")

  #return
  return(out)
}

get_description_config <- function(x){

  # out = switch(
  #   x,
  #   'opt' = 'set of values for the hyperparameters of the model which optimises the mean cross-validated error',
  #   '1se' = 'set of values for the hyperparameters of the model such that the mean cross-validated error is within one standard error of the optimum'
  # )

  # out = switch(
  #   x,
  #   'opt' = 'hyperparameters of the model were selected in order to optimise the mean cross-validated error',
  #   '1se' = 'hyperparameters of the model were selected by following the "one standard error rule" such that the mean cross-validated error is within one standard error of the optimum'
  # )

  out = switch(
    x,
    'opt' = 'hyperparameters of the model were selected in order to optimise the mean resampled performance metric',
    '1se' = 'hyperparameters of the model were selected by following the "one standard error rule" such that the mean resampled performance metric is within one standard error of the optimum'
  )

  return(out)
}

is_opt_config <- function(x){isTRUE(identical(x, "opt"))}
is_1se_config <- function(x){isTRUE(identical(x, "1se"))}

get_description_scoring <- function(x, set = NULL){

  sets = c("train", "test", "full")

  #get number of used measures
  n = length(x)

  #convert to full name
  x = get_measure_names(x)

  #multi
  is.multiscoring = isTRUE(n>1)

  #check
  if(is.multiscoring){
    out = paste(n, "metrics", paste0("(", paste(x, collapse = ", "), ")"), "were used to assess the performance of the fitted models")
  } else {
    out = paste(n, "metric", paste0("(", paste(x, collapse = ", "), ")"), "was used to assess the performance of the fitted models")
  }

  #set
  if(isTRUE(!is.null(set) && (set %in% sets))){
    out = switch(
      set,
      train = paste(out, "while using the training set"),
      test  = paste(out, "while using the testing set"),
      full  = paste(out, "while using the entire set of data"),
      out
    )
  }

  #ret
  return(out)
}


#'Number to symbols
#'
#'@param x number to convert
#'@param cutpoints cutpoints linked to \code{symbols}
#'@param symbols character strings representing the numer in \code{x} falling
#'in the range defined by the corresponding \code{cutpoints}
#'
#'@return A symbolic representation of the number in input.
#'
#'@keywords internal
num_to_sym <- function(
  x,
  cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
  symbols = c("****", "***", "**", "*", ".", "ns")
  ){

  if(is.na(x)){x = as.numeric(x)}

  if(is.null(x)){x = as.numeric(NA)}

  if(!is.null(x)){
    sym = stats::symnum(x = x,
                        legend = FALSE,
                        corr = FALSE,
                        na = FALSE,
                        cutpoints = cutpoints,
                        symbols = symbols
    )
  } else {
    sym = NULL
  }

  return(sym)
}

#'Format score
#'@param x vector or data.frame containing the data of interest (e.g. p-values).
#'If a data.frame, it must contain a \code{score} column.
#'@inheritParams base::format
#'@param cutoff numerical cutoff for adding the symbol to the
#'\code{string} column
#'@param score score name to use in the \code{string} column
#'@param name (optional) name of the method used to obtain the score,
#'to use in \code{string}
#'@return a \code{data.frame} with 4 columns
#'\describe{
#'\item{score}{contains the score as provided in input}
#'\item{score_f}{formatted scores}
#'\item{score_sym}{scores as symbols, as returned by \link{num_to_sym}}
#'\item{string}{scores as string, e.g. 'p = 0.005 **'}
#'}
#'
#'@keywords internal
format_score <- function(
  x,
  scientific = TRUE, digits = 3,
  cutoff = 0.05,
  cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
  symbols = c("****", "***", "**", "*", ".", "ns"),
  score = "p",
  name = NULL
){

  if(is.vector(x)){x = data.frame(score = x, stringsAsFactors = F)}

  if(is.null(x$score)){stop("Check the provided 'x'.")}

  #number as string
  x$score_sym = sapply(X = x$score, FUN = num_to_sym, cutpoints = cutpoints, symbols = symbols)

  #format
  x$score_f = format(x$score, scientific = scientific, digits = digits)

  #String to plot
  x$string = paste(score, "=", x$score_f);

  #check name
  if(!is.null(name)){
    x$string = paste0(name, ", ", x$string)
  }

  #add significance as symbol
  iupdate = !is.na(x$score) & x$score < cutoff
  x$string[iupdate] = paste(x$string[iupdate], x$score_sym[iupdate])
  rm(iupdate)

  #return
  return(x)
}

#'Summary Table to String
#'@description Converts a summary table into a
#'character string.
#'
#'@details This function create a string containing the elements
#'of a \code{data.frame} so that each column is reported as
#'"colname: element1<sep.el>element2<sep.el>...". The different strings are then pasted together,
#'separated by the \code{sep.col} parameter.
#'
#'@param object a \code{data.frame}
#'@param sep.el a character string to separate the terms in a column from the column name
#'@param collapse.el a character string to separate the terms in a column
#'@param sep.col a character string to separate the resulting strings
#'
#'@return A character string
#'
#'@keywords internal
stts <- function(object, sep.el = ": ", collapse.el = ", ", sep.col = "<br>"){

  #--------------------------------------------------------------------------------------------#
  #columns
  cnames = colnames(object)

  #ncols
  ncols = ncol(object)

  #check sep.el
  if(length(sep.el) == 1L){sep.el = rep(x = sep.el, times = ncols)}

  #--------------------------------------------------------------------------------------------#
  #create a string for each column
  # out = sapply(
  #   X = colnames(object),
  #   FUN = function(name, obj){
  #     paste0(name, sep.el, paste(obj[[name]], collapse = collapse.el))
  #   }, obj = object)

  out = sapply(
    X = seq(ncols),
    FUN = function(i, obj, cnames, sep.el, collapse.el){
      #get name
      name = cnames[i]
      #paste
      out = paste0(name, sep.el[i], paste(obj[[name]], collapse = collapse.el))
      #r
      return(out)
    }, obj = object, cnames = cnames, sep.el = sep.el, collapse.el = collapse.el)

  #--------------------------------------------------------------------------------------------#
  #paste the strings
  out = paste(out, collapse = sep.col);

  #return
  return(out)
}


#'Create Feature Names
#'@description This functions create feature names.
#'
#'@details Function to standardise the creation of
#'feature names in renoir. If \code{nm} is provided
#'and is not \code{NULL}, the function returns \code{nm}.
#'If \code{nm = NULL}, a vector of length \code{n}
#'containing the feature names defined by the internal
#'default rule is returned.The rule creates the name so
#'that \code{prefix} is added to the number of the feature.
#'For example, creating names for \code{n = 2} features
#'with \code{prefix = "V"} will return \code{"V1", "V2"}.
#'
#'@param nm vector containing the feature names
#'@param n a length-one integer vector, number of features
#'@param prefix a length-one character vector, used in the
#'creation of the feature names
#'
#'@return A character vector containing the feature names.
#'
#'@keywords internal
#'
#'@author Alessandro Barberis
create_varnames <- function(nm = NULL, n, prefix = "V"){

  if(missing(n)){stop("Missing input: 'n' must be provided.")}

  if(isTRUE(is.null(nm))){
    o = paste0(prefix, seq(n))
  } else {
    o = nm
  }

  return(o)
}









setGenericVerif <- function(x,y){
  if(!isGeneric(x)){
    methods::setGeneric(x,y)
  }else{

  }
}

#'From factor to dummy matrix
#'@param factor a \code{factor}
#'@return a dummy matrix
#'@keywords internal
factor.to.dummy.matrix <- function(factor){

  if (is.factor(factor)) {
    #expanding factors to a set of dummy variables
    # factor = as.factor(factor);
    ntab = table(factor)
    nc = as.integer(length(ntab))
    out = diag(nc)[as.numeric(factor), ];
    colnames(out) = colnames(out, do.NULL = FALSE, prefix = "col")
  } else {
    stop("input is not a factor")
  }

  return(out);
}


#'From dummy matrix to factor
#'@keywords internal
dummy.matrix.to.factor <- function(x){
  if(is.matrix(x)){
    if(!identical(names(table(x)), y=c("0", "1")) && !identical(names(table(x)), y=c("1", "0"))){stop("Error: input is not a dummy matrix.")}
    #Create a vector of levels
    factor = apply(X = x, MARGIN = 1, FUN = function(x){which(x!=0)})
    #set names
    names(factor) = rownames(x);
    #Convert to factor
    factor = as.factor(factor);

  } else {
    stop("Error: input is not a dummy matrix.")
  }
  return(factor)
}




#'Get the number of classes
#'@param y response variable
#'@param resp.type response type
#'@keywords internal
get_num_of_classes = function(y, resp.type){


  n = switch(resp.type,
             gaussian    = NULL,
             binomial    = 2,
             multinomial = if(!is.null(dim(y))){ncol(y)}else{length(levels(as.factor(y)))},
             mgaussian   = NULL,
             cox         = NULL,
             poisson     = NULL)

  return(n);
}


#'Get the number of observations
#'@param y response variable
#'@param resp.type response type
#'@keywords internal
get_num_of_observations = function(y, resp.type){


  n = switch(resp.type,
             gaussian    = length(y),
             mgaussian   = nrow(y),
             binomial    = if(is.null(dim(y))){length(y)}else{nrow(y)},
             multinomial = if(is.null(dim(y))){length(y)}else{nrow(y)},
             cox         = nrow(y),
             poisson     = length(y))

  return(n);
}





#'Check filename and generate a new one
#'@description This function checks the existence of a file in the
#'specified directory, and in such case generates a new filename.
#'@param outdir path to the output directory
#'@param filename name of the file to check
#'@param ext file extension (e.g. 'txt')
#'@keywords internal
check.filename.and.generate <- function(outdir, filename, ext){
  #default new filename
  newfilename = filename;
  #create the full path
  filename2 = paste0(filename, ".",ext);
  file = file.path(outdir, filename2);
  #Check if exists
  i = 1;
  while(file.exists(file)){
    #Update the new file name
    newfilename = paste0(filename,"_",i);
    #Update the full path
    filename2 = paste0(newfilename,".", ext);
    file = file.path(outdir, filename2);
    i = i + 1;
  }
  return(newfilename);
}

#'Check if function parameter is valid
#'@keywords internal
check_function_arg <- function(arg = NULL, check.na = TRUE, check.null = TRUE){
  is.ok = TRUE

  if(length(arg)>0){
    if(check.na){
      is.ok = !is.na(arg)
    }

    if(check.null){
      is.ok = is.ok & !is.null(arg)
    }
  } else {
    is.ok = FALSE
  }

  return(is.ok)
}


# #'Clean renoir objects
# #'@description This function remove non-essential data from \code{renoir} objects.
# #'@param object object to clean
# #'@param learning.method method for fitting the data
# #'@param ... further arguments
# clean <- function(object, learning.method, ...){
#   UseMethod("clean")
# }

# #'Clean a trained object
# #'@description This function remove non-essential data from
# #'the trained object (e.g. the models fitted for tuning purpose).
# #'@param object a \code{renoir.trained} object
# #'@param learning.method method for fitting the data
# #'@return the cleaned object
# clean.renoir.trained <- function(object, learning.method){
#
#   class = class(object);
#
#   out = switch(learning.method,
#                glmnet = {object[[get_name_slot_train_cvfits()]] = NULL; object}
#   )
#
#   class(out) = class;
#
#   return(out)
# }


#'@keywords internal
get_name_package <- function(){return('renoir')}



#'Create filename for final output
#'@return a generated filename for the final output
#'@keywords internal
get_filename_results <- function(resampling.method, learning.method){
  prefix = switch(resampling.method,
                  multi.random = "multir",
                  bootstrap = "bootstrap",
                  cv = "kcv");

  filename = paste(prefix, learning.method, sep = "_");

  return(filename);
}


#'Reshape precruit
#'@description Reshape recruitment matrix to improve printing.
#'@param x data frame containing the probability of recruitment for each feature of the model
#'@param annotation a data frame with rownames (or one column) matching \code{x} rownames.
#'@param feat.index numeric, the index of the column in \code{annotation} matching \code{x} feature names.
#'\code{0} means rownames.
#'@inheritParams base::data.frame
#'@return a data frame with at least 2 columns (features and probability of recruitment),
#'plus eventual annotation.
#'@keywords internal
reshape_recruitment_matrix <- function(x, annotation, feat.index = 0,
                                       check.names = TRUE){

  #Convert x into a data frame
  if(is.na(match(x = 'features', colnames(x)))){
    df = data.frame(features = rownames(x), x, stringsAsFactors = FALSE, check.names = check.names);
  } else {df = x}


  #if annotation is present, match the data with the current features
  if(!missing(annotation) && !is.na(annotation) && !is.null(annotation)){

    if(identical(x = feat.index, y = 0)){
      #a) get common elements
      inter = intersect(x = df[['features']], y = rownames(annotation))

      if(length(inter)>0){
        #a) Get row/col names
        newm_r = df[['features']];
        newm_c = colnames(annotation);
        #b) create an empty data frame
        # newv = data.frame(row.names = df[['features']], stringsAsFactors = FALSE)
        newdf = data.frame(matrix(data = NA,
                                  nrow = length(newm_r),
                                  ncol = length(newm_c),
                                  dimnames = list(newm_r, newm_c)),
                           stringsAsFactors = F, check.names = check.names)
        #b) add elements
        # newv[inter,] = annotation[inter,,drop=F];
        newdf[inter,] = annotation[inter,];
        #c) update
        outdf = cbind(df[newm_r,,drop=F], newdf[newm_r,,drop=F]);
        #d) select order
        outdf = outdf[,c('features', newm_c, colnames(df)[-1]),drop=F]
      }
    } else {
      #a) get common features
      inter = unique(intersect(x = df[['features']], y = annotation[, feat.index, drop = T]))

      #b) Check if any common
      if(length(inter)>0){
        #c) update annotation to have only meaningful values
        annotation = annotation[annotation[,feat.index,drop = T] %in% inter, ,drop=F]

        #d) create new data frame
        outdf = data.frame(annotation[,feat.index,drop = F], annotation[,-feat.index,drop = F],
                           stringsAsFactors = F, check.names = check.names)

        #e) update first column name to 'features'
        coln = colnames(outdf);
        coln[1] = 'features';
        colnames(outdf) = coln;

        #f) get different features
        diff = setdiff(x = df[['features']], y = inter);

        if(length(diff) > 0){
          #g) create new empty
          newdf = data.frame(matrix(data = NA,
                                    nrow = length(diff),
                                    ncol = length(colnames(outdf)),
                                    dimnames = list(NULL, colnames(outdf))),
                             stringsAsFactors = F, check.names = check.names)

          #h) update new dataframe so that first column has features
          newdf[,1] = diff;

          #i) update output data frame
          outdf = rbind(outdf, newdf)
        }

        #j) attach probability of recruitment to each element
        if(ncol(df)>2){
          p = lapply(X = outdf[,'features'], FUN = function(feat){df[which(df['features'] == feat), -1, drop=F]})

          p = do.call(what = rbind, args = p)
        } else {
          p = sapply(X = outdf[,'features'], FUN = function(feat){df[which(df['features'] == feat), -1]})
        }


        #k) update out data frame
        outdf = cbind(outdf, p);

        #l) update names
        colnames(outdf) = c(coln, colnames(df)[-1])

      } else {outdf = df}
    }

  } else {
    outdf = df;
  }

  return(outdf);

}

# #'@keywords internal
# get_name_regression_model <- function(resp.type, learning.method){
#
#   name = switch(resp.type,
#                 gaussian    = 'linear',
#                 mgaussian   = 'linear',
#                 binomial    = 'logistic',
#                 multinomial = 'multinomial',
#                 poisson     = 'poisson',
#                 cox         = 'Cox')
#
#   learning.method = switch(learning.method,
#                            glmnet = "regression model");
#
#   return(paste(name, learning.method))
# }



#'Create a confusion table in html
#'@param x a confusion matrix as returned by \code{\link[glmnet]{confusion.glmnet}}
#'@inheritParams knitr::kable
#'@inheritParams base::round
#'@return A character vector of the table source code.
#'@keywords internal
print_confusion_table = function (x, digits = max(3, getOption("digits") - 3), format = 'html', ...) {

  #Check the confusion table
  x = check_confusion_matrix(x = x)


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
  tmp = knitr::kable(x = x, format = format, output = TRUE)

  #Add header
  tmp = kableExtra::add_header_above(kable_input = tmp, header = c(" " = 1, "True" = ncol(x)))

  #Add row header
  tmp = kableExtra::pack_rows(kable_input = tmp, group_label = "Predicted", start_row = 1, end_row = nrow(x))

  #Add percent correct
  tmp = kableExtra::footnote(kable_input = tmp, general = paste("\n Percent Correct: ", format(round(correct, digits)), "\n"))

  return(tmp)
}



