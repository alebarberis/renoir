#'Student's t-Test
#'@description Computes the two sample t-tests for each variable and returns the relative
#'non-adjusted p-value.
#'@keywords internal
ttest = function(x, g, alternative="two.sided", ...){
  #Get the indices of the 2 groups
  g = as.factor(g);
  # lvls = as.numeric(levels(g))
  lvls = levels(g)

  index.g1 = which(g == lvls[1])
  index.g2 = which(g == lvls[2])

  r = matrixTests::row_t_welch(x=x[,index.g1,drop=F], y = x[,index.g2,drop=F], ...);
  return(r$pvalue);
}

#'Wilcoxon Rank Sum and Signed Rank Tests
#'@inheritParams stats::wilcox.test
#'@keywords internal
wtest = function(x, g, alternative="two.sided", ...){
  #Get the indices of the 2 groups
  g = as.factor(g);
  # lvls = as.numeric(levels(g))
  lvls = levels(g)

  index.g1 = which(g == lvls[1]);
  index.g2 = which(g == lvls[2]);

  #Results
  r = matrixTests::row_wilcoxon_twosample(x=x[, index.g1,drop=F], y = x[, index.g2,drop=F], ...)
  return(r$pvalue);
}


#'Kruskal-Wallis Rank Sum Test
#'@inheritParams stats::kruskal.test
#'@keywords internal
kw = function(x, g){
  r = matrixTests::row_kruskalwallis(x = x, g=g);
  return(r$pvalue);
}


#'Fit an Analysis of Variance Model
#'@inheritParams stats::aov
#'@keywords internal
anova = function(x, g){
  r = matrixTests::row_oneway_equalvar(x = x, g=g);
  return(r$pvalue);
}


#'Compute score
#'@description Compute mutual information or a derived measure.
#'@param out.type the output type
#'\describe{
#'   \item{mutual.information}{the mutual information; it is zero when the two variables are independent}
#'   \item{redundancy}{a scaled and symmetric version of the mutual information;
#'   it has a minimum value of zero when the two variables are independent; its maximum value is given
#'   by min(H(X),H(Y))/(H(X)+H(Y))}
#'   \item{symmetric.uncertainty}{another symmetric version of the mutual information;
#'   it has a minimum value of zero when the two variables are independent}
#'   \item{normalised.mutual.information}{it is derived from thinking of mutual information as
#'   an analogue to covariance, and it is calculated akin to the Pearson correlation coefficient;
#'   it ranges between \code{[0, 1]}, where 0 is related to two independent variables}
#'}
#'@return a numerical value
#'@keywords internal
mutual_information <- function(
  # X,
  # Y,
  x, g,
  discretization.method = "doane",
  out.type = c("mutual.information",
                "redundancy",
                "symmetric.uncertainty",
                "normalised.mutual.information"),
  verbose = F){


  #Get the indices of the 2 groups
  g = as.factor(g);
  # lvls = as.numeric(levels(g))
  lvls = levels(g)

  index.g1 = which(g == lvls[1]);
  index.g2 = which(g == lvls[2]);

  X = x[,index.g1]
  Y = x[,index.g2]

  #----------------------------------------------------------------------------#
  out.type = match.arg(out.type)

  #----------------------------------------------------------------------------#
  if(is.numeric(X) && length(unique(range(X)))!=2) {
    out = NA
    warning("\nCheck X data range\n")
  } else if (is.numeric(Y) && length(unique(range(Y)))!=2){
    out = NA
    warning("\nCheck Y data range\n")
  } else {
    #----------------------------------------------------------------------------#
    #estimates the mutual information from observed data
    if(verbose){cat(paste(Sys.time(), "Estimating the mutual information from observed data..."), sep = "")}
    mutual.information = varrank::mi.data(X = X,
                                          Y = Y,
                                          discretization.method = discretization.method,
                                          k=NULL)
    if(verbose){cat("DONE", sep = "\n")}

    #DEBUG
    if(mutual.information<0){
      # tmpdir = tempdir()
      # dir.create(path = tmpdir, showWarnings = T, recursive = T)
      # filepath = file.path(tmpdir, "mylocals.Rda")
      # save(list=ls(), file=filepath)
      # stop(paste("\nComputed negative mutual information: check data stored at\n", filepath,"\n"))

      # warning(paste("Computed negative mutual information: check data stored at\n", filepath,"\n"))

      warning(paste("Computed negative mutual information\n"))
    }

    #----------------------------------------------------------------------------#
    #check output type
    if(!identical(out.type, "mutual.information")){

      #discretizes data frame of possibly continuous random variables
      if(verbose){cat(paste(Sys.time(), "Discretising data frame of possibly continuous random variables..."), sep = "")}
      freqs.table.X = varrank::discretization(data.df = X,
                                              discretization.method = discretization.method,
                                              frequency = TRUE)[[1]]

      freqs.table.Y = varrank::discretization(data.df = Y,
                                              discretization.method = discretization.method,
                                              frequency = TRUE)[[1]]
      if(verbose){cat("DONE", sep = "\n")}
      #----------------------------------------------------------------------------#
      #Computes an Empirical Estimation of the Entropy
      if(verbose){cat(paste(Sys.time(), "Computing an empirical estimation of the Entropy..."), sep = "")}
      Hx <- varrank::entropy.data(freqs.table = freqs.table.X)
      Hy <- varrank::entropy.data(freqs.table = freqs.table.Y)
      if(verbose){cat("DONE", sep = "\n")}

      #----------------------------------------------------------------------------#
      #normalise
      if(verbose){cat(paste(Sys.time(), "Normalising the data..."), sep = "")}
      out = switch(out.type,
                   'redundancy' = mutual.information / (Hx + Hy),
                   'symmetric.uncertainty' = 2 * (mutual.information / (Hx + Hy)),
                   'normalised.mutual.information' = mutual.information / sqrt(Hx * Hy)
      )
      if(verbose){cat("DONE", sep = "\n")}

    } else {
      out = mutual.information
    }
  }


  #----------------------------------------------------------------------------#
  #return
  return(out)
}
