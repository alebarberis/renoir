#'Check the existance of a directory
#'@keywords internal
check_output_dir <- function(outdir){
  if(!missing(outdir) && !is.null(outdir)){hasOutdir = T} else {hasOutdir = F}
  if(hasOutdir && !dir.exists(outdir)){dir.create(outdir, recursive = TRUE)}

  return(hasOutdir)
}

#'Check the existance of a file
#'@keywords internal
check.file.path <- function(path){
  if(!missing(path) && !is.null(path) && dir.exists(dirname(path))){
    bool = TRUE;
  } else {
    bool = F;
  }
  return(bool);

}
