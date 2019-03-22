#' @export
install_requirements <- function(requirements = file.path(getwd(),"requirements.txt"), 
                                 install = TRUE, packrat = FALSE, dryrun = FALSE,
                                 verbose = TRUE, repo = "https://cran.rstudio.com") {
  #' @title Install project requirements
  #'
  #' @description Install (and optionally generate) required packages
  #'
  #' @param requirements filename where requirements are stored
  #' @param gen Should required packages be generated?
  #' @param packrat Should project be initialized with packrat?
  #' @param dryrun Flag if you just want to see what would happen if you ran
  #' the function
  #' @param verbose Flag to change verbosity
  #' @param repo What repository should be used to install pacakges?
  #'
  #' @return invisible
  #' @examples
  #' \dontrun{
  #' requirements(dryrun = TRUE)
  #' }
  content = process_requirements_file(requirements)

  if(packrat){
    tryCatch(packrat::status(),
             error=function(x){packrat::init();packrat::on()})
  }

  required = strsplit(content," +")
  installed = installed.packages()[,3]
  needed = vapply(required,compare_and_install,FUN.VALUE = 1,
                  existing=installed,
                  dryrun=dryrun,
                  verbose=verbose,
                  repo=repo,
                  install=install)

  loading = lapply(required,load_packages,
                   verbose=verbose,
                   dryrun=dryrun)

  if(dryrun) cat("NOTE: This was just a dry run. No packages have been installed.")
  return(invisible(0))
}
