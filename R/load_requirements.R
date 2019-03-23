#' @export
load_requirements <- function(requirements = file.path(getwd(),"requirements.txt"), 
                              install = TRUE, packrat = FALSE, dryrun = FALSE,
                              verbose = TRUE, repo = "https://cran.rstudio.com") {
  #' @title Load project requirements
  #'
  #' @description Load (and optionally install) required packages
  #'
  #' @param requirements filename where requirements are stored
  #' @param install Should required packages be installed?
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
  
  return(invisible(0))
}
