#' @export
install_requirements <- function(requirements = "requirements.txt", 
                                 packrat = FALSE, dryrun = FALSE,
                                 verbose = dryrun, repo = options()$repo[1],
                                 gen = !file.exists(requirements), 
                                 glob_paths = file.path(dirname(requirements),"*.R")) {
  #' @title Install project requirements
  #'
  #' @description Install (and optionally generate) required packages
  #'
  #' @param requirements filename where requirements are stored
  #' @param packrat Should project be initialized with packrat?
  #' @param dryrun Flag if you just want to see what would happen if you ran
  #' the function
  #' @param verbose Flag to change verbosity
  #' @param repo What repository should be used to install pacakges?
  #' @param gen Should required packages be generated?
  #' @param glob_paths Character vector of patterns for relative or absolute filepaths. Missing values will be ignored. See ?Sys.glob for more details.
  #'
  #' @return invisible
  #' @examples
  #' \dontrun{
  #' requirements(dryrun = TRUE)
  #' }
  if(gen) generate_requirements(glob_paths = glob_paths,
                                output_path = requirements)
  
  reqs = process_requirements_file(requirements)

  if(packrat) activate_packrat()

  tmp = install_reqs(reqs = reqs,
                     dryrun = dryrun, 
                     verbose = verbose, 
                     repo = repo)

  if(dryrun) cat("NOTE: This was just a dry run. No packages have been installed.")
  return(invisible(0))
}
