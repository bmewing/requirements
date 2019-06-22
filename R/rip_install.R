#' @export
rip_install = function(requirements = "requirements.txt",
                       packrat = FALSE, dryrun = FALSE,
                       verbose = dryrun, repo = options()$repo[1],
                       gen = !file.exists(requirements),
                       inspect_files = c("R", "Rmd", "Rpres", "lock"),
                       path = dirname(requirements),
                       ...) {
  #' @title Install project requirements
  #'
  #' @description Install (and optionally generate) required packages
  #'
  #' @param requirements filename where requirements are stored
  #' @param packrat Should project be initialized with packrat?
  #' @param dryrun Flag if you just want to see what would happen if you ran
  #' the function
  #' @param verbose Flag to change verbosity
  #' @param repo What repository should be used to install packages?
  #' @param gen Should required packages be generated?
  #' @param inspect_files Character vector of file extensions to search for dependencies in.
  #'                      Officially supports the following extensions: `c("R", "Rmd", "Rpres", "lock")`.
  #'                      Other extensions will be processed as if they were `.R` files.
  #' @param path Location where extensions from `inspect_files` should be searched for.
  #'             (see `path` from `?list.files`).
  #' @param ... Additional arguments to be passed to `rip_freeze()`
  #'
  #' @return invisible
  #' @examples
  #' \dontrun{
  #' requirements(dryrun = TRUE)
  #' }
  if (gen) rip_freeze(output_path = requirements,
                      inspect_files = inspect_files,
                      path = path,
                      ...)

  reqs = process_requirements_file(requirements)

  if (packrat) activate_packrat()

  tmp = install_reqs(reqs = reqs,
                     dryrun = dryrun,
                     verbose = verbose,
                     repo = repo)

  if (dryrun) message("NOTE: This was just a dry run. No packages have been installed.")

  return(invisible(0))
}
