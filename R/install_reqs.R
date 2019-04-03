install_special_req = function(elem, pattern, f, dryrun, verbose){
  #' @param elem list element to be worked on
  #' @param pattern regex pattern to match on
  #' @param f the function used to install
  #' @param dryrun if TRUE, no packages will be installed, but you can see what would have happened
  #' @param verbose should the function be explicit about activities
  #'
  #' @return count of failures
  failures = 0

  if (length(elem) > 0) {
    for (i in elem) {
      url = sub(pattern, "", i)
      vmess(sprintf(NOTE_INSTALL_PACKAGE, url, ""), verbose)
      if (!dryrun) {
        tryCatch(f(url),
                 error = function(x) {
                   failures <<- failures + 1
                   message(sprintf(ERROR_OTHER_FAILURE, url))
                 })
      }
    }
  }
  return(failures)
}

install_unversioned = function(elem, installed, dryrun, verbose, repo){
  #' @param elem the list element of unversioned package requirements
  #' @param installed list of installed packages
  #' @param dryrun if TRUE, no packages will be installed, but you can see what would have happened
  #' @param verbose should the function be explicit about activities
  #' @param repo what CRAN repository should be used?
  #'
  #' @return the number of failures
  failures = 0

  for (i in elem) {
    available_versions = get_available_versions(i)
    if (is.null(available_versions)){
      failures = failures + 1
      vmess(sprintf(ERROR_NO_PACKAGE_EXISTS, i), TRUE)
    } else if (is.na(installed[i])) {
      vmess(sprintf(NOTE_PACKAGE_NOT_INSTALLED, i), verbose)
      version = tail(sort(available_versions), 1)
      vmess(sprintf(NOTE_INSTALL_PACKAGE, i, sprintf(NOTE_INSTALL_PACKAGE_VERSION, version)), verbose)
      failures = failures + install_cran_package(package = i,
                                                 version = NULL,
                                                 repo = repo,
                                                 dryrun = dryrun)
    }
  }

  return(failures)
}

install_cran_package = function(package, version, repo, dryrun){
  failures = 0
  if (!dryrun) {
    tryCatch(remotes::install_version(package,
                                       version = version,
                                       repos = repo,
                                       quiet = TRUE),
             error = function(x){
               failures <<- failures + 1
               message(sprintf(ERROR_OTHER_FAILURE, package))
             })
  }
  return(failures)
}

identify_comparison_op = function(req){
  comp_match = vapply(c(COMPS,'='),
                      grepl,
                      x = req,
                      fixed = TRUE,
                      FUN.VALUE = logical(1))
  if(any(comp_match)){
    comp = names(which.max(comp_match))
  } else {
    comp = NA_character_
  }
  return(comp)
}

process_versioned_requirement = function(req){
  comp = identify_comparison_op(req)
  split = strsplit(req, comp)[[1]]
  if (comp == "=") comp = COMP_EXACTLY_EQUAL
  package = gsub(" *", "", split[1])
  version = gsub(" *", "", split[2])
  return(list(package = package,
              version = version,
              comp = comp))
}

is_versioned_install_needed = function(package, version, comp, installed, verbose){
  install_needed = FALSE
  if (is.na(installed[package])) {
    vmess(sprintf(NOTE_PACKAGE_NOT_INSTALLED, package), verbose)
    install_needed = TRUE
  } else {
    install_needed = !compare_version(installed[package], version, comp)
  }
  return(install_needed)
}

install_if_compat_available = function(processed_element, repo, verbose, dryrun){
  failures = 0

  package = processed_element$package
  version = processed_element$version
  comp = processed_element$comp

  available_versions = get_available_versions(package)

  available_compatibility = vapply(available_versions,
                                   FUN = compare_version,
                                   FUN.VALUE = logical(1),
                                   target = version,
                                   comp = comp)

  if (!any(available_compatibility)) {
    failures = failures + 1
    vmess(sprintf(ERROR_NO_PACKAGE_EXISTS, package), TRUE)
  } else {
    latest_compatible = names(which.max(which(available_compatibility)))
    vmess(sprintf(NOTE_INSTALL_PACKAGE, package,
                  sprintf(NOTE_INSTALL_PACKAGE_VERSION,
                          latest_compatible)),
          verbose)
    install_cran_package(package, latest_compatible, repo, dryrun)
  }
  return(failures)
}

install_versioned = function(elem, installed, dryrun, verbose, repo){
  #' @param elem the list element of unversioned package requirements
  #' @param installed list of installed packages
  #' @param dryrun if TRUE, no packages will be installed, but you can see what would have happened
  #' @param verbose should the function be explicit about activities
  #' @param repo what CRAN repository should be used?
  #'
  #' @return the number of failures
  failures = 0

  for (i in elem) {
    processed_element = process_versioned_requirement(i)
    package = processed_element$package
    version = processed_element$version
    comp = processed_element$comp

    install_needed = is_versioned_install_needed(package, version, comp, installed, verbose)
    if (!install_needed) next

    if (!is.na(installed[package])) {
      vmess(sprintf(NOTE_BAD_PACKAGE_VERSION, package, installed[package]), verbose)
    }

    package_failure = install_if_compat_available(processed_element, repo, verbose, dryrun)
    failures = failures + package_failure
  }
  return(failures)
}

install_reqs = function(reqs, dryrun, verbose = dryrun,
                        repo = options()$repo[1], ...){
  #' @param reqs results of process_requirements_file
  #' @param dryrun if TRUE, no packages will be installed, but you can see what would have happened
  #' @param verbose should the function be explicit about activities
  #' @param repo what CRAN repository should be used?
  #' @param ... values to pass to get_installed
  #' @return data.frame of installed packages and their versions

  installed = get_installed(...)

  failures = 0

  # Install git
  failures = failures +
    install_special_req(reqs$git, GIT_REPLACE, remotes::install_git, dryrun, verbose) +
    install_special_req(reqs$svn, SVN_EXT_REP, remotes::install_svn, dryrun, verbose) +
    install_special_req(reqs$bioc, BIO_EXT_REP, remotes::install_bioc, dryrun, verbose) +
    install_special_req(reqs$url, "", remotes::install_url, dryrun, verbose) +
    install_special_req(reqs$local, "", remotes::install_local, dryrun, verbose) +
    install_unversioned(reqs$unversioned, installed, dryrun, verbose, repo) +
    install_versioned(reqs$versioned, installed, dryrun, verbose, repo)

  if (failures > 0) stop(sprintf(ERROR_FAILURE_COUNT, failures))

  return(0)
}
