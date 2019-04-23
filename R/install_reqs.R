install_special_req = function(i, pattern, f, dryrun, verbose){
  #' @param i single special requirement to be installed
  #' @param pattern regex pattern to match on
  #' @param f the function used to install
  #' @param dryrun if TRUE, no packages will be installed, but you can see what would have happened
  #' @param verbose should the function be explicit about activities
  #'
  #' @return count of failures
  failures = 0
  url = sub(pattern, "", i)
  vmess(sprintf(NOTE_INSTALL_PACKAGE, url, ""), verbose)
  if (!dryrun) {
    tryCatch(f(url),
             error = function(x) {
               failures <<- failures + 1 # nolint
               message(sprintf(ERROR_OTHER_FAILURE, url))
             })
  }
  return(failures)
}

install_unversioned = function(i, installed, dryrun, verbose, repo){
  #' @param i single unversioned package requirement (package name)
  #' @param installed list of installed packages
  #' @param dryrun if TRUE, no packages will be installed, but you can see what would have happened
  #' @param verbose should the function be explicit about activities
  #' @param repo what CRAN repository should be used?
  #'
  #' @return the number of failures
  failures = 0
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
               failures <<- failures + 1 # nolint
               message(sprintf(ERROR_OTHER_FAILURE, package))
             })
  }
  return(failures)
}

process_versioned_requirement = function(req){
  comp = identify_comparison_op(req)
  split = strsplit(req, comp)[[1]]
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

install_versioned = function(i, installed, dryrun, verbose, repo){
  #' @param i single versioned package requirements (package comp version)
  #' @param installed list of installed packages
  #' @param dryrun if TRUE, no packages will be installed, but you can see what would have happened
  #' @param verbose should the function be explicit about activities
  #' @param repo what CRAN repository should be used?
  #'
  #' @return the number of failures
  failures = 0
  processed_element = process_versioned_requirement(i)
  package = processed_element$package
  version = processed_element$version
  comp = processed_element$comp
  
  install_needed = is_versioned_install_needed(package, version, comp, installed, verbose)
  if (!install_needed) return(0)
  
  if (!is.na(installed[package])) {
    vmess(sprintf(NOTE_BAD_PACKAGE_VERSION, package, installed[package]), verbose)
  }
  
  package_failure = install_if_compat_available(processed_element, repo, verbose, dryrun)
  failures = failures + package_failure
  return(failures)
}


count_failures = function(elem, func, ...){
  #' @param elem the requirements to be installed
  #' @param func the internal function used to handle installation
  #' @param ... other arguments to pass to the internal function
  if(length(elem) > 0){
    return(sum(vapply(elem, func, FUN.VALUE = numeric(1), ...)))
  } else {
    return(0)
  }
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
    count_failures(reqs$git, install_special_req, GIT_REPLACE, remotes::install_git, dryrun, verbose) +
    count_failures(reqs$svn, install_special_req, SVN_EXT_REP, remotes::install_svn, dryrun, verbose) +
    count_failures(reqs$bioc, install_special_req, BIO_EXT_REP, remotes::install_bioc, dryrun, verbose) +
    count_failures(reqs$url, install_special_req, "", remotes::install_url, dryrun, verbose) +
    count_failures(reqs$local, install_special_req, "", remotes::install_local, dryrun, verbose) +
    count_failures(reqs$unversioned, install_unversioned, installed, dryrun, verbose, repo) +
    count_failures(reqs$versioned, install_versioned, installed, dryrun, verbose, repo)
  
  if (failures > 0) stop(sprintf(ERROR_FAILURE_COUNT, failures))
  
  return(0)
}
