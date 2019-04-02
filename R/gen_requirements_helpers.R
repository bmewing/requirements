

match_packages_from_files = function(file_paths) {
  #' @keywords internal
  #' Match valid package names from files
  #'
  #' @param file_paths Character vector of files to check for package references.
  #'
  #' @return Character vector of package names matched.
  deps_list = lapply(file_paths, packrat:::fileDependencies)

  if (length(deps_list) == 0) return(character(0))

  deps_vec = unique(unlist(deps_list))
  return(deps_vec[legal_r_package_name(deps_vec)])
}


safe_package_version = function(pkg) {
  #' @keywords internal
  #' Wrapper for packageVersion to fail gracefully
  #'
  #' @param pkg String of package name to retrieve version for
  #'
  #' @return Package version as string; \code{NA} if unavailable.
  tryCatch(
    expr = {
      return(packageVersion(pkg))
    },
    error = function(e) {
      return(NA_character_)
    }
  )
}


validate_eq_sym = function(eq_sym) {
  #' @keywords internal
  #' Raise exception if comparison operator is invalid
  #'
  #' @param eq_sym The equality symbol to be used when writing requirements (i.e. package>=1.0.0).
  #' @return None; exception is raised if invalid symbol
  stopifnot(length(eq_sym) == 1)

  if (eq_sym == "=") return(COMP_EXACTLY_EQUAL)

  if (!(eq_sym %in% COMPS)) {
    comps_str = paste(COMPS, collapse = "', '")
    error_msg = "'%s' is not a valid comparison operator; valid operators are: '%s'"
    fmt_error_msg = sprintf(error_msg, eq_sym, comps_str)
    stop(fmt_error_msg)
  }
  return(eq_sym)
}


append_version_requirement = function(pkg, eq_sym=COMP_GTE, rm_missing=FALSE) {
  #' @keywords internal
  #' Paste version requirements to character vector of package names
  #'
  #' @param package_names String containing package name.
  #' @param eq_sym The equality symbol to be used when writing requirements (i.e. package>=1.0.0).
  #'   Use \code{NULL} to not include package versions in your requirements file.
  #' @param rm_missing Should packages not installed locally be exlcuded from output?
  #'
  #' @return Character vector of versioned package names;
  #'   \code{NA} if package unavailable and \code{isTRUE(rm_missing)}.
  pkg_version = safe_package_version(pkg)

  if (rm_missing & is.na(pkg_version)) {
    versioned_package = NA_character_
  } else if (is.na(pkg_version) | is.null(eq_sym)) {
    versioned_package = pkg
  } else {
    eq_sym = validate_eq_sym(eq_sym)
    versioned_package = paste0(pkg, eq_sym, pkg_version)
  }

  return(versioned_package)
}


append_version_requirements = function(package_names, eq_sym=COMP_GTE, rm_missing=FALSE) {
  #' @keywords internal
  #' Paste version requirements to character vector of package names
  #'
  #' @param package_names Character vector of package names.
  #' @param eq_sym The equality symbol to be used when writing requirements (i.e. package>=1.0.0).
  #'   Use \code{NULL} to not include package versions in your requirements file.
  #' @param rm_missing Should packages not installed locally be exlcuded from output?
  #'
  #' @return Character vector of versioned package names.
  versioned_packages = vapply(package_names, function(p) {
    append_version_requirement(p, eq_sym, rm_missing)
  }, character(1), USE.NAMES = FALSE)  # nolint

  return(versioned_packages[!is.na(versioned_packages)])
}


write_requirements_file = function(package_requirements, file_path="requirements.txt") {
  #' @keywords internal
  #' Helper for writing requirements to file
  #'
  #' Wrapper of writeLines with default file_path
  #'
  #' @param package_requirements Character vector of requirements to write to file.
  #' @param file_path Path to write requirements to.
  if (length(package_requirements) == 0) message("No dependencies found. Writing a blank requirements file.")

  requirements_file_contents = c(AUTO_GEN_COMMENTS, package_requirements)

  writeLines(requirements_file_contents, file_path)
}
