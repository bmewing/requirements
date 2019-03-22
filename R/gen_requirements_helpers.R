
read_package_lines_from_file = function(file_path, filter_words=c('::', 'library', 'require', 'p_load')) {
  #' @keywords internal
  #' Pull package referencing lines from R file
  #'
  #' @param file_path String containg path to file to check for package references.
  #' @param filter_words Character vector of 'words' to use as filter for package references.
  #'
  #' @return Character vector containing package referencing lines from \code{file_path}.
  all_file_lines = readLines(file_path)

  package_lines_re = paste(filter_words, collapse = '|')
  package_lines = all_file_lines[grep(package_lines_re, all_file_lines)]

  return(unique(package_lines))
}


read_package_lines_from_files = function(file_paths, filter_words=c('::', 'library', 'require', 'p_load')) {
  #' @keywords internal
  #' Pull package referencing lines from R files
  #'
  #' @param file_paths Character vector of files to check for package references.
  #' @param filter_words Character vector of 'words' to use as filter for package references.
  #'
  #' @return Character vector containing package referencing lines from \code{file_paths}.
  package_lines_list = lapply(file_paths, read_package_lines_from_file)
  uniq_package_lines = unique(unlist(package_lines_list))

  return(uniq_package_lines)
}


match_package = function(candidate_lines, package_regex) {
  #' @keywords internal
  #' Match valid package names from character vector of lines of R code
  #'
  #' @param candidate_lines Character vector of code lines to search for package references.
  #' @param package_regex Regex as string to be used to match package references in code.
  #'
  #' @return Character vector of package names matched.
  lib_matches_list = stringr::str_match_all(candidate_lines, package_regex)
  lib_matches_str = unlist(lapply(lib_matches_list, function(m) m[, 2]))
  lib_matches_str = lib_matches_str[!is.na(lib_matches_str)]

  return(unique(lib_matches_str))
}


match_packages = function(candidate_lines, package_regexes) {
  #' @keywords internal
  #' Match valid package names from character vector of lines of R code
  #'
  #' @param candidate_lines Character vector of code lines to search for package references.
  #' @param package_regexes Character vector of regexes used to match package references in code.
  #'
  #' @return Character vector of package names matched.
  match_list = lapply(package_regexes, function(r) match_package(candidate_lines, r))
  uniq_matches = unique(unlist(match_list))

  return(uniq_matches)
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
  # TODO: implement this
  return(eq_sym)
}


append_version_requirement = function(pkg, eq_sym='>=', rm_missing=FALSE) {
  #' @keywords internal
  #' Paste version requirements to character vector of package names
  #'
  #' @param package_names String containing package name.
  #' @param eq_sym The equality symbol to be used when writing requirements (i.e. package>=1.0.0).
  #'   Use \code{NULL} to not include package versions in your requirements file.
  #' @param rm_missing Should packages not installed locally be exlcuded from output?
  #'
  #' @return Character vector of versioned package names; \code{NA} if package unavailable and \code{isTRUE(rm_missing)}.
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


append_version_requirements = function(package_names, eq_sym='>=', rm_missing=FALSE) {
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
  }, character(1), USE.NAMES = FALSE)

  return(versioned_packages[!is.na(versioned_packages)])
}


write_requirements_file = function(package_requirements, file_path='requirements.txt') {
  #' @keywords internal
  #' Helper for writing requirements to file
  #'
  #' Wrapper of writeLines with default file_path
  #'
  #' @param package_requirements Character vector of requirements to write to file.
  #' @param file_path Path to write requirements to.
  writeLines(package_requirements, file_path)
}
