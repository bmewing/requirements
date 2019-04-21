
extract_lockfile_info = function(lockfile_lines, field_name) {
  #' @keywords internal
  #' Extract values of all fields in lockfile matching a given field name
  #'
  #' @param lockfile_lines output of readLines applied to a packrat lockfile
  #' @param field_name name of field to extract information from.
  #'   Use just the bare name. Do NOT include regex nor colon nor space; this will be added in this function.
  #' @return character vector of all values found matching the given field name
  #'
  #' @examples
  #' \dontrun {
  #'  lockfile_lines = readLines("packrat/packrat.lock")
  #'  extract_lockfile_info(lockfile_lines, 'Package')
  #'  # [1] "BH" "DT"
  #'
  #'  extract_lockfile_info(lockfile_lines, 'Version')
  #'  # [1] "1.62.0-1" "0.2"
  #' }
  field_name_re = paste0("^", field_name, ": ")
  field_lines = grep(field_name_re, lockfile_lines, value = TRUE)
  field_values = gsub(field_name_re, "", field_lines)

  return(field_values)
}


read_reqs_from_lockfile = function(lockfile_path = "packrat/packrat.lock", eq_sym = ">=") {
  #' @keywords internal
  #' Read packrat lockfile and extract package names & versions from data
  #'
  #' @param lockfile_path path to packrat lockfile
  #' @param eq_sym The equality symbol to be used when writing requirements (i.e. package>=1.0.0).
  #'   Use \code{NULL} to not include package versions in your requirements file.
  #' @return character vector of requirements
  #'
  #' @examples
  #' read_packages_from_lockfile()
  #' [1] "BH>=1.62.0-1" "DT>=0.2"
  #'
  #' read_packages_from_lockfile(eq_sym = NULL)
  #' [1] "BH" "DT"
  lockfile_lines = readLines(lockfile_path)

  package_names = extract_lockfile_info(lockfile_lines, "Package")

  if (is.null(eq_sym) | length(package_names) == 0) return(package_names)
  eq_sym = validate_eq_sym(eq_sym)

  version_numbers = extract_lockfile_info(lockfile_lines, "Version")

  if (length(package_names) != length(version_numbers)) {
    stop("Malformed lockfile.  Each package listed in lockfile must also have a version.")
  }

  mapply(paste0, package_names, eq_sym, version_numbers, USE.NAMES = FALSE)
}


read_package_lines_from_file = function(file_path, filter_words) {
  #' @keywords internal
  #' Pull package referencing lines from R file
  #'
  #' @param file_path String containg path to file to check for package references.
  #' @param filter_words Character vector of 'words' to use as filter for package references.
  #'
  #' @return Character vector containing package referencing lines from \code{file_path}.
  all_file_lines = readLines(file_path)

  package_lines_re = paste(filter_words, collapse = "|")
  package_lines = all_file_lines[grep(package_lines_re, all_file_lines)]

  return(unique(package_lines))
}


read_package_lines_from_files = function(file_paths, filter_words=c("::", "library", "require", "p_load")) {
  #' @keywords internal
  #' Pull package referencing lines from R files
  #'
  #' @param file_paths Character vector of files to check for package references.
  #' @param filter_words Character vector of 'words' to use as filter for package references.
  #'
  #' @return Character vector containing package referencing lines from \code{file_paths}.
  if (length(file_paths) == 0) return(character(0))

  package_lines_list = lapply(file_paths, read_package_lines_from_file, filter_words = filter_words)
  uniq_package_lines = unique(unlist(package_lines_list))

  return(uniq_package_lines)
}


str_match_all = function(string, pattern) {
  #' @keywords internal
  #' Extract capture groups from vector of strings with regex (made to emulate stringr::str_match_all)
  #'
  #' @param string character vector to extract capture groups from
  #' @param pattern len 1 character vector containing regex to use for matching/capture group extraction
  #' @return character vector containing captured output found in string
  #'
  #' @examples
  #' str_match_all(c('test', 'best'), '(.)est')
  #' # [[1]]
  #' #      [,1]   [,2]
  #' # [1,] "test" "t"
  #' #
  #' # [[2]]
  #' #      [,1]   [,2]
  #' # [1,] "best" "b"
  #'
  #' str_match_all(c('test', 'best'), 'x')
  #' # [[1]]
  #' #      [,1]
  #' #
  #' # [[2]]
  #' #      [,1]

  # Below implementation taken from last example in ?grep documentation & modified
  # > There is no gregexec() yet, but one can emulate it by running
  # > regexec() on the regmatches obtained via gregexpr().
  matches_list = lapply(regmatches(string, gregexpr(pattern, string)), function(e) {
    matches = regmatches(e, regexec(pattern, e))

    do.call(rbind, matches)
  })

  # Elements of string that weren't matched by pattern are NULL in matches_list.
  # The below process replaces NULL elements with a row-less matrix that matches
  # the column number non-NULL elements.
  null_matches = vapply(matches_list, is.null, logical(1))
  n_null_matches = sum(null_matches)

  if (n_null_matches == 0) return(matches_list)

  if (n_null_matches == length(matches_list)) {
    null_matrix_placeholder = matrix(character(0), nrow = 0, ncol = 1)
  } else {
    non_null_n_col = max(vapply(matches_list[!null_matches], ncol, integer(1)))
    null_matrix_placeholder = matrix(character(0), nrow = 0, ncol = non_null_n_col)
  }

  matches_list[null_matches] = rep(list(null_matrix_placeholder), n_null_matches)

  return(matches_list)
}


match_package = function(candidate_lines, package_regex) {
  #' @keywords internal
  #' Match valid package names from character vector of lines of R code
  #'
  #' @param candidate_lines Character vector of code lines to search for package references.
  #' @param package_regex Regex as string to be used to match package references in code.
  #'
  #' @return Character vector of package names matched.
  lib_matches_list = str_match_all(candidate_lines, package_regex)
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
  if (length(candidate_lines) == 0) return(character(0))

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
