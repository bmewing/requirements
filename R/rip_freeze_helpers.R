
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
  lockfile_lines = trimws(readLines(lockfile_path))

  package_names = extract_lockfile_info(lockfile_lines, "Package")

  matched_packages_df = data.frame(name = package_names,
                                   version = rep(NA_character_, length(package_names)),
                                   stringsAsFactors = FALSE)

  if (is.null(eq_sym) | nrow(matched_packages_df) == 0) return(matched_packages_df)

  eq_sym = validate_eq_sym(trimws(eq_sym))

  version_numbers = extract_lockfile_info(lockfile_lines, "Version")

  if (length(package_names) != length(version_numbers)) {
    stop("Malformed lockfile.  Each package listed in lockfile must also have a version.")
  }

  matched_packages_df[["version"]] = version_numbers

  return(matched_packages_df)
}


read_rmd_code_chunk_lines = function(file_path, filter_words) {
  #' @keywords internal
  #' Pull package referencing lines from an Rmd file's R code chunks
  #'
  #' @param file_path String containing path to Rmd file to check for package references.
  #' @param filter_words Character vector of 'words' to use as filter for package references.
  #'
  #' @return Character vector containing package referencing lines from \code{file_path}.

  rmd_lines = readLines(file_path)

  # Find lines that start with ```{r
  chunk_start_lines = grep("^```\\{r\\b.*\\}", rmd_lines)

  # Find lines that start with ```
  potential_chunk_end_lines = grep("^```", rmd_lines)

  # Rm ```{r lines from candidates list of chunk enders
  potential_chunk_end_lines = setdiff(potential_chunk_end_lines, chunk_start_lines)

  rmd_code_chunks = lapply(chunk_start_lines, function(chunk_start_line) {
    # Find which candidate endline occurs most soon after an r code chunk start line
    all_line_diffs = potential_chunk_end_lines - chunk_start_line
    valid_line_diffs = all_line_diffs[all_line_diffs > 0]
    chunk_end_line_diff = valid_line_diffs[which.min(valid_line_diffs)]
    chunk_end_line = potential_chunk_end_lines[all_line_diffs == chunk_end_line_diff]

    # Subset codechunk lines (drop ``` lines)
    rmd_lines[(chunk_start_line + 1):(chunk_end_line - 1)]
  })

  return(unlist(rmd_code_chunks))
}


read_package_lines_from_file = function(file_path, filter_words = c("::", "library", "require", "p_load")) {
  #' @keywords internal
  #' Pull package referencing lines from R file
  #'
  #' @param file_path String containing path to file to check for package references.
  #' @param filter_words Character vector of 'words' to use as filter for package references.
  #'
  #' @return Character vector containing package referencing lines from \code{file_path}.
  file_extension = tolower(tools::file_ext(file_path))
  if (file_extension %in% c("rmd", "rpres")) {
    file_reader = read_rmd_code_chunk_lines
  } else {
    file_reader = readLines
  }

  all_file_lines = file_reader(file_path)

  package_lines_re = paste(filter_words, collapse = "|")
  package_lines = all_file_lines[grep(package_lines_re, all_file_lines)]

  return(unique(package_lines))
}


read_package_lines_from_files = function(file_paths, filter_words = c("::", "library", "require", "p_load")) {
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
    null_matrix_placeholder = matrix(character(0), nrow = 0, ncol = 2)
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


safe_packageVersion = function(pkg) {  # nolint
  #' @keywords internal
  #' Wrapper for packageVersion to fail gracefully
  #'
  #' @param pkg String of package name to retrieve version for
  #'
  #' @return Package version as string; \code{NA} if unavailable.
  tryCatch(
    expr = {
      return(as.character(packageVersion(pkg)))
    },
    error = function(e) {
      return(NA_character_)
    }
  )
}


desc_order = function(..., na.last = TRUE, method = c("auto", "shell", "radix")) {  # nolint
  order(..., na.last = na.last, decreasing = TRUE, method = method)
}


order_package_versions = function(version_str, decreasing = TRUE) {
  #' @keywords internal
  #' Wrapper for package_version to fail gracefully
  #'
  #' @param version_str Character vector of package version strings to be coerced to package_version class
  #'
  #' @return Vector of package_version objects.

  version_vec_list = lapply(version_str, function(p) {
    p_version = NA_real_
    try({p_version = package_version(p)}, silent = TRUE)  # nolint
    class(p_version) = "list"
    p_version[[1]]
  })  # nolint

  version_mat = suppressWarnings(do.call(rbind, version_vec_list))
  version_df = as.data.frame(version_mat)

  order_func = if (decreasing) desc_order else order
  version_order = do.call(order_func, version_df)

  return(version_order)
}


validate_eq_sym = function(eq_sym) {
  #' @keywords internal
  #' Raise exception if comparison operator is invalid
  #'
  #' @param eq_sym The equality symbol to be used when writing requirements (i.e. package>=1.0.0).
  #' @return None; exception is raised if invalid symbol
  stopifnot(length(eq_sym) == 1)

  if (!(eq_sym %in% COMPS)) {
    comps_str = paste(COMPS, collapse = "', '")
    error_msg = "'%s' is not a valid comparison operator; valid operators are: '%s'"
    fmt_error_msg = sprintf(error_msg, eq_sym, comps_str)
    stop(fmt_error_msg)
  }
  return(eq_sym)
}


append_version_requirements = function(matched_packages_df, eq_sym = COMP_GTE) {
  #' @keywords internal
  #' Paste requirement names and version numbers together
  #'
  #' @param matched_packages_df data.frame with columns c("name", "version")
  #' @param eq_sym The equality symbol to be used when writing requirements (i.e. package>=1.0.0).
  #'   Use \code{NULL} to not include package versions in your requirements file.
  #'
  #' @return Character vector of versioned package names.
  if (nrow(matched_packages_df) == 0) return(character(0))
  if (is.null(eq_sym)) return(sort(trimws(matched_packages_df$name)))

  eq_sym = validate_eq_sym(eq_sym)

  versioned_packages_df = matched_packages_df[!is.na(matched_packages_df[["version"]]), ]
  unversioned_requirements = matched_packages_df[is.na(matched_packages_df[["version"]]), "name"]

  if (nrow(versioned_packages_df) == 0) return(unversioned_requirements)

  versioned_requirements = mapply(paste0,
                                  trimws(versioned_packages_df[["name"]]),
                                  eq_sym,
                                  trimws(versioned_packages_df[["version"]]),
                                  USE.NAMES = FALSE)

  sort(c(versioned_requirements, unversioned_requirements))
}


rm_dup_matched_packages = function(matched_packages_df) {
  #' @keywords internal
  #' Remove duplicated packages from a matched_packages_df
  #'
  #' @details Duplicated packages occur when detecting packages from R files and packrat.lock file.
  #' If duplicated packages with conflicting version numbers are found then the older version numbers will be dropped.
  #' A warning will be displayed if a conflicting version number scenario occurs.
  #'
  #' @param matched_packages_df data.frame with columns c("name", "version")
  #'
  #' @return A data.frame of same structure as input `matched_packages_df` but with duplicates dropped
  #'
  #' @examples
  #' matched_packages_df = data.frame(name = c("testthat", "testthat", "DT", "DT", "mgsub"),
  #'                                  version = c("0.0.0", "1.0.0", "42.0.0", "3.0.0", "1.0.0"),
  #'                                  stringsAsFactors = FALSE)
  #' (matched_packages_df = rm_dup_matched_packages(matched_packages_df))
  #' #       name version
  #' # 1       DT  42.0.0
  #' # 2    mgsub   1.0.0
  #' # 3 testthat   1.0.0
  matched_packages_df = unique(matched_packages_df)

  # Exit early if no dups found
  if (!any(duplicated(matched_packages_df[["name"]]))) {
    row.names(matched_packages_df) = NULL
    return(matched_packages_df)
  }

  # Sort by version number and drop `duplicated()` names to avoid having to split/apply/recombine
  pkg_version_order = order_package_versions(matched_packages_df[["version"]])
  sorted_packages_df = matched_packages_df[pkg_version_order, ]

  dups = duplicated(sorted_packages_df[["name"]])
  deduped_packages_df = sorted_packages_df[!dups, ]

  # Warn about packages involved conflict resolution
  dup_package_names = sort(unique(sorted_packages_df[["name"]][dups]))
  warn_msg =
    "packrat.lock version and installed version conlficted for the following packages: %s\n"
  warning(sprintf(warn_msg, paste0(dup_package_names, collapse = ", ")),
          "The most recent version(s) of these packages will be used.")

  deduped_packages_df = deduped_packages_df[order(deduped_packages_df[["name"]]), ]
  row.names(deduped_packages_df) = NULL
  return(deduped_packages_df)
}


write_requirements_file = function(package_requirements, file_path = "requirements.txt", append = FALSE) {
  #' @keywords internal
  #' Helper for writing requirements to file
  #'
  #' Wrapper of writeLines with default file_path
  #'
  #' @param package_requirements Character vector of requirements to write to file.
  #' @param file_path Path to write requirements to.
  #' @param append Should \code{package_requirements} be appended to end of \code{file_path}?
  #'               Overwrites \code{file_path} if \code{FALSE}.
  if (length(package_requirements) == 0) message("No dependencies found. Writing a blank requirements file.")

  # Only add comments about auto-generation if creating a new file
  if (!file.exists(file_path) | !append) {
    package_requirements = c(AUTO_GEN_COMMENTS, package_requirements)
  }

  write(x = package_requirements, file = file_path, append = append)
}
