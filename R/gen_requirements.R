#' Utility functions for automatically generating a requirements.txt file

# quote from: https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Creating-R-packages
# > The mandatory ‘Package’ field gives the name of the package.
# > This should contain only (ASCII) letters, numbers and dot,
# > have at least two characters and start with a letter and not end in a dot.
CANONICAL_PACKAGE_NAME_RE = '[a-zA-Z]{1}[a-zA-Z0-9\\.]*[a-zA-Z0-9]{1,}'
LIB_RE   = sprintf('library\\((%s)\\)', CANONICAL_PACKAGE_NAME_RE)
REQ_RE   = sprintf('require\\((%s)\\)', CANONICAL_PACKAGE_NAME_RE)
PAC_RE   = sprintf('p_load\\((%s)\\)',  CANONICAL_PACKAGE_NAME_RE)
COLON_RE = sprintf('(%s)::',            CANONICAL_PACKAGE_NAME_RE)

PACKAGE_RES = c(LIB_RE, REQ_RE, PAC_RE, COLON_RE)
usethis::use_data(PACKAGE_RES, internal = TRUE, overwrite = TRUE)


read_package_lines_from_file = function(file_path, filter_words=c('::', 'library', 'require', 'p_load')) {
  all_file_lines = readLines(file_path)

  package_lines_re = paste(filter_words, collapse = '|')
  package_lines = all_file_lines[grep(package_lines_re, all_file_lines)]

  return(unique(package_lines))
}


read_package_lines_from_files = function(file_paths, filter_words=c('::', 'library', 'require', 'p_load')) {
  package_lines_list = lapply(file_paths, read_package_lines_from_file)
  uniq_package_lines = unique(unlist(package_lines_list))

  return(uniq_package_lines)
}


match_package = function(candidate_lines, package_regex) {
  lib_matches = stringr::str_match(candidate_lines, package_regex)[,2]
  lib_matches = lib_matches[!is.na(lib_matches)]

  return(lib_matches)
}


match_packages = function(candidate_lines, package_regexes) {
  match_list = lapply(package_regexes, function(r) match_package(candidate_lines, r))
  uniq_matches = unique(unlist(match_list))

  return(uniq_matches)
}


safe_package_version = function(pkg) {
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
  # TODO: implement this
  return(eq_sym)
}


append_version_requirement = function(pkg, eq_sym='>=', rm_missing=FALSE) {
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
  versioned_packages = vapply(package_names, function(p) {
    append_version_requirement(p, eq_sym, rm_missing)
  }, character(1), USE.NAMES = FALSE)
  
  return(versioned_packages[!is.na(versioned_packages)])
}


write_requirements_file = function(package_requirements, file_path='requirements.txt') {
  writeLines(package_requirements, file_path)
}


generate_requirements = function(glob_paths='*.R', output_path='requirements.txt', eq_sym='>=', rm_missing=FALSE) {
  file_paths = Sys.glob(glob_paths)
  package_text_lines = read_package_lines_from_files(file_paths)

  matched_packages = match_packages(package_text_lines, PACKAGE_RES)
  sorted_matched_packages = sort(matched_packages)

  versioned_matched_packages = append_version_requirements(sorted_matched_packages, eq_sym)

  write_requirements_file(versioned_matched_packages)
}


generate_requirements('exploration/*.R',
                      eq_sym = NULL,
                      rm_missing = FALSE)
