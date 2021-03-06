activate_packrat = function() {
  tryCatch(packrat::status(),
           error = function(x) {
             packrat::init()
             packrat::on()
           })

  return(NULL)
}


legal_r_package_name = function(name) {
  #' @param name package name to be checked for validity
  #' @return logical vector of names being legal r package names

  return(grepl(CANONICAL_PACKAGE_NAME_RE_STANDALONE, name))
}


equivalent_version = function(existing, target) {
  #' @keywords internal
  #' Check if wildcarded target version is equivalent to existing installed version
  #'
  #' @param existing The semantic version number installed
  #' @param target The semantic version number targeted by requirements file
  #'
  #' @return TRUE if versions are equivalent; otherwise FALSE
  #'
  #' @examples
  #' equivalent_version("1.0.1", "1.*")
  #' # [1] TRUE
  #' equivalent_version("1.9.1", "1.*")
  #' # [1] TRUE
  #' equivalent_version("2.0.0", "1.*")
  #' # [1] FALSE
  #'
  # Convert wildcarded versions to regex
  re_target = glob2rx(target)

  grepl(re_target, existing)
}


compare_compatible_version = function(existing, target) {
  #' @keywords internal
  #' Check if existing installed version ~= target version
  #'
  #' @param existing The semantic version number installed
  #' @param target The semantic version number targeted by requirements file
  #'
  #' @return TRUE if versions are compatible; otherwise FALSE
  #'
  #' @examples
  #' compare_compatible_version("1.1.1", "1.*")
  #' # [1] TRUE
  #' compare_compatible_version("1.1.1", "1.0")
  #' # [1] TRUE
  #' compare_compatible_version("1.1.1", "2.0")
  #' # [1] FALSE

  # Replace trailing version specifier with wildcard (i.e. 2.2 -> 2.*)
  compatible_version_wildcard = sub("(.*\\.)(.+)", "\\1*", target)

  # Per PEP 440 `~= 2.2` is equivalent to `>= 2.2 & == 2.*`
  compare_version(existing, compatible_version_wildcard, COMP_EXACTLY_EQUAL) &
    compare_version(existing, target, COMP_GTE)
}


compare_version = function(existing, target, comp) {
  #' @keywords internal
  #' Check if existing requirement matches user specified target version
  #'
  #' @param existing The semantic version number installed
  #' @param target The semantic version number targeted by requirements file
  #' @param comp The version comparison operator
  #'
  #' @return logical(1) indicating if comparison is TRUE
  #'
  #' @examples
  #' compare_version("1.0.1", "1.*", COMP_EXACTLY_EQUAL)
  #' [1] TRUE
  #' compare_version("1.0.1", "1.*", COMP_COMPATIBLE)
  #' [1] TRUE
  #' compare_version("1.0.1", "1.*", COMP_NOT_EQUAL)
  #' [1] FALSE

  # Push compatible version check to dedicated helper
  if (comp == COMP_COMPATIBLE) return(compare_compatible_version(existing, target))

  # Check if existing == target would satisfy comparison
  is_equality_check = comp %in% COMP_EQUALITY

  # Check if `target` is equivalent to `existing` wrt wildcards
  is_equivalent_version = equivalent_version(existing, target)

  if (is_equality_check & is_equivalent_version) return(TRUE)
  if (comp == COMP_NOT_EQUAL & is_equivalent_version) return(FALSE)

  # TODO: definitively prove this is a valid substitution
  non_wild_card_target = gsub("*", "0", target, fixed = TRUE)

  existing_version = package_version(existing)
  target_version = package_version(non_wild_card_target)

  get(comp)(existing_version, target_version)
}


get_installed = function(dummy=NULL) {
  #' @param dummy dummy data to return for testing purposes
  #' @return named vector of package versions
  if (!is.null(dummy)) {
    return(dummy)
  } else {
    return(installed.packages()[, 3])
  }
}


get_available_versions = function(package) {
  #' @param package name of package to fetch versions for
  #' @return character vector of available package versions; NULL if package not found
  #'
  #' @examples
  #' get_available_versions("mgsub")
  #' # [1] "1.0.0" "1.5.0" "1.7.0" "1.7.1"
  #'
  #' get_available_versions("mgsub_fakerino")
  #' # NULL
  crandb_url = paste0("http://crandb.r-pkg.org/", package, "/all")  # nolint
  response = httr::GET(crandb_url)

  if (httr::status_code(response) == 404) return(NULL)

  response_content = httr::content(response)
  version_timeline = response_content$timeline

  return(names(version_timeline))
}


vmess = function(x, v) {
  #' @param x what message should be printed?
  #' @param v should the message actually be printed?
  #' @return nothing important
  #' @details To help with verbose message printing

  if (v) {
    message(x)
  }

  invisible(return(NULL))
}
