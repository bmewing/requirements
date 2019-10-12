#' Generate a requirements.txt for your project
#'
#' @description Generate a requirements file from existing file(s) and installed packages
#'
#' @param output_path String indicating where to write resulting requirements file to.
#' @param eq_sym The equality symbol to be used when writing requirements (i.e. package>=1.0.0).
#'   Use \code{NULL} to not include package versions in your requirements file.
#' @param inspect_files Character vector of file extensions to search for dependencies in.
#'                      Officially supports the following extensions: `c("R", "Rmd", "Rpres", "lock")`.
#'                      Other extensions will be processed as if they were `.R` files.
#' @param path Location where extensions from `inspect_files` should be searched for.
#'             (see `path` from `?list.files`).
#' @param recursive Should the seatch for `inspect_files` recurse into directories?
#'                  (see `recursive` from `?list.files`).
#' @param rm_missing Should packages not installed locally be excluded from output?
#'
#' @return Nothing is returned.  Results are written to \code{output_path}
#'
#' @details
#' If `"lock"` is included in `inspect_files`, it is assumed to refer to a `packrat.lock`.  Only files matching the
#' pattern `glob2rx("*packrat/packrat.lock")` will be considered.
#'
#' Methodology for package matching relies on string pattern matching rather than a more sophisticated method.
#' This will match most 'standard' ways of referencing libraries in R code.
#' The following lines of code show examples where the \code{data.table} package will be matched properly:
#' \itemize{
#'   \item \code{library(data.table)}
#'   \item \code{library("data.table")}
#'   \item \code{library(data.table, warn.conflicts = TRUE)}
#'   \item \code{require(data.table)}
#'   \item \code{require("data.table")}
#'   \item \code{require(data.table, quietly = TRUE)}
#'   \item \code{pacman::p_load(data.table)}
#'   \item \code{pacman::p_load("data.table")}
#'   \item \code{pacman::p_load(data.table, install = FALSE)}
#'   \item \code{data.table::data.table()}
#'   \item \code{data.table:::data.table()}
#' }
#'
#' Matching will fail if:
#'
#' \itemize{
#'   \item loading multiple packages with \code{pacman::p_load} (only the first package will be matched)
#'   \item using a character vector to load; e.g. \code{pkg = "testthat"; library(pkg, character.only = TRUE)}
#'     (will match \code{pkg} as package instead of \code{testthat})
#'   \item using a string resembling a package load; e.g. \code{"library(fake.package)"}
#'     (will match \code{fake.package})
#' }
#'
#'
#' @examples
#'
#' \dontrun{
#' rip_freeze("R/*.R")
#' rip_freeze("R/*.R", output_path = "my_requirements.txt")
#' rip_freeze("R/*.R", output_path = "equal_to_requirements.txt", eq_sym = "==")
#' rip_freeze("R/*.R", output_path = "versionless_requirements.txt", eq_sym = NULL)
#' rip_freeze("R/*.R", output_path = "installed_requirements.txt", rm_missing = TRUE)
#' }
#' @export
rip_freeze = function(output_path = "requirements.txt",
                      eq_sym = ">=",
                      inspect_files = c("R", "Rmd", "Rpres", "lock"),
                      path = ".",
                      recursive = TRUE,
                      rm_missing = FALSE) {

  file_ext_patterns = paste0("\\.", gsub("^\\.", "", inspect_files), "$")
  file_ext_pattern = paste(file_ext_patterns, collapse = "|")
  file_paths = list.files(path = path,
                          pattern = file_ext_pattern,
                          full.names = TRUE,
                          recursive = recursive,
                          ignore.case = TRUE)

  packrat_lock_path = grep(glob2rx("*packrat/packrat.lock"),
                           file_paths,
                           ignore.case = TRUE,
                           value = TRUE)

  file_paths = setdiff(file_paths, packrat_lock_path)

  if (length(packrat_lock_path) > 0) {
    matched_packages_df_list = lapply(packrat_lock_path,
                                      read_reqs_from_lockfile,
                                      eq_sym = eq_sym)

    packrat_matched_packages_df = do.call(rbind, matched_packages_df_list)
  } else {
    packrat_matched_packages_df = data.frame()
  }

  package_text_lines = read_package_lines_from_files(file_paths)

  matched_packages = match_packages(package_text_lines, PACKAGE_RES)
  matched_packages_df = data.frame(name = matched_packages,
                                   version = rep(NA_character_, length(matched_packages)),
                                   stringsAsFactors = FALSE)

  if (!is.null(eq_sym)) {
    matched_packages_df[["version"]] = vapply(matched_packages_df[["name"]], safe_packageVersion, character(1))
  }

  matched_packages_df = rbind(matched_packages_df, packrat_matched_packages_df)

  if (rm_missing) {
    matched_packages_df = matched_packages_df[!is.na(matched_packages_df[["version"]]), ]
  }

  matched_packages_df = rm_dup_matched_packages(matched_packages_df)

  requirements_vector = append_version_requirements(matched_packages_df, eq_sym)

  write_requirements_file(requirements_vector, output_path)
}


#' @rdname rip_freeze
#' @export
generate_requirements = function(output_path = "requirements.txt",
                                 eq_sym = ">=",
                                 inspect_files = c("R", "Rmd", "Rpres", "lock"),
                                 path = ".",
                                 recursive = TRUE,
                                 rm_missing = FALSE) {
  rip_freeze(output_path = output_path,
             eq_sym = eq_sym,
             inspect_files = inspect_files,
             path = path,
             recursive = recursive,
             rm_missing = rm_missing)
}
