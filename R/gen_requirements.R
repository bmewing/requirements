#' Generate a requirements.txt for your project
#'
#' @description Generate a requirements file from existing file(s) and installed packages
#'
#' @param glob_paths Character vector of patterns for relative or absolute filepaths. Missing values will be ignored.
#'   See \code{?Sys.glob} for more details.
#' @param output_path String indicating where to write resulting requirements file to.
#' @param eq_sym The equality symbol to be used when writing requirements (i.e. package>=1.0.0).
#'   Use \code{NULL} to not include package versions in your requirements file.
#' @param packrat_lock_path If not \code{NULL}, then requirements will be pulled from
#'   the \code{packrat.lock} at this location and appended to the final \code{requirements.txt} output.
#' @param rm_missing Should packages not installed locally be excluded from output?
#'
#' @return Nothing is returned.  Results are written to \code{output_path}
#'
#' @details Methodology for package matching relies on string pattern matching rather than a more sophisticated method.
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
#' generate_requirements("R/*.R")
#' generate_requirements("R/*.R", output_path = "my_requirements.txt")
#' generate_requirements("R/*.R", output_path = "equal_to_requirements.txt", eq_sym = "==")
#' generate_requirements("R/*.R", output_path = "versionless_requirements.txt", eq_sym = NULL)
#' generate_requirements("R/*.R", output_path = "installed_requirements.txt", rm_missing = TRUE)
#' }
#' @export
generate_requirements = function(glob_paths = "*.R",
                                 output_path = "requirements.txt",
                                 eq_sym = ">=",
                                 packrat_lock_path = NULL,
                                 rm_missing = FALSE) {
  file_paths = Sys.glob(glob_paths)
  package_text_lines = read_package_lines_from_files(file_paths)

  matched_packages = match_packages(package_text_lines, PACKAGE_RES)
  matched_packages_df = data.frame(name = matched_packages,
                                   version = rep(NA_character_, length(matched_packages)),
                                   stringsAsFactors = FALSE)

  if (!is.null(eq_sym)) {
    matched_packages_df[["version"]] = vapply(matched_packages_df[["name"]], safe_package_version, character(1))
  }

  if (!is.null(packrat_lock_path)) {
    matched_packages_df = rbind(
      matched_packages_df,
      read_reqs_from_lockfile(packrat_lock_path, eq_sym)
    )
  }

  if (rm_missing) {
    matched_packages_df = matched_packages_df[!is.na(matched_packages_df[["version"]]), ]
  }

  matched_packages_df = rm_dup_matched_packages(matched_packages_df)

  requirements_vector = append_version_requirements(matched_packages_df, eq_sym)

  write_requirements_file(requirements_vector, output_path)
}


#' @rdname generate_requirements
#' @export
freeze_requirements = function(glob_paths = "*.R",
                               output_path = "requirements.txt",
                               eq_sym = ">=",
                               packrat_lock_path = NULL,
                               rm_missing = FALSE) {
  generate_requirements(glob_paths = glob_paths,
                        output_path = output_path,
                        eq_sym = eq_sym,
                        packrat_lock_path = packrat_lock_path,
                        rm_missing = rm_missing)
}
