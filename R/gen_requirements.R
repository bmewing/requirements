#' Generate a requirements.txt for your project
#'
#' @description Generate a requirements file from existing file(s) and installed packages
#'
#' @param glob_paths Character vector of patterns for relative or absolute filepaths. Missing values will be ignored.
#'   See \code{?Sys.glob} for more details.
#' @param output_path String indicating where to write resulting requirements file to.
#' @param eq_sym The equality symbol to be used when writing requirements (i.e. package>=1.0.0).
#'   Use \code{NULL} to not include package versions in your requirements file.
#' @param rm_missing Should packages not installed locally be exlcuded from output?
#'
#' @return Nothing is returned.  Results are written to \code{output_path}
#'
#' @examples
#'
#' \dontrun{
#' generate_requirements('R/*.R')
#' generate_requirements('R/*.R', output_path='my_requirements.txt')
#' generate_requirements('R/*.R', output_path='equal_to_requirements.txt', eq_sym='>=')
#' generate_requirements('R/*.R', output_path='versionless_requirements.txt', eq_sym=NULL)
#' generate_requirements('R/*.R', output_path='installed_requirements.txt', rm_missing=TRUE)
#' }
#' @export
generate_requirements = function(glob_paths='*.R', output_path='requirements.txt', eq_sym='>=', rm_missing=FALSE) {
  file_paths = Sys.glob(glob_paths)
  package_text_lines = read_package_lines_from_files(file_paths)

  matched_packages = match_packages(package_text_lines, PACKAGE_RES)
  sorted_matched_packages = sort(matched_packages)

  versioned_matched_packages = append_version_requirements(sorted_matched_packages, eq_sym, rm_missing)

  write_requirements_file(versioned_matched_packages, output_path)
}
