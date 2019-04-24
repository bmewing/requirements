packrat_to_requirements = function(packrat_lock_path = "packrat/packrat.lock",
                                   output_path = "requirements.txt",
                                   eq_sym = ">=",
                                   append = TRUE) {
  #' Generate a requirements file from a \code{packrat.lock} file
  #'
  #' @param packrat_lock_path Path to \code{packrat.lock} to extract requirements from.
  #' @param output_path String indicating where to write resulting requirements file to.
  #' @param eq_sym The equality symbol to be used when writing requirements (i.e. package>=1.0.0).
  #'   Use \code{NULL} to not include package versions in your requirements file.
  #' @param append Should requirements from \code{packrat_lock_path} be appended to end of \code{output_path}?
  #'               Overwrites contents of \code{output_path} if \code{FALSE}.
  #'
  #' @return Nothing is returned.  Results are written to \code{output_path}
  #'
  #' @examples
  #'
  #' \dontrun{
  #' packrat_to_requirements(output_path = "my_requirements.txt")
  #' packrat_to_requirements(output_path = "versionless_requirements.txt", eq_sym = NULL)
  #' packrat_to_requirements(output_path = "overwrite_requirements.txt", append = FALSE)
  #' }
  #' @export
  matched_packages_df = read_reqs_from_lockfile(packrat_lock_path, eq_sym)
  matched_packages_df = rm_dup_matched_packages(matched_packages_df)

  requirements_vector = unique(append_version_requirements(matched_packages_df, eq_sym))

  write_requirements_file(requirements_vector, output_path, append)
}
