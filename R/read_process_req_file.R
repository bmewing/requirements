read_requirements_file = function(req){
  #' @param req path to requirements file
  #' @return vector of requirements to be processed

  EXIST_ERR = "The specified requirements file, %s, does not exist"  # nolint
  EMPTY_ERR = "The requirements file %s is empty."  # nolint
  if (!file.exists(req)) stop(sprintf(EXIST_ERR, req))

  content = readLines(req)
  if (length(content) == 0) stop(sprintf(EMPTY_ERR, req))

  additional_files = grep("^ *\\-r", content, value = TRUE)
  if (length(additional_files) > 0) {
    for (af in additional_files) {
      tmp = read_requirements_file(sub("^ *\\-r *", "", af))
      content = c(content, tmp)
    }
  }
  return(content)
}

remove_comments_from_req = function(content) {
  content = content[!grepl("^ *#", content)]
  content = content[!grepl("^ *\\-r", content)]
  return(content)
}

capture_special_installs = function(content) {
  git_req = grep(GIT_EXTRACT, content, value = TRUE)
  svn_req = grep(SVN_EXT_REP, content, value = TRUE)
  bio_req = grep(BIO_EXT_REP, content, value = TRUE)
  url_req = grep(URL_EXTRACT, content, value = TRUE)
  output = list(git = git_req,
                svn = svn_req,
                bioc = bio_req,
                url = url_req)
  return(output)
}

capture_versioned_requirements = function(content) {
  version_req = grep(paste(COMPS, collapse = "|"), content, value = TRUE)
  tmp = strsplit(version_req, paste(COMPS, collapse = "|"))
  pkg_name = vapply(tmp, `[[`, "pkg", 1)
  version_req = version_req[legal_r_package_name(trimws(pkg_name))]
  return(version_req)
}

capture_local_requirements = function(content){
  # local file can only be .tar.gz, .tgz, or .zip
  local_source_req = grep("\\.tar\\.gz$", content, value = TRUE)
  local_win_req = grep("\\.zip$", content, value = TRUE)
  local_mac_req = grep("\\.tgz$", content, value = TRUE)
  local_req = c(local_source_req, local_win_req, local_mac_req)
  local_req = local_req[file.exists(local_req)]
  return(local_req)
}

process_requirements_file = function(req){
  #' @param req path to requirements file
  #' @return list with all supported requirement types

  content = read_requirements_file(req)
  content = remove_comments_from_req(content)

  output = capture_special_installs(content)
  content = content[!content %in% unlist(output)]

  output$versioned = capture_versioned_requirements(content)
  content = content[!content %in% output$versioned]

  output$local = capture_local_requirements(content)
  content = content[!content %in% output$local]
  # remaining packages
  output$unversioned = content[legal_r_package_name(trimws(content))]
  content = content[!content %in% output$unversioned]

  if (length(content) > 0) {
    RESOLUTION_ERR = "Not all requirements are allowed: %s"  # nolint
    stop(sprintf(RESOLUTION_ERR, paste(content, collapse = ", ")))
  }

  return(output)
}
