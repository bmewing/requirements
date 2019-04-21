read_requirements_file = function(req){
  #' @param req path to requirements file
  #' @return vector of requirements to be processed

  if (!file.exists(req)) stop(sprintf(REQ_FILE_EXIST_ERR, req))

  content = readLines(req)
  if (length(content) == 0) stop(sprintf(REQ_FILE_EMPTY_ERR, req))

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

validate_versioning = function(req){
  return(grepl(VALID_REQ, req))
}

capture_versioned_requirements = function(content) {
  versioned = vapply(content, identify_comparison_op, character(1))
  versioned = versioned[!is.na(versioned)]
  if(!all(versioned %in% COMPS)){
    first_error = which(!versioned %in% COMPS)[1]
    illegal_comp = versioned[first_error]
    stop(sprintf(REQ_FILE_INVALID_COMP,
                 content[first_error], 
                 illegal_comp,
                 agrep(illegal_comp, COMPS, value=TRUE)[1]))
  }
  version_req = names(versioned[!is.na(versioned)])
  processed = lapply(version_req, process_versioned_requirement)
  valid_packages = vapply(processed,
                          function(x){legal_r_package_name(trimws(x$package))}, # nolint
                          FUN.VALUE = logical(1))
  return(version_req[valid_packages])
}

identify_comparison_op = function(req){
  comp = gsub(pattern = COMP_EXTRACTOR,
              replacement = "\\1",
              x = req)
  if (comp == req){
    comp = NA_character_
  }
  return(comp)
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
  if (length(content) == 0) stop(sprintf(REQ_FILE_EMPTY_ERR, req))

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
    stop(sprintf(REQ_FILE_RESOLUTION_ERR, paste(content, collapse = ", ")))
  }

  return(output)
}