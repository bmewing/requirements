read_requirements_file = function(req) {
  #' @param req path to requirements file
  #' @return dataframe of requirements file contents plus line numbers and source file reference

  if (!file.exists(req)) stop(sprintf(REQ_FILE_EXIST_ERR, req))
  content = readLines(req)
  if (length(content) == 0) stop(sprintf(REQ_FILE_EMPTY_ERR, req))
  output = data.frame(content = content,
                      line = seq_along(content),
                      file = req,
                      stringsAsFactors = FALSE)

  additional_files = grep("^ *\\-r", content, value = TRUE)
  if (length(additional_files) > 0) {
    for (af in additional_files) {
      tmp = read_requirements_file(sub("^ *\\-r *", "", af))
      output = rbind(output, tmp)
    }
  }

  return(output)
}

identify_duplicate_reqs = function(content_df) {
  #' @param content_df dataframe of requirements with three columns
  #' @return content_df untouched
  #' @details content_df$content is the requirement line
  #'          content_df$line is the line number
  #'          content_df$file is the file the content originated from
  #'  If there are any duplicated requirements (other than exact duplicates, e.g. mgsub < 1 and mgsub > 0)
  #'  it throws a verbose error.

  content_dup = content_df[grepl(CANONICAL_PACKAGE_NAME_RE_EXTRACT, content_df$content), ]
  content_dup$package = gsub(CANONICAL_PACKAGE_NAME_RE_EXTRACT, "\\1", content_dup$content)

  dups = duplicated(content_dup$package)
  if (!any(dups)) {
    return(content_df)
  } else {
    error = ""
    for (i in which(dups)) {
      first_occur = which(content_dup$package == content_dup[2, "package"])[1]
      error = paste0(error,
                     sprintf(REQ_FILE_DUPLICATE_REQ,
                             content_dup$content[i],
                             content_dup$file[i],
                             content_dup$line[i],
                             content_dup$content[first_occur],
                             content_dup$file[first_occur],
                             content_dup$line[first_occur],
                             content_dup$package[i]))
    }
    stop(error)
  }
}


strip_comments = function(content, remove_additional_file = TRUE) {
  #' @param content vector of characters
  #' @param remove_additional_file should lines starting with -r be removed?
  #' @return vector of chracters without comments or -r and trimmed whitespace
  #' @details This was originally part of another function but was split out to be used
  #' in a few different places.
  content = gsub("#.*", "", content)
  if (remove_additional_file) {
    content = gsub("^ *\\-r.*", "", content)
  }
  content = trimws(content, which = "both")
  return(content)
}


remove_comments_dups_from_req = function(content_df) {
  #' @param content_df dataframe of requirements with three columns
  #' @return vector of requirements to be processed
  #' @details content_df$content is the requirement line
  #'          content_df$line is the line number
  #'          content_df$file is the file the content originated from
  #' Removes all full line comments and references to additional requirements files
  #' Trims whitespace from lines and removes in-line comments
  #' Checks for duplicated requirements
  content_df$content = strip_comments(content_df$content)
  content_df = content_df[nchar(content_df$content) > 0, ]
  #remove identical duplicates
  content_df = content_df[!duplicated(content_df$content), ]
  content_df = identify_duplicate_reqs(content_df)
  return(content_df$content)
}

capture_special_installs = function(content) {
  #' @param content vector of requirements to be processed
  #' @return list of all special installs needed

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

validate_versioning = function(req) {
  #' @param req a requirement to be checked
  #' @return logical indicating if the versioned requirement is valid (including valid comparison operator)

  return(grepl(VALID_REQ, req))
}

capture_versioned_requirements = function(content) {
  #' @param content vector of requirements
  #' @return vector of requirements which include version numbers and valid comparison operators
  #' @detail Part of the larger processing of the requirements file, this works on a subset of
  #' all requirements to identify those that need to be checked for viability and have special
  #' versions installed.
  versioned = vapply(content, identify_comparison_op, character(1))
  versioned = versioned[!is.na(versioned)]
  if (!all(versioned %in% COMPS)) {
    first_error = which(!versioned %in% COMPS)[1]
    illegal_comp = versioned[first_error]
    stop(sprintf(REQ_FILE_INVALID_COMP,
                 content[first_error],
                 illegal_comp,
                 agrep(illegal_comp, COMPS, value = TRUE)[1]))
  }
  version_req = names(versioned[!is.na(versioned)])
  processed = lapply(version_req, process_versioned_requirement)
  valid_packages = vapply(processed,
                          function(x){legal_r_package_name(trimws(x$package))}, # nolint
                          FUN.VALUE = logical(1))
  return(version_req[valid_packages])
}

identify_comparison_op = function(req) {
  #' @param req a single versioned requirement string
  #' @return the comparison operator used
  #' @detail if there is not a valid comparison operator it returns NA
  comp = gsub(pattern = COMP_EXTRACTOR,
              replacement = "\\1",
              x = req)
  if (comp == req) {
    comp = NA_character_
  }
  return(comp)
}

capture_local_requirements = function(content) {
  #' @param content a vector of requirements
  #' @return vector of local file requirements
  #' @detail while there may be more legal file types, it checks to see if the requirement
  #' looks like a local file. Because it is part of the larger processing of the requirements
  #' file, this step occurs after URLs are removed and so doesn't need to worry about that.
  local_source_req = grep("\\.tar\\.gz$", content, value = TRUE)
  local_win_req = grep("\\.zip$", content, value = TRUE)
  local_mac_req = grep("\\.tgz$", content, value = TRUE)
  local_req = c(local_source_req, local_win_req, local_mac_req)
  local_req = local_req[file.exists(local_req)]
  return(local_req)
}

process_requirements_file = function(req) {
  #' @param req path to requirements file
  #' @return list with all supported requirement types

  content = read_requirements_file(req)
  content = remove_comments_dups_from_req(content)
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
