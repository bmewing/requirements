read_requirements_file = function(req){
  #' @param req path to requirements file
  #' @return vector of requirements to be processed

  EXIST_ERR = 'The specified requirements file, %s, does not exist'
  EMPTY_ERR = 'The requirements file %s is empty.'
  if(!file.exists(req)) stop(sprintf(EXIST_ERR, req))

  content = readLines(req)
  if(length(content) == 0) stop(sprintf(EMPTY_ERR, req))

  additional_files = grep('^ *\\-r', content, value=TRUE)
  if(length(additional_files) > 0){
    for(af in additional_files){
      tmp = read_requirements_file(sub('^ *\\-r *','',af))
      content = c(content, tmp)
    }
  }
  return(content)
}

activate_packrat = function(){
  tryCatch(packrat::status(),
           error=function(x){packrat::init();packrat::on()})
  return(NULL)
}

legal_r_package_name = function(name){
  #' @param name package name to be checked for validity
  #' @return logical vector of names being legal r package names

  return(grepl(CANONICAL_PACKAGE_NAME_RE,name))
}

process_requirements_file = function(req){
  #' @param req path to requirements file
  #' @return list with all supported requirement types

  content = read_requirements_file(req)

  # remove comments, additional files
  content = content[!grepl('^ *#',content)]
  content = content[!grepl('^ *\\-r',content)]
  # special installs
  git_req = grep('^git[\\+:]',content,value=TRUE)
  svn_req = grep('^svn\\+',content,value=TRUE)
  bioc_req = grep('^bioc\\+',content,value=TRUE)
  url_req = grep('^https?:',content,value=TRUE)
  content = content[!content %in% c(git_req,svn_req,bioc_req,url_req)]
  # version specific
  version_req = grep(paste(COMPS,collapse='|'),content,value=TRUE)
  tmp = strsplit(version_req,paste(COMPS,collapse='|'))
  pkg_name = vapply(tmp,`[[`,"pkg",1)
  version_req = version_req[legal_r_package_name(pkg_name)]
  content = content[!content %in% version_req]
  # local file can only be .tar.gz, .tgz, or .zip
  local_source_req = grep('\\.tar\\.gz$',content,value=TRUE)
  local_win_req = grep('\\.zip$',content,value=TRUE)
  local_mac_req = grep('\\.tgz$',content,value=TRUE)
  local_req = c(local_source_req,local_win_req,local_mac_req)
  local_req = local_req[file.exists(local_req)]
  content = content[!content %in% local_req]
  # remaining packages
  pkg_req = content[legal_r_package_name(content)]
  content = content[!content %in% pkg_req]

  if(length(content) > 0){
    RESOLUTION_ERR = 'Not all requirements are allowed: %s'
    stop(sprintf(RESOLUTION_ERR,paste(content,collapse=', ')))
  }

  output = list(git = git_req,
                svn = svn_req,
                bioc = bioc_req,
                url = url_req,
                local = local_req,
                versioned = version_req,
                unversioned = pkg_req)
  return(output)
}

compare_version = function(existing, target, comp){
  #' @param target The semantic version number targeted
  #' @param existing The semantic version number installed
  #' @param comp The version comparison operator
  #' @return logical indicating if semantic version matches requirements

  if(existing == '*' | target == '*'){
    if(comp == "!=") return(FALSE) else return(TRUE)
  }
  if(comp == "~=") comp = "=="
  return(get(comp)(existing, target))
}

check_version = function(target,existing,comp){
  #' @param target The version targeted for installation
  #' @param existing The currently installed version
  #' @param comp The version comparison operator
  #' @return logical indicating if existing version matches requirements

  target = strsplit(target,"[^0-9a-zA-Z\\*]")[[1]]
  existing = strsplit(existing,"[^0-9a-zA-Z\\*]")[[1]]
  fill = '0'
  if(target[length(target)] == '*' | comp == '~=') fill = '*'
  if(length(target) > length(existing)){
    existing = c(existing,rep(fill,length(target) - length(existing)))
  } else {
    target = c(target,rep(fill,length(existing) - length(target)))
  }
  to_compare = cbind(existing,target,comp)
  results = mapply(compare_version,to_compare[,1],to_compare[,2],to_compare[,3])
  if(comp == '!=') return(any(results)) else return(all(results))
}

get_installed = function(dummy=NULL){
  #' @param dummy dummy data to return for testing purposes
  #' @return named vector of package versions
  if(!is.null(dummy)){
    return(dummy)
  } else {
    return(installed.packages()[,3])
  }
}

install_reqs = function(reqs, dryrun, verbose = dryrun,
                        repo = options()$repo[1], ...){
  #' @param reqs results of process_requirements_file
  #' @param dryrun if TRUE, no packages will be installed, but you can see what would have happened
  #' @param verbose should the function be explicit about activities
  #' @param repo what CRAN repository should be used?
  #' @param ... values to pass to get_installed
  #' @return data.frame of installed packages and their versions

  INSTALL = 'Installing %s %s'
  INSTALL_VERSION = 'at version %s'
  NOT_INSTALLED = 'NOTE: %s is not currently installed.'
  BAD_VERSION = 'NOTE: %s is current installed at version %s which is not sufficient.'
  NONE_EXISTS = 'ERROR: No version exists for %s which meets requirements.'
  OTHER_FAIL = 'ERROR: Requirement %s could not be satisified for some reason.'

  failures = 0

  # Install git
  if(length(reqs$git) > 0){
    for(i in reqs$git){
      url = sub('^git\\+','',i)
      v = vmess(sprintf(INSTALL,url,''),verbose)
      if(!dryrun){
        tryCatch(devtools::install_git(url),
                 error=function(x){
                   failures <<- failures + 1
                   message(sprintf(OTHER_FAIL,url))
                 })
      }
    }
  }

  if(length(reqs$svn) > 0){
    for(i in reqs$svn){
      url = sub('^svn\\+','',i)
      v = vmess(sprintf(INSTALL,url,''),verbose)
      if(!dryrun){
        tryCatch(devtools::install_svn(url),
                 error=function(x){
                   failures <<- failures + 1
                   message(sprintf(OTHER_FAIL,url))
                 })
      }
    }
  }

  if(length(reqs$bioc) > 0){
    for(i in reqs$bioc){
      url = sub('^bioc\\+','',i)
      v = vmess(sprintf(INSTALL,url,''),verbose)
      if(!dryrun){
        tryCatch(devtools::install_bioc(url),
                 error=function(x){
                   failures <<- failures + 1
                   message(sprintf(OTHER_FAIL,url))
                 })
      }
    }
  }

  if(length(reqs$url) > 0){
    for(i in reqs$url){
      v = vmess(sprintf(INSTALL,i,''),verbose)
      if(!dryrun){
        tryCatch(devtools::install_url(i),
                 error=function(x){
                   failures <<- failures + 1
                   message(sprintf(OTHER_FAIL,i))
                 })
      }
    }
  }

  if(length(reqs$local) > 0){
    for(i in reqs$local){
      type = 'source'
      if(!grepl('\\.tar\\.gz$',i)){
        type = 'binary'
      }
      v = vmess(sprintf(INSTALL,i,''),verbose)
      if(!dryrun){
        tryCatch(install.packages(i,repos=NULL,type=type),
                 error=function(x){
                   failures <<- failures + 1
                   message(sprintf(OTHER_FAIL,i))
                 })
      }
    }
  }

  installed = get_installed(...)

  if(length(reqs$unversioned) > 0){
    for(i in reqs$unversioned){
      if(is.na(installed[i])){
        available_versions = get_available_versions(i)
        v = vmess(sprintf(NOT_INSTALLED,i),verbose)
        version = tail(sort(available_versions), 1)
        v = vmess(sprintf(INSTALL,i,sprintf(INSTALL_VERSION,version)),verbose)
        if(!dryrun){
          tryCatch(install.packages(i,repos=repo),
                   error=function(x){
                     failures <<- failures + 1
                     message(sprintf(OTHER_FAIL,i))
                   })
        }
      }

    }
  }

  if(length(reqs$versioned) > 0){
    for(i in reqs$versioned){
      install_needed = FALSE
      comp = sub(paste0('^.*?(',paste(COMPS,collapse='|'),').*$'),'\\1',i)
      if(comp == "=") comp = "=="
      split = strsplit(i,paste(COMPS,collapse='|'))[[1]]
      package = gsub(' *','',split[1])
      version = gsub(' *','',split[2])
      if(is.na(installed[i])){
        v = vmess(sprintf(NOT_INSTALLED,i),verbose)
        install_needed = TRUE
      } else {
        install_needed = !check_version(version,installed[i],comp)
      }
      if(install_needed){
        if(!is.na(installed[i])) v = vmess(sprintf(BAD_VERSION,package,installed[i]),verbose)

        available_versions = get_available_versions(package)
        available_compatibility = vapply(available_versions,
                                         check_version,
                                         TRUE,
                                         target = version,
                                         comp = comp)
        if(!any(available_compatibility)){
          failures = failures + 1
          v = vmess(sprintf(NONE_EXISTS,package),verbose)
        } else {
          latest_compatible = names(which.max(which(available_compatibility)))
          v = vmess(sprintf(INSTALL,package,
                            sprintf(INSTALL_VERSION,latest_compatible)),verbose)
          if(!dryrun){
            tryCatch(devtools::install_version(package,
                                               version=latest_compatible,
                                               repos=repo,
                                               quiet=TRUE),
                     error=function(x){
                       failures <<- failures + 1
                       message(sprintf(OTHER_FAIL,i))
                     })
          }
        }
      }
    }
  }

  if(failures > 0) stop(paste0('There were ',failures,' package(s) which could not be installed.'))
  return(0)
}

#TODO make sure this works in the new context
load_packages = function(packages,verbose,dryrun){
  if(verbose) cat("Loading",packages[1],"version",packages[3],"\n")
  if(!dryrun) library(packages[1],character.only=TRUE)
  return(0)
}


get_available_versions = function(package) {
  #' @param package name of package to fetch versions for
  #' @return character vector of available package versions; NULL if package not found
  #'
  #' @examples
  #' get_available_versions('mgsub')
  #' # [1] "1.0.0" "1.5.0" "1.7.0" "1.7.1"
  #'
  #' get_available_versions('mgsub_fakerino')
  #' # NULL
  crandb_url = paste0("http://crandb.r-pkg.org/", package, "/all")
  response = httr::GET(crandb_url)

  if (httr::status_code(response) == 404) return(NULL)

  response_content = httr::content(response)
  version_timeline = response_content$timeline

  return(names(version_timeline))
}


vmess = function(x,v){
  #' @param x what message should be printed?
  #' @param v should the message actually be printed?
  #' @return nothing important
  #' @details To help with verbose message printing

  if(v){
    message(x)
  }
  invisible(return(NULL))
}
