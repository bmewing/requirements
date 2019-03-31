activate_packrat = function(){
  tryCatch(packrat::status(),
           error=function(x){packrat::init();packrat::on()})
  return(NULL)
}

legal_r_package_name = function(name){
  #' @param name package name to be checked for validity
  #' @return logical vector of names being legal r package names

  return(grepl(CANONICAL_PACKAGE_NAME_RE_STANDALONE,name))
}


equivalent_version = function(existing, target) {
  #' Check if wildcarded target version is equivalent to existing installed version
  #'
  #' @param existing The semantic version number installed
  #' @param target The semantic version number targeted by requirements file
  #'
  #' @return TRUE if versions are equivalent; otherwise FALSE
  #'
  #' @examples
  #' equivalent_version('1.0.1', '1.*')
  #' # [1] TRUE
  #' equivalent_version('1.9.1', '1.*')
  #' # [1] TRUE
  #' equivalent_version('2.0.0', '1.*')
  #' # [1] FALSE
  #'
  # Convert wildcarded versions to regex
  re_target = glob2rx(target)

  grepl(re_target, existing)
}


compare_compatible_version = function(existing, target) {
  #' Check if existing installed version ~= target version
  #'
  #' @param existing The semantic version number installed
  #' @param target The semantic version number targeted by requirements file
  #'
  #' @return TRUE if versions are compatible; otherwise FALSE
  #'
  #' @examples
  #' compare_compatible_version('1.1.1', '1.*')
  #' # [1] TRUE
  #' compare_compatible_version('1.1.1', '1.0')
  #' # [1] TRUE
  #' compare_compatible_version('1.1.1', '2.0')
  #' # [1] FALSE

  # Replace trailing version specifier with wildcard (i.e. 2.2 -> 2.*)
  compatible_version_wildcard = sub('(.*\\.)(.+)', '\\1*', target)

  # Per PEP 440 `~= 2.2` is equivalent to `>= 2.2 & == 2.*`
  compare_version(existing, compatible_version_wildcard, '==') &
    compare_version(existing, target, '>=')
}


compare_version = function(existing, target, comp) {
  #' Check if existing requirement matches user specified target version
  #'
  #' @param existing The semantic version number installed
  #' @param target The semantic version number targeted by requirements file
  #' @param comp The version comparison operator
  #'
  #' @return logical(1) indicating if comparison is TRUE
  #'
  #' @examples
  #' compare_version('1.0.1', '1.*', '==')
  #' [1] TRUE
  #' compare_version('1.0.1', '1.*', '~=')
  #' [1] TRUE
  #' compare_version('1.0.1', '1.*', '!=')
  #' [1] FALSE

  # Push compatible version check to dedicated helper
  if (comp == "~=") return(compare_compatible_version(existing, target))

  # Check if existing == target would satisfy comparison
  is_equality_check = comp %in% c('>=', '<=', '~=', '==')

  # Check if `target` is equivalent to `existing` wrt wildcards
  is_equivalent_version = equivalent_version(existing, target)

  if (is_equality_check & is_equivalent_version) return(TRUE)
  if (comp == "!=" & is_equivalent_version) return(FALSE)

  # TODO: definitively prove this is a valid substitution
  non_wild_card_target = gsub('*', '0', target, fixed = TRUE)

  existing_version = package_version(existing)
  target_version = package_version(non_wild_card_target)

  get(comp)(existing_version, target_version)
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
        install_needed = !compare_version(installed[i], version, comp)
      }
      if(install_needed){
        if(!is.na(installed[i])) v = vmess(sprintf(BAD_VERSION,package,installed[i]),verbose)

        available_versions = get_available_versions(package)

        available_compatibility = vapply(available_versions, function(available_version) {
          compare_version(available_version, version, comp)
        }, logical(1))

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
