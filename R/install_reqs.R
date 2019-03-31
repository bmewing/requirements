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