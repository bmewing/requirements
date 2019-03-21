compare_version = function(target,existing,comp){
  target = strsplit(target,"[[:punct:]]")[[1]]
  existing = strsplit(existing,"[[:punct:]]")[[1]]
  if(length(target) > length(existing)){
    existing = c(existing,rep("0",length(target) - length(existing)))
  } else {
    target = c(target,rep("0",length(existing) - length(target)))
  }
  to_compare = cbind(existing,target)
  results = mapply(get(comp),to_compare[,1],to_compare[,2])
  if(all(results)) return(TRUE) else return(FALSE)
}

compare_and_install = function(target,existing,dryrun,verbose,repo,install){
  #' @param target Package to be compared/installed
  #' @param existing Named vector of currently installed packages
  #' @param dryrun Should any action actually be taken?
  #' @param verbose Should the function be verbose in what it is doing?
  #' @param repo What repositiory should be used for installation?
  #' @param install In the event of a requirements mismatch, should a correct version be installed?
  name = target[1]
  installed = existing[name]
  comp = target[2]
  version = target[3]
  output = 1
  if(comp == "=") comp = "=="
  if(is.na(installed)){
    if(install){
      if(verbose) cat("NOTE:",name,"is not installed.\nInstalling",name,"version",version,"\n")
      if(!dryrun){
        devtools::install_version(name,version=version,repos=repo,quiet = TRUE)
      }
      output = 0
    }
  }
  
  correct_version = compare_version(version,installed,comp)
  
  if(correct_version){
    if(verbose & dryrun) cat("NOTE:",name,"is already installed with version",version,"\n")
    output = 0
  } else {
    if(grepl(">",comp)){
      newest_good_enough = check_newest_version(name, repo, version, comp)
      if(!newest_good_enough){
        stop(paste0("ERROR: ",name," is not a new enough version. However, no version exists which is suitable."))
      }
      if(install){
        if(verbose) cat("NOTE:",name,"is not a new enough version.\nInstalling the latest version of",name,"\n")
        if(!dryrun){
          devtools::install_version(name,version=NULL,repos=repo,quiet = TRUE)
        }
        output = 0
      }
    } else {
      if(install){
        if(verbose) cat("NOTE:",name,"is not the correct version.\nInstalling",name,"version",version,"\n")
        if(!dryrun){
          devtools::install_version(name,version=version,repos=repo,quiet = TRUE)
        }
        output = 0
      }
    }
  }
  return(output)
}

load_packages = function(packages,verbose,dryrun){
  if(verbose) cat("Loading",packages[1],"version",packages[3],"\n")
  if(!dryrun) library(packages[1],character.only=TRUE)
  return(0)
}

read_archive = function (repo){
  tryCatch({
    con = gzcon(url(sprintf("%s/src/contrib/Meta/archive.rds", repo), "rb"))
    on.exit(close(con))
    readRDS(con)
  }, warning = function(e) {
    list()
  }, error = function(e) {
    list()
  })
}

check_newest_version = function(package, repo, target, comp){
  archive = read_archive(repo)
  info = archive[[package]]
  if (!is.null(info)) {
    info$repo = repo
    info$path = rownames(info)
  }
  latest = info[order(info$path, info$mtime),][nrow(info),"path"]
  version = strsplit(latest,"_")[[1]][2]
  version = sub("\\.tar\\.gz","",version)
  new_enough = compare_version(target, version, comp)
  return(new_enough)
}
