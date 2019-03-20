#' @export

requirements <- function(root_dir = getwd(), load = TRUE, packrat = FALSE, dryrun = FALSE,
                         verbose = TRUE, repo = "https://cran.rstudio.com") {
  #' @title Load project requirements
  #'
  #' @description Load and (if necessary) install required packages
  #'
  #' @param root_dir Path to where requirements.txt is located
  #' @param packrat Should project be initialized with packrat?
  #' @param dryrun Flag if you just want to see what would happen if your ran
  #' the function
  #' @param verbose Flag to change verbosity
  #' @param repo What repository should be used to install pacakges?
  #'
  #' @return invisible
  #' @examples
  #' \dontrun{
  #' requirements(dryrun = TRUE)
  #' }
  req = file.path(root_dir,"requirements.txt")
  stopifnot(file.exists(req))

  content = readLines(req)
  stopifnot(length(content) > 0)

  if(packrat){
    tryCatch(packrat::status(),
             error=function(x){packrat::init();packrat::on()})
  }

  required = strsplit(content," +")
  installed = installed.packages()[,3]
  needed = vapply(required,compare_and_install,FUN.VALUE = 1,
                  existing=installed,
                  dryrun=dryrun,
                  verbose=verbose,
                  repo=repo)
  installed = installed.packages()[,3]
  if(load){
    loading = lapply(required,load_packages,
                     verbose=verbose,
                     dryrun=dryrun)
  }

  if(dryrun) cat("NOTE: This was just a dry run. No packages have been installed or loaded.")
  return(invisible(0))
}

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

compare_and_install = function(target,existing,dryrun,verbose,repo){
  name = target[1]
  installed = existing[name]
  comp = target[2]
  version = target[3]
  if(comp == "=") comp = "=="
  if(is.na(installed)){
    if(verbose) cat("NOTE:",name,"is not installed.\nInstalling",name,"version",version,"\n")
    if(!dryrun){
      devtools::install_version(name,version=version,repos=repo,quiet = TRUE)
    }
    return(1)
  }

  correct_version = compare_version(version,installed,comp)

  if(correct_version){
    if(verbose & dryrun) cat("NOTE:",name,"is already installed with version",version,"\n")
    return(0)
  } else {
    if(grepl(">",comp)){
      newest_good_enough = check_newest_version(name, repo, version, comp)
      if(!newest_good_enough){
        stop(paste0("ERROR: ",name," is not a new enough version. However, no version exists which is suitable."))
      }
      if(verbose) cat("NOTE:",name,"is not a new enough version.\nInstalling the latest version of",name,"\n")
      if(!dryrun){
        devtools::install_version(name,version=NULL,repos=repo,quiet = TRUE)
      }
    } else {
      if(verbose) cat("NOTE:",name,"is not the correct version.\nInstalling",name,"version",version,"\n")
      if(!dryrun){
        devtools::install_version(name,version=version,repos=repo,quiet = TRUE)
      }
    }
    return(1)
  }
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
