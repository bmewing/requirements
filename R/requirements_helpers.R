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
