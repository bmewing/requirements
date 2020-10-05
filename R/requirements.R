requirement = R6::R6Class("requirement",
  public = list(
    package = NA_character_,
    version = NA_character_,
    initialize = function(requirement) {

    },
    is_installed = function() {
      return(private$installed)
    }
  ),
  private = list(
    installed = FALSE,
    dry_run = FALSE,
    verbose = TRUE,
    message = function(m) {
      if (private$verbose) {
        message(m)
      }

      invisible(return(NULL))
    }
  )
)

cran_req = R6::R6Class("CRAN_Requirement", #nolint
  inherit = requirement,
  public = list(
    repo = NA_character_,
    version_to_install = NA_character_,

    initialize = function(requirement, repos=options("repos")[[1]], verbose=TRUE, dry_run=FALSE) {
      if (repos == "@CRAN@") repos = "https://cran.rstudio.com/"
      private$dry_run = dry_run
      self$repo = repos
      private$valid_requirement(requirement)
      private$check_if_installed()
      if (!private$installed) {
        private$find_available_versions()
        private$choose_version_to_install()
      }
    },

    install = function() {
      if (private$installed) {
        private
      }
      if (!private$installed & !private$dry_run) remotes::install_version(self$package, self$version_to_install)
      private$installed = TRUE
    },

    get_status = function() {
      m = ifelse(private$installed, "Installed", "Not Installed")
      private$message(m)

      return(private$installed)
    },

    get_available_versions = function() {
      if (any(is.na(private$available_versions))) {
        private$find_available_versions()
      }
      private$message(paste(private$available_versions, collapse = ", "))
      return(invisible(private$available_versions))
    }
  ),

  private = list(
    available_versions = NA_character_,
    installed_version = NA_character_,
    valid_name = function(name) {
      #' @param name package name to be checked for validity
      #' @return logical vector of names being legal r package names

      return(grepl(CANONICAL_PACKAGE_NAME_RE_STANDALONE, name))
    },

    valid_comparison = function(version) {
      #' @param req a vector of version comparisons
      #' @return logical indicating if specified comparison is valid
      if (length(version) == 0) {
        return(NA_character_)
      } else if (length(version) > 2) {
        stop(paste0("Too many versions specified: ", paste(version, collapse = ", ")))
      }
      formatted = gsub(pattern = sprintf("([^ 0-9\\.]+) ?(%s)$", MINIMUM_PACKAGE_REQ),
                  replacement = "\\1 \\2",
                  x = version)
      comp = vapply(strsplit(formatted, " "), `[[`, FUN.VALUE = NA_character_, 1)
      valid_comps = all(comp %in% COMPS)
      if (length(version) > 1) {
        valid_comps = valid_comps & !any(comp %in% COMP_UNCOMBINABLE)
      }
      if (!valid_comps) {
        stop(paste0("Illegal comparisons specified: ", paste(version, collapse = ", ")))
      }
      return(formatted)
    },

    valid_requirement = function(req) {
      #' @param req a single versioned requirement string
      #' @return logical indicating a properly formatted requirement
      package_name = sub("^ *([a-zA-Z]{1}[a-zA-Z0-9\\.]*[a-zA-Z0-9]{1,}).*", "\\1", req)
      if (!private$valid_name(package_name)) {
        stop(paste0("Illegal package name detected: ", package_name))
      }
      version = strsplit(trimws(rev(strsplit(req, package_name, fixed = TRUE)[[1]])[1]), ", *")[[1]]
      self$package = package_name
      self$version = private$valid_comparison(version)
      invisible(self)
    },

    install_needed = function() {
      #check if valid version already installed
      installed_version = installed.packages()[, 3][self$package]
      if (!is.na(installed_version)) {
        checks = vapply(self$version, compare_version, FUN.VALUE = logical(1), existing = installed_version)
        private$installed = all(checks)
      }
    },

    find_available_versions = function() {
      #' @return character vector of available package versions; NULL if package not found
      #'
      #' @examples
      #' get_available_versions("mgsub")
      #' # [1] "1.0.0" "1.5.0" "1.7.0" "1.7.1"
      #'
      #' get_available_versions("mgsub_fakerino")
      #' # NULL
      output = NULL
      crandb_url = paste0("http://crandb.r-pkg.org/", self$package, "/all")  # nolint
      response = httr::GET(crandb_url)

      if (httr::status_code(response) == 404) {
        for (r in self$repo) {
          archive_url = sprintf("%ssrc/contrib/Archive/%s", r, self$package)
          current_url = sprintf("%ssrc/contrib/", r)
          archive_page = httr::content(httr::GET(archive_url), as = "text")
          current_page = httr::content(httr::GET(current_url), as = "text")
          output = c(private$extract_versions(archive_page), private$extract_versions(current_page))
          output = output[output != ""]
          if (length(output) == 0) output = NA_character_
          if (!is.na(output)) break
        }
      } else {
        response_content = httr::content(response)
        version_timeline = response_content$timeline
        output = names(version_timeline)
      }

      private$available_versions = output
      if (all(is.na(private$available_versions))) stop(sprintf("Package %s not found in provided repos.", self$package))
    },

    extract_versions = function(cnt) {
      matches = gregexpr(sprintf("%s_[0-9]+(\\.[0-9]+)+\\.[a-zA-Z]", self$package), cnt)
      start = matches[[1]]
      length = attr(matches[[1]], "match.length")
      versions = vapply(seq_along(start),
                        function(i) {
                          substr(cnt, start[i], start[i] + length[i] - 1)
                        },
                        FUN.VALUE = NA_character_)
      final_versions = unique(gsub(sprintf(".*?(%s)\\.[a-zA-Z]", MINIMUM_PACKAGE_REQ), "\\1", versions))
      return(final_versions)
    },

    equivalent_version = function(existing, target) {
      #' @keywords internal
      #' Check if wildcarded target version is equivalent to existing installed version
      #'
      #' @param existing The semantic version number installed
      #' @param target The semantic version number targeted by requirements file
      #'
      #' @return TRUE if versions are equivalent; otherwise FALSE
      #'
      #' @examples
      #' equivalent_version("1.0.1", "1.*")
      #' # [1] TRUE
      #' equivalent_version("1.9.1", "1.*")
      #' # [1] TRUE
      #' equivalent_version("2.0.0", "1.*")
      #' # [1] FALSE
      #'
      # Convert wildcarded versions to regex
      re_target = glob2rx(target)

      grepl(re_target, existing)
    },

    compare_compatible_version = function(existing, target) {
      #' @keywords internal
      #' Check if existing installed version ~= target version
      #'
      #' @param existing The semantic version number installed
      #' @param target The semantic version number targeted by requirements file
      #'
      #' @return TRUE if versions are compatible; otherwise FALSE
      #'
      #' @examples
      #' compare_compatible_version("1.1.1", "1.*")
      #' # [1] TRUE
      #' compare_compatible_version("1.1.1", "1.0")
      #' # [1] TRUE
      #' compare_compatible_version("1.1.1", "2.0")
      #' # [1] FALSE

      # Replace trailing version specifier with wildcard (i.e. 2.2 -> 2.*)
      compatible_version_wildcard = sub("(.*\\.)(.+)", "\\1*", target)

      # Per PEP 440 `~= 2.2` is equivalent to `>= 2.2 & == 2.*`
      private$compare_version(existing, compatible_version_wildcard, COMP_EXACTLY_EQUAL) &
        private$compare_version(existing, target, COMP_GTE)
    },

    compare_version = function(version, existing) {
      #' @keywords internal
      #' Check if existing requirement matches user specified target version
      #'
      #' @param existing The semantic version number installed
      #' @param target The semantic version number targeted by requirements file
      #' @param comp The version comparison operator
      #'
      #' @return logical(1) indicating if comparison is TRUE
      #'
      #' @examples
      #' compare_version("1.0.1", "1.*", COMP_EXACTLY_EQUAL)
      #' [1] TRUE
      #' compare_version("1.0.1", "1.*", COMP_COMPATIBLE)
      #' [1] TRUE
      #' compare_version("1.0.1", "1.*", COMP_NOT_EQUAL)
      #' [1] FALSE

      # Push compatible version check to dedicated helper

      if (is.na(version)){
        output = TRUE
      } else {
        split_version = strsplit(version, " ")
        comp = split_version[[1]][1]
        target = split_version[[1]][2]
        if (comp == COMP_COMPATIBLE) return(private$compare_compatible_version(existing, target))

        # Check if existing == target would satisfy comparison
        is_equality_check = comp %in% COMP_EQUALITY

        # Check if `target` is equivalent to `existing` wrt wildcards
        is_equivalent_version = private$equivalent_version(existing, target)

        if (is_equality_check & is_equivalent_version) return(TRUE)
        if (comp == COMP_NOT_EQUAL & is_equivalent_version) return(FALSE)

        # TODO: definitively prove this is a valid substitution
        non_wild_card_target = gsub("*", "0", target, fixed = TRUE)

        existing_version = package_version(existing)
        target_version = package_version(non_wild_card_target)

        output = get(comp)(existing_version, target_version)
      }
      return(output)
    },

    check_if_installed = function() {
      installed = installed.packages()[, 3]
      private$installed_version = installed[self$package]
      if (!is.na(private$installed_version)) {
        valid_local = vapply(self$version, private$compare_version, FUN.VALUE = logical(1), existing = private$installed_version)
        private$installed = all(valid_local)
      }
    },

    choose_version_to_install = function(){
      sorted = FALSE
      sorted_av = rev(private$available_versions)
      while (!sorted) {
        sorted = TRUE
        for (i in 2:length(private$available_versions)) {
          a = sorted_av[i-1]
          b = sorted_av[i]
          if (package_version(a) < package_version(b)) {
            sorted_av[i] = a
            sorted_av[i-1] = b
            sorted = FALSE
          }
        }
      }
      for (v in sorted_av) {
        valid_version = vapply(self$version, private$compare_version, FUN.VALUE = logical(1),
                               existing = v)
        if (all(valid_version)) {
          self$version_to_install = v
          break
        }
      }
    }
  )
)
