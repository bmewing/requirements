cran_locations = R6::R6Class("CRAN_Locations",
  public = list(
    urls = NULL,
    original = options("repos"),
    initialize = function(index_url = NULL, extra_index_urls = NULL) {
      if (!is.null(index_url)) {
        self$urls = c(index_url, extra_index_urls)
      } else {
        self$urls = c(options("repos")[["repos"]], extra_index_urls)
      }
    },
    reset = function() {
      options("repos") = self$original
    }
  )
)

installed_packages = R6::R6Class("Installed_Packages",
  public = list(
    installed = installed.packages()[, 3],
    initialize = function(dummy=NULL) {
      if (!is.null(dummy)) {
        self$installed = dummy
      }
    },

    is_installed = function(requirement) {
      details = self$installed[requirement$name]
      installed = !is.na(details) && private$compare_version(details, requirement$version, requirement$comparison)
      return(installed)
    }
  ),
  private = list(
    compare_version = function(existing, target, comp) {
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

      get(comp)(existing_version, target_version)
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
    }
  )
)
