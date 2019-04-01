source('internal_data/regexes.R')
source('internal_data/comparison_symbols.R')
source('internal_data/messages.R')

usethis::use_data(
  COMPS,                      # Character vector of valid comparison symbols
  COMP_EQUALITY,              # Character vector of valid equality comparison symbols
  COMP_EXACTLY_EQUAL,   # ==
  COMP_LTE,             # <=
  COMP_LT,              # <
  COMP_GTE,             # >=
  COMP_GT,              # >
  COMP_COMPATIBLE,      # ~=
  COMP_NOT_EQUAL,       # !=
  GIT_EXTRACT,          # String containing regex to match special requirement types
  GIT_REPLACE,
  SVN_EXT_REP,
  BIO_EXT_REP,
  URL_EXTRACT,
  NOTE_INSTALL_PACKAGE,         # Note about installing a package
  NOTE_INSTALL_PACKAGE_VERSION, # Note about specific version being installed
  NOTE_PACKAGE_NOT_INSTALLED,   # Note that a required package is not installed
  NOTE_BAD_PACKAGE_VERSION,     # Note that an installed package is of an invalid version
  ERROR_NO_PACKAGE_EXISTS,      # Error message that no package exists which satisfies requirements
  ERROR_OTHER_FAILURE,          # Error message for other generic cases
  CANONICAL_PACKAGE_NAME_RE_STANDALONE,  # String contain regex to match valid package names by CRAN standards
  PACKAGE_RES,                # Character vector of regexes to match package names used in code (i.e. `library(name)`)
  internal = TRUE,
  overwrite = TRUE
)
