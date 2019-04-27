source('internal_data/comparison_symbols.R')
source('internal_data/regexes.R')
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
  ERROR_FAILURE_COUNT,          # Error message for counting the number of packages which failed to install
  REQ_FILE_EXIST_ERR,           # Error message for non-existing requirements file
  REQ_FILE_EMPTY_ERR,           # Error message for empty requirements file
  REQ_FILE_RESOLUTION_ERR,      # Error message for non-resolvable requirements requirements file
  REQ_FILE_INVALID_COMP,        # Error message for invalid comparison operator
  REQ_FILE_DUPLICATE_REQ,       # Error message for duplicated requirements
  VALID_REQ,                    # Pattern for validating versioned requirements
  COMP_EXTRACTOR,               # Pattern for extracting comparison operator
  CANONICAL_PACKAGE_NAME_RE_STANDALONE,  # String containing regex to match valid package names by CRAN standards
  CANONICAL_PACKAGE_NAME_RE_EXTRACT,     # String containing regex to extract valid package names by CRAN standards
  PACKAGE_RES,                # Character vector of regexes to match package names used in code (i.e. `library(name)`)
  AUTO_GEN_COMMENTS,          # Comments to be placed at the top of auto-generated requirements files
  internal = TRUE,
  overwrite = TRUE
)
