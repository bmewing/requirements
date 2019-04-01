source('internal_data/regexes.R')
source('internal_data/comparison_symbols.R')

usethis::use_data(
  COMPS,                      # Character vector of valid equality comparison symbols
  CANONICAL_PACKAGE_NAME_RE_STANDALONE,  # String contain regex to match valid package names by CRAN standards
  PACKAGE_RES,                # Character vector of regexes to match package names used in code (i.e. `library(name)`)
  internal = TRUE,
  overwrite = TRUE
)
