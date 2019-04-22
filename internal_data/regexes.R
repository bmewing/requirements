# quote from: https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Creating-R-packages
# > The mandatory ‘Package’ field gives the name of the package.
# > This should contain only (ASCII) letters, numbers and dot,
# > have at least two characters and start with a letter and not end in a dot.
CANONICAL_PACKAGE_NAME_RE = '[a-zA-Z]{1}[a-zA-Z0-9\\.]*[a-zA-Z0-9]{1,}'
CANONICAL_PACKAGE_NAME_RE_STANDALONE = paste0('^',CANONICAL_PACKAGE_NAME_RE,'$')
LIB_RE   = sprintf('library\\([\'"]?(%s)[\'"]?[\\),]', CANONICAL_PACKAGE_NAME_RE)
REQ_RE   = sprintf('require\\([\'"]?(%s)[\'"]?[\\),]', CANONICAL_PACKAGE_NAME_RE)
PAC_RE   = sprintf('p_load\\([\'"]?(%s)[\'"]?[\\),]',  CANONICAL_PACKAGE_NAME_RE)
COLON_RE = sprintf('(%s):{2,3}[^:]',                   CANONICAL_PACKAGE_NAME_RE)

MINIMUM_PACKAGE_REQ = "[0-9\\*]+(\\.[0-9\\*]+)*?"
VERSIONED_REQ = "^ *%s *(%s) *%s$"
VALID_REQ = sprintf(VERSIONED_REQ,
                    CANONICAL_PACKAGE_NAME_RE,
                    paste(COMPS, collapse="|"),
                    MINIMUM_PACKAGE_REQ)
COMP_EXTRACTOR = sprintf(VERSIONED_REQ,
                         CANONICAL_PACKAGE_NAME_RE,
                         "[^ 0-9]*?",
                         MINIMUM_PACKAGE_REQ)

PACKAGE_RES = c(LIB_RE, REQ_RE, PAC_RE, COLON_RE)

GIT_EXTRACT = "^git[\\+:]" #pattern for recognizing git requirements in .txt
GIT_REPLACE = "^git\\+"    #pattern for preparing git req for install
SVN_EXT_REP = "^svn\\+"    #single pattern for svn
BIO_EXT_REP = "^bioc\\+"   #single pattern for bioconductor installs
URL_EXTRACT = "^https?:"   #pattern for recognizing url requirements in .txt
