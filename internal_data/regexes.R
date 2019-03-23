# quote from: https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Creating-R-packages
# > The mandatory ‘Package’ field gives the name of the package.
# > This should contain only (ASCII) letters, numbers and dot,
# > have at least two characters and start with a letter and not end in a dot.
CANONICAL_PACKAGE_NAME_RE = '[a-zA-Z]{1}[a-zA-Z0-9\\.]*[a-zA-Z0-9]{1,}'
LIB_RE   = sprintf('library\\([\'"]?(%s)[\'"]?[\\),]', CANONICAL_PACKAGE_NAME_RE)
REQ_RE   = sprintf('require\\([\'"]?(%s)[\'"]?[\\),]', CANONICAL_PACKAGE_NAME_RE)
PAC_RE   = sprintf('p_load\\([\'"]?(%s)[\'"]?[\\),]',  CANONICAL_PACKAGE_NAME_RE)
COLON_RE = sprintf('(%s):{2,3}[^:]',                   CANONICAL_PACKAGE_NAME_RE)

PACKAGE_RES = c(LIB_RE, REQ_RE, PAC_RE, COLON_RE)
