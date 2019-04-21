context("gen_requirements")

# nolint start
TMP_DIR = tempdir()
FILE_1 = file.path(TMP_DIR, "file_1.R")
FILE_2 = file.path(TMP_DIR, "file_2.R")
FILE_3 = file.path(TMP_DIR, "file_3.R")
LOCKFILE = file.path(TMP_DIR, "test_packrat.lock")
LOCKFILE_MALFORMED = file.path(TMP_DIR, "test_malformed_packrat.lock")

FILE_TEXT_1 = c(
  "library(fake.package, quietly = TRUE)",
  "require(testthat)",
  "pacman::p_load('dplyr')",
  "requirements::requirements()",
  "devtools:::fake_function()",
  "stringr::function(readr:::function)",
  "dt[, x := 1]\n"
)

FILE_TEXT_2 = c("library(packrat)",
                "library(fake_package)\n")
FILE_TEXT_3 = ""

LOCKFILE_TEXT =
"PackratFormat: 1.4
PackratVersion: 0.4.8.1
RVersion: 3.3.0
Repos: CRAN=https://cran.rstudio.com/

Package: BH
Source: CRAN
Version: 1.62.0-1
Hash: 14dfb3e8ffe20996118306ff4de1fab2

Package: DT
Source: CRAN
Version: 0.2
Hash: 36b032203797956fedad5a25055016a9
Requires: htmltools, htmlwidgets, magrittr
"

LOCKFILE_MALFORMED_TEXT =
"PackratFormat: 1.4
PackratVersion: 0.4.8.1
RVersion: 3.3.0
Repos: CRAN=https://cran.rstudio.com/

Package: BH
Source: CRAN
Hash: 14dfb3e8ffe20996118306ff4de1fab2

Package: DT
Source: CRAN
Version: 0.2
Hash: 36b032203797956fedad5a25055016a9
Requires: htmltools, htmlwidgets, magrittr
"

FILES = c(FILE_1, FILE_2, FILE_3)
FILE_TEXTS = list(FILE_TEXT_1, FILE_TEXT_2, FILE_TEXT_3)

PACKAGE_LINES_1 = FILE_TEXT_1[-length(FILE_TEXT_1)]
PACKAGE_LINES_2 = trimws(FILE_TEXT_2)
PACKAGE_LINES_3 = character(0)

PACKAGE_LINES_ALL = c(PACKAGE_LINES_1, PACKAGE_LINES_2, PACKAGE_LINES_3)
PACKAGE_LINES_ALL = unique(PACKAGE_LINES_ALL)

PACKAGES_1 = c("fake.package", "testthat", "dplyr", "pacman", "requirements", "devtools", "stringr", "readr")
PACKAGES_2 = c("packrat")
PACKAGES_ALL = c(PACKAGES_1, PACKAGES_2)
# nolint end

setup({
  invisible(
    mapply(writeLines, text = FILE_TEXTS, con = FILES)
  )
  write(LOCKFILE_TEXT, LOCKFILE)
  write(LOCKFILE_MALFORMED_TEXT, LOCKFILE_MALFORMED)
})

teardown({
  unlink(TMP_DIR)
})

test_that("read_package_lines_from_files", {
  expect_equal(read_package_lines_from_files(character(0)), character(0))

  expect_equal(read_package_lines_from_files(FILES), PACKAGE_LINES_ALL)
})

test_that("str_match_all", {
  expect_equal(
    str_match_all("test", "test"),
    list(structure("test", .Dim = c(1L, 1L)))
  )

  expect_equal(
    str_match_all(c("test", "best"), "(.)est"),
    list(structure(c("test", "t"), .Dim = 1:2),
         structure(c("best", "b"), .Dim = 1:2))
  )

  expect_equal(
    str_match_all(c("test", "nope"), "(.)est"),
    list(structure(c("test", "t"), .Dim = 1:2),
         structure(character(0), .Dim = c(0L, 2L)))
  )

  expect_equal(
    str_match_all(c("nope"), "(.)est"),
    list(structure(character(0), .Dim = 0:1))
  )
})

test_that("read_reqs_from_lockfile", {
  expect_equal(
    read_reqs_from_lockfile(lockfile_path = LOCKFILE),
    c("BH>=1.62.0-1", "DT>=0.2")
  )

  expect_equal(
    read_reqs_from_lockfile(lockfile_path = LOCKFILE, eq_sym = "="),
    c("BH==1.62.0-1", "DT==0.2")
  )

  expect_equal(
    read_reqs_from_lockfile(lockfile_path = LOCKFILE, eq_sym = NULL),
    c("BH", "DT")
  )

  expect_error(
    read_reqs_from_lockfile(lockfile_path = LOCKFILE_MALFORMED),
    regexp = "Malformed lockfile"
  )
})

test_that("match_packages", {
  expect_equal(match_packages(character(0), PACKAGE_RES), character(0))

  expect_equal(
    sort(match_packages(PACKAGE_LINES_ALL, PACKAGE_RES)),
    sort(PACKAGES_ALL)
  )
})

test_that("safe_package_version", {
  expect_equal(
    safe_package_version(""),
    NA_character_,
    label = "return NA for fake package"
  )

  expect_equal(
    safe_package_version("testthat"),
    packageVersion("testthat"),
    label = "defaults"
  )
})

test_that("append_version_requirements", {
  expect_equal(
    append_version_requirement("testthat"),
    paste0("testthat>=", packageVersion("testthat")),
    label = "defaults"
  )

  expect_equal(
    append_version_requirement("testthat", eq_sym = COMP_EXACTLY_EQUAL),
    paste0("testthat==", packageVersion("testthat")),
    label = "change eq_sym"
  )

  expect_equal(
    append_version_requirement("f", rm_missing = TRUE),
    NA_character_,
    label = "single fake package rm_missing = TRUE (return NA)"
  )

  expect_equal(
    append_version_requirements(c("*", "testthat"), rm_missing = TRUE),
    paste0("testthat>=", packageVersion("testthat")),
    label = "fake package rm_missing = TRUE"
  )

  expect_equal(
    append_version_requirements(c("*", "testthat"), rm_missing = FALSE),
    c("*", paste0("testthat>=", packageVersion("testthat"))),
    label = "fake package rm_missing = FALSE"
  )
})

test_that("write_requirements_file", {
  test_requirements_file = file.path(TMP_DIR, "test_blank_requirements.txt")

  expect_message(
    write_requirements_file(character(0), test_requirements_file),
    regexp = "Writing a blank requirements file"
  )
})

test_that("generate_requirements", {
  test_requirements_file = file.path(TMP_DIR, "requirements.txt")
  test_glob = file.path(TMP_DIR, "file_*.R")

  generate_requirements(test_glob, test_requirements_file, eq_sym = NULL)

  expect_equal(
    readLines(test_requirements_file),
    c(AUTO_GEN_COMMENTS, sort(PACKAGES_ALL))
  )
})

test_that("validate_eq_sym", {
  expect_error(
    validate_eq_sym("42"),
    regexp = "'42'"
  )

  expect_equal(validate_eq_sym("=="), "==")
  expect_equal(validate_eq_sym("="), "==")

  expect_error(
    validate_eq_sym(c("==", "==")),
    regexp = "length\\(eq_sym\\) == 1 is not TRUE"
  )
})
