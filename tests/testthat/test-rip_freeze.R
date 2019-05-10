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

Package: testthat
Source: CRAN
Version: 0.0.0
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
    list(structure(character(0), .Dim = c(0L, 2L)))
  )
})

test_that("read_reqs_from_lockfile", {
  expected_matched_package_df = data.frame(name = c("testthat", "DT"),
                                           version = c("0.0.0", "0.2"),
                                           stringsAsFactors = FALSE)

  expect_equal(
    read_reqs_from_lockfile(lockfile_path = LOCKFILE),
    expected_matched_package_df
  )

  expected_matched_package_df[["version"]] = NA_character_

  expect_equal(
    read_reqs_from_lockfile(lockfile_path = LOCKFILE, eq_sym = NULL),
    expected_matched_package_df
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

  expect_equal(
    sort(match_packages("library(requirements)", PACKAGE_RES)),
    "requirements"
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
    as.character(packageVersion("testthat")),
    label = "defaults"
  )
})

test_that("append_version_requirements", {
  expected_matched_package_df = data.frame(name = c("testthat", "DT"),
                                           version = c(NA, "0.2"),
                                           stringsAsFactors = FALSE)
  expect_equal(
    append_version_requirements(expected_matched_package_df, eq_sym = ">="),
    c("DT>=0.2", "testthat")
  )

  expect_equal(
    append_version_requirements(expected_matched_package_df, eq_sym = "=="),
    c("DT==0.2", "testthat")
  )

  expect_equal(
    append_version_requirements(expected_matched_package_df, eq_sym = NULL),
    c("DT", "testthat")
  )

  expect_equal(
    append_version_requirements(expected_matched_package_df[1, ]),
    "testthat"
  )
})

test_that("write_requirements_file", {
  test_requirements_file = file.path(TMP_DIR, "test_blank_requirements.txt")

  expect_message(
    write_requirements_file(character(0), test_requirements_file),
    regexp = "Writing a blank requirements file"
  )

  expect_equal({
    write_requirements_file("test_req", test_requirements_file)
    readLines(test_requirements_file)
  },
  c(AUTO_GEN_COMMENTS, "test_req")
  )

  expect_equal({
    write_requirements_file("test_req2", test_requirements_file, append = TRUE)
    readLines(test_requirements_file)
  },
  c(AUTO_GEN_COMMENTS, "test_req", "test_req2")
  )
})

test_that("generate_requirements", {
  test_requirements_file = file.path(TMP_DIR, "requirements.txt")
  test_glob = file.path(TMP_DIR, "file_*.R")

  generate_requirements(FILE_3, test_requirements_file, rm_missing = TRUE)

  expect_equal(
    readLines(test_requirements_file),
    c(AUTO_GEN_COMMENTS)
  )

  generate_requirements(test_glob, test_requirements_file, eq_sym = NULL, packrat_lock_path = LOCKFILE)

  expect_equal(
    readLines(test_requirements_file),
    c(AUTO_GEN_COMMENTS, sort(unique(c(PACKAGES_ALL, "testthat", "DT"))))
  )

  rip_freeze(test_glob, test_requirements_file, eq_sym = NULL, packrat_lock_path = LOCKFILE)

  expect_equal(
    readLines(test_requirements_file),
    c(AUTO_GEN_COMMENTS, sort(unique(c(PACKAGES_ALL, "testthat", "DT"))))
  )
})

test_that("rip_freeze_packrat", {
  test_requirements_file = file.path(TMP_DIR, "requirements.txt")

  rip_freeze_packrat(LOCKFILE, test_requirements_file, append = FALSE)

  expect_equal(
    readLines(test_requirements_file),
    c(AUTO_GEN_COMMENTS, "DT>=0.2", "testthat>=0.0.0")
  )
})

test_that("validate_eq_sym", {
  expect_error(
    validate_eq_sym("42"),
    regexp = "'42'"
  )

  expect_error(
    validate_eq_sym("="),
    regexp = "not a valid comparison operator"
  )

  expect_equal(validate_eq_sym("=="), "==")

  expect_error(
    validate_eq_sym(c("==", "==")),
    regexp = "length\\(eq_sym\\) == 1 is not TRUE"
  )
})

test_that("rm_dup_matched_packages", {
  input_df = data.frame(name = c("testthat", "testthat", "testthat", "DT", "DT", "mgsub", "mgsub", "BH"),
                        version = c("0.0.0", "2.0.0", "1.0.0", "42.0.0", "3.0.0", "1.0.0", "1.0.0", "1.0.0"),
                        stringsAsFactors = FALSE)

  expected_output_df = input_df[c(8, 4, 7, 2), ]
  row.names(expected_output_df) = NULL

  expect_equal(
    suppressWarnings(rm_dup_matched_packages(input_df)),
    expected_output_df
  )

  expect_warning(
    rm_dup_matched_packages(input_df),
    regexp = "DT, testthat"
  )

  expect_silent(
    rm_dup_matched_packages(input_df[6:7, ])
  )
})
