context("gen_requirements")

# nolint start
TMP_DIR = tempdir()
FILE_1 = file.path(TMP_DIR, "file_1.R")
FILE_2 = file.path(TMP_DIR, "file_2.R")
FILE_3 = file.path(TMP_DIR, "file_3.R")

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
})

teardown({
  unlink(TMP_DIR)
})

test_that("read_package_lines_from_files", {
  expect_equal(read_package_lines_from_files(FILES), PACKAGE_LINES_ALL)
})

test_that("match_packages", {
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


test_that("generate_requirements", {
  test_requirements_file = file.path(TMP_DIR, "requirements.txt")
  test_glob = file.path(TMP_DIR, "file_*.R")

  generate_requirements(test_glob, test_requirements_file, eq_sym = NULL)

  expect_equal(
    readLines(test_requirements_file),
    sort(PACKAGES_ALL)
  )
})
