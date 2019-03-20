context('gen_requirements')

TMP_DIR = tempdir()
FILE_1 = file.path(tmp_dir, 'file_1.R')
FILE_2 = file.path(tmp_dir, 'file_2.R')
FILE_3 = file.path(tmp_dir, 'file_3.R')

FILE_TEXT_1 = c(
  'library(fake.package)',
  'require(testthat)',
  'pacman::p_load("dplyr")',
  'requirements::requirements()',
  'devtools:::fake_function()',
  'dt[, x := 1]\n'
)

FILE_TEXT_2 = c('library(packrat)',
                'library(fake_package)\n')
FILE_TEXT_3 = ''

FILES = c(FILE_1, FILE_2, FILE_3)
FILE_TEXTS = list(FILE_TEXT_1, FILE_TEXT_2, FILE_TEXT_3)

PACKAGE_LINES_1 = FILE_TEXT_1[-length(FILE_TEXT_1)]
PACKAGE_LINES_2 = trimws(FILE_TEXT_2)
PACKAGE_LINES_3 = character(0)

PACKAGE_LINES_ALL = c(PACKAGE_LINES_1, PACKAGE_LINES_2, PACKAGE_LINES_3)
PACKAGE_LINES_ALL = unique(PACKAGE_LINES_ALL)

PACKAGES_ALL = c('fake.package', 'packrat', 'testthat', 'pacman', 'requirements', 'devtools')

setup({
  invisible(
    mapply(writeLines, text=FILE_TEXTS, con=FILES)
  )
})

teardown({
  unlink(tmp_dir)
})

test_that('read_package_lines_from_files', {
  expect_equal(read_package_lines_from_files(FILES), PACKAGE_LINES_ALL)
})

test_that('match_packages', {
  expect_equal(match_packages(PACKAGE_LINES_ALL, PACKAGE_RES), PACKAGES_ALL)
})

test_that('safe_package_version', {
  expect_equal(safe_package_version(''), NA_character_)
  expect_equal(safe_package_version('testthat'), packageVersion('testthat'))
})

test_that('append_version_requirements', {
  expect_equal(
    append_version_requirement('f', rm_missing = TRUE),
    NA_character_
  )
  
  expect_equal(
    append_version_requirement('testthat'),
    paste0('testthat>=', packageVersion('testthat'))
  )
  
  expect_equal(
    append_version_requirement('testthat', eq_sym = '=='),
    paste0('testthat==', packageVersion('testthat'))
  )
  
  expect_equal(
    append_version_requirements(c('*', 'testthat'), rm_missing = TRUE),
    paste0('testthat==', packageVersion('testthat'))
  )
  
  expect_equal(
    append_version_requirements(c('*', 'testthat'), rm_missing = FALSE),
    c('*', paste0('testthat>=', packageVersion('testthat')))
  )
})

