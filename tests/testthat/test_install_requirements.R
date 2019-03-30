context('general_helpers')

test_that('Reading requirements files', {
  expect_equal(read_requirements_file('testdata/requirements_1.txt'), c("mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice < 1", "whitechapelR >= 0.3"))
  expect_equal(read_requirements_file('testdata/requirements_2.txt'), c("mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice ~= 0", "whitechapelR >= 0.3", "dplyr != 0.7.*"))
})


context('install_requirements')

TMP_DIR = tempdir()
writeLines(c('library(mgsub)','require(lexRankr)'),file.path(TMP_DIR,'dummy.R'))
INST = c("mgsub"="1.5.1.3","lexRankr"="0.4.1","readOffice"="0.2.2")

test_that('Invalid Package Requirement', {
  expect_error(install_requirements('testdata/requirements_1.txt',
                                 gen = FALSE, packrat = FALSE,
                                 dryrun = TRUE, verbose = FALSE,
                                 dummy = INST))
})

test_that('Requirements Generate Correctly', {
  expect_error(install_requirements(file.path(TMP_DIR,'tmp_req.txt'),
                                      gen = TRUE, packrat = FALSE,
                                      dryrun = TRUE, verbose = TRUE,
                                      dummy = INST))
})

test_that('Getting available package versions', {
  expect_equal(get_available_versions('mgsub')[1:4],
               c("1.0.0", "1.5.0", "1.7.0", "1.7.1"))

  expect_null(get_available_versions('lexrankR'))

})
