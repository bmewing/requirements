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

test_that('Package Logic', {
  expect_message(install_requirements('testdata/requirements_2.txt',
                                      gen = FALSE, packrat = FALSE,
                                      dryrun = TRUE, verbose = TRUE,
                                      dummy = INST))
})