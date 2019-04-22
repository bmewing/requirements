context("install_reqs.R")

INST = c("mgsub" = "1.5.1.3", "lexRankr" = "0.4.1", "readOffice" = "0.2.2")  # nolint

test_that("install_special_req", {
  #check working fine message on git
  expect_message(fail_count <- install_special_req("git+https://github.com/bmewing/mgsub", # nolint
                                     GIT_REPLACE, remotes::install_git, TRUE, TRUE),
                 regexp = "https://github\\.com/bmewing/mgsub")
  #check git working fine
  expect_equal(fail_count, 0)
  #check failure message on git
  expect_message(fail_count <- install_special_req("git+https://github.com/bmewing/thiswillneverexist", # nolint
                                     GIT_REPLACE, remotes::install_git, FALSE, TRUE),
                 regexp = sprintf(ERROR_OTHER_FAILURE, "https://github\\.com/bmewing/thiswillneverexist"))
  #check failure counts return properly
  expect_equal(fail_count, 1)
  #check success for svn
  expect_equal(install_special_req("svn+svn://scm.r-forge.r-project.org/svnroot/daewr/pkg/mixexp",
                                   SVN_EXT_REP, remotes::install_svn, TRUE, FALSE), 0)
  #check success for bioconductor
  expect_equal(install_special_req("bioc+release/SummarizedExperiment",
                                   BIO_EXT_REP, remotes::install_bioc, TRUE, FALSE), 0)
  #check success for url
  expect_equal(install_special_req("https://github.com/bmewing/mgsub/releases/download/v.1.5/mgsub_1.5.0.tar.gz",
                                   "", remotes::install_url, TRUE, FALSE), 0)
  #check success for local
  expect_equal(install_special_req("testdata/dummy_package.zip", "", remotes::install_local, TRUE, FALSE), 0)
})

test_that("install_unversioned", {
  expect_message(fail_count <- install_unversioned(elem = "dplyr", # nolint
                                                   installed = INST,
                                                   dryrun = TRUE,
                                                   verbose = TRUE,
                                                   repo = "https://cran.rstudio.com"),
                 regexp = "dplyr")
  expect_equal(fail_count, 0)
  expect_message(fail_count <- install_unversioned(elem = "2fun2quit", # nolint
                                                   installed = INST,
                                                   dryrun = FALSE,
                                                   verbose = TRUE,
                                                   repo = "https://cran.rstudio.com"),
               regexp = sprintf(ERROR_NO_PACKAGE_EXISTS, "2fun2quit"))
  expect_equal(fail_count, 1)
})

test_that("install_cran_package", {
  expect_silent(fail_count <- install_cran_package(package = "mgsub", # nolint
                                                   version = "1.5.0",
                                                   repo = "https://cran.rstudio.com",
                                                   dryrun = FALSE))
  expect_equal(fail_count, 0)
  expect_silent(fail_count <- install_cran_package(package = "mgsub", # nolint
                                                   version = NULL,
                                                   repo = "https://cran.rstudio.com",
                                                   dryrun = FALSE))
  expect_equal(fail_count, 0)
  expect_message(fail_count <- install_cran_package(package = "2fun2quit", # nolint
                                                    version = NULL,
                                                    repo = "https://cran.rstudio.com",
                                                    dryrun = FALSE),
                 regexp = "2fun2quit")
  expect_equal(fail_count, 1)
})

test_that("process_versioned_requirement", {
  expect_type(process_versioned_requirement("mgsub=1.5"), "list")
  expect_equal(process_versioned_requirement("mgsub=1.5"),
               list(package = "mgsub", version = "1.5", comp = COMP_EXACTLY_EQUAL))
  expect_equal(process_versioned_requirement("mgsub == 1.5"),
               list(package = "mgsub", version = "1.5", comp = COMP_EXACTLY_EQUAL))
  expect_equal(process_versioned_requirement("mgsub>=1.*"),
               list(package = "mgsub", version = "1.*", comp = COMP_GTE))
  expect_equal(process_versioned_requirement("mgsub>1.*"),
               list(package = "mgsub", version = "1.*", comp = COMP_GT))
  expect_equal(process_versioned_requirement("mgsub<=1.*"),
               list(package = "mgsub", version = "1.*", comp = COMP_LTE))
  expect_equal(process_versioned_requirement("mgsub<1.*"),
               list(package = "mgsub", version = "1.*", comp = COMP_LT))
  expect_equal(process_versioned_requirement("mgsub!=1.*"),
               list(package = "mgsub", version = "1.*", comp = COMP_NOT_EQUAL))
  expect_equal(process_versioned_requirement("mgsub~=1.*"),
               list(package = "mgsub", version = "1.*", comp = COMP_COMPATIBLE))
})

test_that("is_versioned_install_needed", {
  expect_message(res <- is_versioned_install_needed(package = "dplyr", # nolint
                                             version = "*",
                                             comp = "==",
                                             installed = INST,
                                             verbose = TRUE),
                 regexp = sprintf(NOTE_PACKAGE_NOT_INSTALLED, "dplyr"))
  expect_true(res)
  expect_silent(res <- is_versioned_install_needed(package = "mgsub", # nolint
                                                    version = "*",
                                                    comp = "==",
                                                    installed = INST,
                                                    verbose = TRUE))
  expect_false(res)
  expect_true(is_versioned_install_needed(package = "mgsub",
                                          version = "2.0",
                                          comp = ">=",
                                          installed = INST,
                                          verbose = TRUE))
})

test_that("install_if_compat_available", {
  expect_message(fail_count <- install_if_compat_available(list(package = "mgsub", # nolint
                                                                version = "2.0",
                                                                comp = ">="),
                                                           repo = "https://cran.rstudio.com",
                                                           verbose = TRUE,
                                                           dryrun = TRUE),
                 regexp = sprintf(ERROR_NO_PACKAGE_EXISTS, "mgsub"))
  expect_equal(fail_count, 1)

  expect_message(fail_count <- install_if_compat_available(list(package = "mgsub", # nolint
                                                                version = "2.0",
                                                                comp = "<"),
                                                           repo = "https://cran.rstudio.com",
                                                           verbose = TRUE,
                                                           dryrun = TRUE),
                 regexp = sprintf(NOTE_INSTALL_PACKAGE, "mgsub", ""))
  expect_equal(fail_count, 0)
})

test_that("install_versioned", {
  expect_message(fail_count <- install_versioned(elem = "mgsub <= 1.5", # nolint
                                   installed = INST,
                                   dryrun = TRUE,
                                   verbose = TRUE,
                                   repo = "https://cran.rstudio.com"),
                 regexp = sprintf(NOTE_BAD_PACKAGE_VERSION, "mgsub", INST["mgsub"]))
  expect_equal(fail_count, 0)
  expect_equal(install_versioned(elem = "mgsub >= 1.5",
                                 installed = INST,
                                 dryrun = TRUE,
                                 verbose = TRUE,
                                 repo = "https://cran.rstudio.com"), 0)
  expect_equal(install_versioned(elem = "mgsub >= 1.7",
                                 installed = INST,
                                 dryrun = TRUE,
                                 verbose = TRUE,
                                 repo = "https://cran.rstudio.com"), 0)
  expect_equal(install_versioned(elem = "mgsub >= 2.0",
                                 installed = INST,
                                 dryrun = TRUE,
                                 verbose = TRUE,
                                 repo = "https://cran.rstudio.com"), 1)
  expect_equal(install_versioned(elem = NULL,
                                 installed = INST,
                                 dryrun = TRUE,
                                 verbose = TRUE,
                                 repo = "https://cran.rstudio.com"), 0)
})

#reqs, dryrun, verbose = dryrun, dummy=INST
test_that("install_reqs", {
  expect_equal(install_reqs(reqs = process_requirements_file("testdata/requirements_2.txt"),
                            dryrun = TRUE,
                            verbose = TRUE,
                            dummy = INST), 0)
  expect_error(install_reqs(reqs = process_requirements_file("testdata/requirements_5.txt"),
                            dryrun = TRUE,
                            verbose = TRUE,
                            dummy = INST),
               regexp = "There were 2 package")
})

test_that("identify_comparison_op", {
  expect_equal(identify_comparison_op("mgsub=1.5"),  "=")
  expect_equal(identify_comparison_op("mgsub==1.5"), "==")
  expect_equal(identify_comparison_op("mgsub>=1.5"), ">=")
  expect_equal(identify_comparison_op("mgsub<=1.5"), "<=")
  expect_equal(identify_comparison_op("mgsub<1.5"),  "<")
  expect_equal(identify_comparison_op("mgsub>1.5"),  ">")
  expect_equal(identify_comparison_op("mgsub!=1.5"), "!=")
  expect_equal(identify_comparison_op("mgsub~=1.5"), COMP_COMPATIBLE)
  expect_equal(identify_comparison_op("mgsub"), NA_character_)
})
