context("test parent wrapper installation function")
test_that("install_requirements", {
  expect_message(install_requirements("testdata/requirements_2.txt",
                                      packrat = FALSE,
                                      dryrun = TRUE,
                                      verbose = FALSE),
                 regexp = "NOTE: This was just a dry run. No packages have been installed.")
})

INST = c("mgsub" = "1.5.1.3", "lexRankr" = "0.4.1", "readOffice" = "0.2.2")  # nolint

context("compare_version & helpers")

test_that("compare_compatible_version", {
  expect_true(compare_compatible_version("1.1.1", "1.*"))
  expect_true(compare_compatible_version("1.1.1", "1.0"))

  expect_false(compare_compatible_version("1.1.1", "2.0"))
})


test_that("equivalent_version", {
  existing_version = "1.1.1"
  equivalaent_target_versions = c(
    "*",
    "1.*", "*.1",
    "*.1.1", "1.*.1", "1.1.*",
    "*.*.1", "1.*.*", "*.1.*",
    "*.*.*",
    "1.1.1"
  )

  test_results = vapply(equivalaent_target_versions, function(etv) {
    equivalent_version(existing_version, etv)
  }, logical(1))  # nolint

  expect_true(all(test_results))

  expect_false(equivalent_version("1.1.1", "*.*.*.*"))
  expect_false(equivalent_version("1.1.1", "1.2.*"))
  expect_false(equivalent_version("1.1.1", "1.1.2"))
})


test_that("compare_version", {
  expect_true(compare_version("1.0.1", "1.0.1", COMP_EXACTLY_EQUAL))
  expect_true(compare_version("1.0.1", "1.*",   COMP_EXACTLY_EQUAL))
  expect_true(compare_version("1.0.1", "1.0.1", COMP_GTE))
  expect_true(compare_version("1.0.1", "1.*",   COMP_GTE))
  expect_true(compare_version("1.0.1", "0.9",   COMP_GT))
  expect_true(compare_version("1.0.1", "1.*",   COMP_LTE))
  expect_true(compare_version("1.0.1", "1.5",   COMP_LT))
  expect_true(compare_version("1.0.1", "1.5",   COMP_NOT_EQUAL))
  expect_true(compare_version("1.0.1", "2.*",   COMP_NOT_EQUAL))
  expect_true(compare_version("1.0.1", "*.0.2", COMP_NOT_EQUAL))

  expect_false(compare_version("1.0.1", "1.0.2", COMP_EXACTLY_EQUAL))
  expect_false(compare_version("2.0.1", "1.*",   COMP_EXACTLY_EQUAL))
  expect_false(compare_version("0.0.1", "1.0.1", COMP_GTE))
  expect_false(compare_version("0.0.1", "1.*",   COMP_GTE))
  expect_false(compare_version("0.9.0", "0.9",   COMP_GT))
  expect_false(compare_version("2.0.1", "1.*",   COMP_LTE))
  expect_false(compare_version("2.0.1", "1.5",   COMP_LT))
  expect_false(compare_version("1.5.0", "1.5",   COMP_NOT_EQUAL))
  expect_false(compare_version("2.1.0", "2.*",   COMP_NOT_EQUAL))
  expect_false(compare_version("1.0.2", "*.0.2", COMP_NOT_EQUAL))
})


context("General Helpers")


test_that("legal_r_package_name", {
  expect_true(legal_r_package_name("mgsub"))
  expect_false(legal_r_package_name("2fun2quit"))
})

test_that("get_installed", {
  expect_type(get_installed(), "character")
  expect_true(length(get_installed()) > 0)
  expect_type(get_installed(dummy = INST), "character")
  expect_equal(get_installed(dummy = INST), INST)
})

test_that("vmess", {
  expect_message(vmess("hi", TRUE), "hi")
  expect_silent(vmess("hi", FALSE))
})

context("Core Install Functionality")

TMP_DIR = tempdir()  # nolint
writeLines(c("library(mgsub)", "require(lexRankr)"), file.path(TMP_DIR, "dummy.R"))

test_that("Invalid Package Requirement", {
  expect_error(install_reqs(reqs = req1,
                            dryrun = TRUE, verbose = FALSE,
                            dummy = INST))
})

test_that("Getting available package versions", {
  expect_equal(get_available_versions("mgsub")[1:4],
               c("1.0.0", "1.5.0", "1.7.0", "1.7.1"))

  expect_null(get_available_versions("lexrankR"))
})
