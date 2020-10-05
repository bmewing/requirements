context("requirements classes")

test_that("requirement class works properly", {
  expect_equal(class(tmp <- requirement$new("dummy"))[1], "requirement") #nolint
  expect_false(tmp$is_installed())
})

test_that("CRAN_Requirement works properly", {
  #totally valid, unversioned
  expect_equal(class(tmp <- cran_req$new("mgsub"))[1], "CRAN_Requirement") #nolint
  expect_equal(tmp$package, "mgsub")
  expect_equal(tmp$version, NA_character_)
  expect_message(tmp$get_available_versions(), regexp = "1\\.5\\.0")
  #totally valid, versioned
  expect_equal(class(tmp <- cran_req$new("AGread >0.5"))[2], "requirement") #nolint
  expect_equal(tmp$package, "AGread")
  expect_equal(tmp$version, "> 0.5")
  expect_message(tmp$get_available_versions(), regexp = "1\\.1\\.1")
  expect_true(tmp$version_to_install == rev(tmp$get_available_versions())[1])
  expect_false(tmp$is_installed())
  expect_message(tmp$is_installed(), regexp="Not Installed")
  expect
  #totally valid, multiple versioned
  expect_equal(class(tmp <- cran_req$new("AGread >0.5, <1.0"))[2], "requirement") #nolint
  expect_equal(tmp$package, "AGread")
  expect_length(tmp$version, 2)
  expect_message(x <- tmp$get_available_versions(), regexp = "1\\.1\\.1")
  expect_true(is.na(tmp$version_to_install))
  expect_false(tmp$is_installed())
  #totally valid, unversioned
  expect_equal(class(tmp <- cran_req$new("base"))[1], "CRAN_Requirement") #nolint
  expect_equal(tmp$package, "base")
  expect_equal(tmp$version, NA_character_)
  expect_true(tmp$is_installed())
  expect_message(tmp$is_installed(), regexp="Installed")

  #valid name, doesn't exist
  expect_error(cran_req$new("emnTAW"), regexp = "not found in provided repos")
  #all failpoints
  expect_error(cran_req$new("AGread >0.5, >=1.2, >= 6.2"), regexp = "Too many versions")
  expect_error(cran_req$new("AGread >0.5, ==1.2"), regexp = "Illegal comparisons")
  expect_error(cran_req$new("AGread ==0.5, ==1.2"), regexp = "Illegal comparisons")
  expect_error(cran_req$new("AGread >0.5, ~=1.2"), regexp = "Illegal comparisons")
  expect_error(cran_req$new("AGread ~=0.5, <=1.2"), regexp = "Illegal comparisons")
  expect_error(cran_req$new("AGread >>0.5"), regexp = "Illegal comparisons")
  expect_error(cran_req$new("2fun2quit"), regexp = "Illegal package name")
})
