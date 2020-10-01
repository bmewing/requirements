context("requirements classes")

test_that("requirement class works properly", {
  expect_equal(class(tmp <- requirement$new("dummy"))[1], "requirement")
  expect_false(tmp$is_installed())
})

test_that("CRAN_Requirement works properly", {
  #totally valid, unversioned
  expect_equal(class(tmp <- cran_req$new("mgsub"))[1], "CRAN_Requirement")
  expect_equal(tmp$package, "mgsub")
  expect_equal(tmp$version, NA_character_)
  expect_true("1.5.0" %in% tmp$get_available_versions())
  #totally valid, versioned
  expect_equal(class(tmp <- cran_req$new("AGread >0.5"))[2], "requirement")
  expect_equal(tmp$package, "AGread")
  expect_equal(tmp$version, "> 0.5")
  expect_true("1.1.1" %in% tmp$get_available_versions())
  expect_false(tmp$is_installed())
  #totally valid, versioned
  expect_equal(class(tmp <- cran_req$new("AGread >0.5, <1.0"))[2], "requirement")
  expect_equal(tmp$package, "AGread")
  expect_length(tmp$version, 2)
  expect_true("1.1.1" %in% tmp$get_available_versions())
  expect_false(tmp$is_installed())

  #all failpoints
  expect_error(cran_req$new("AGread >0.5, >=1.2, >= 6.2"), regexp = "Too many versions")
  expect_error(cran_req$new("AGread >0.5, ==1.2"), regexp = "Illegal comparisons")
  expect_error(cran_req$new("AGread >0.5, ~=1.2"), regexp = "Illegal comparisons")
  expect_error(cran_req$new("AGread >>0.5"), regexp = "Illegal comparisons")
  expect_error(cran_req$new("2fun2quit"), regexp = "Illegal package name")
})
