context("read_process_req_file.R")

test_that("read_requirements_file", {
  expect_is(read_requirements_file("testdata/requirements_1.txt"), "data.frame")
  expect_equal(read_requirements_file("testdata/requirements_1.txt"),
               data.frame(content = c("# to test a mix of possible and impossible",
                                      "mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice < 1.0",
                                      "whitechapelR >= 0.3"),
                          line = 1:5,
                          file = c("testdata/requirements_1.txt", "testdata/requirements_1.txt",
                                   "testdata/requirements_1.txt", "testdata/requirements_1.txt",
                                   "testdata/requirements_1.txt"),
                          stringsAsFactors = FALSE)
               )
  expect_equal(read_requirements_file("testdata/requirements_3.txt"),
               data.frame(content = c("# to test other forms of requirements",
                                      "-r testdata/requirements_2.txt", "tidyr",
                                      "git+https://github.com/bmewing/requirements",
                                      "svn+ssh://developername@svn.r-forge.r-project.org/svnroot/robast/",
                                      "bioc+release/SummarizedExperiment",
                                      "https://github.com/bmewing/mgsub/releases/download/v.1.5/mgsub_1.5.0.tar.gz",
                                      "testdata/dummy_package.zip", "# to test comlete set of comparison operators",
                                      "mgsub >= 1.5.0", "lexRankr == 0.4.* #test inline comment",
                                      "readOffice ~= 0.0  ",
                                      "whitechapelR >= 0.3", "dplyr != 0.7.*"),
                          line = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L, 6L),
                          file = c("testdata/requirements_3.txt", "testdata/requirements_3.txt",
                                   "testdata/requirements_3.txt", "testdata/requirements_3.txt",
                                   "testdata/requirements_3.txt", "testdata/requirements_3.txt",
                                   "testdata/requirements_3.txt", "testdata/requirements_3.txt",
                                   "testdata/requirements_2.txt", "testdata/requirements_2.txt",
                                   "testdata/requirements_2.txt", "testdata/requirements_2.txt",
                                   "testdata/requirements_2.txt", "testdata/requirements_2.txt"),
                          stringsAsFactors = FALSE)
               )
  expect_equal(read_requirements_file("testdata/requirements_7.txt"),
               data.frame(content = c("# testing blank lines and conflicting requirements",
                                      "", "readOffice > 1.0", "", "readOffice == 1", "",
                                      "-r testdata/requirements_7b.txt",
                                      "# testing blank lines and conflicting requirements",
                                      "", "readOffice < 1.0", "", "mgsub == 1", ""),
                          line = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 1L, 2L, 3L, 4L, 5L, 6L),
                          file = c("testdata/requirements_7.txt", "testdata/requirements_7.txt",
                                   "testdata/requirements_7.txt", "testdata/requirements_7.txt",
                                   "testdata/requirements_7.txt", "testdata/requirements_7.txt",
                                   "testdata/requirements_7.txt", "testdata/requirements_7b.txt",
                                   "testdata/requirements_7b.txt", "testdata/requirements_7b.txt",
                                   "testdata/requirements_7b.txt", "testdata/requirements_7b.txt",
                                   "testdata/requirements_7b.txt"),
                          stringsAsFactors = FALSE)
               )
  expect_error(read_requirements_file("testdata/requirements_4.txt"),
               regexp = "is empty")
  expect_error(read_requirements_file("testdata/requirements_fake.txt"),
               regexp = sprintf(REQ_FILE_EXIST_ERR, "testdata/requirements_fake.txt"))
})

test_that("process_requirements_file", {
  expect_error(process_requirements_file("testdata/requirements_0.txt"),
               sprintf(REQ_FILE_INVALID_COMP, "lexRankr = 0.4.*", "=", "=="))
  expect_type(req1 <- process_requirements_file("testdata/requirements_1.txt"), "list") # nolint
  expect_length(process_requirements_file("testdata/requirements_1.txt"), 7)
  expect_length(process_requirements_file("testdata/requirements_1.txt")[["url"]], 0)
  expect_equal(process_requirements_file("testdata/requirements_1.txt")[["versioned"]],
               c("mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice < 1.0", "whitechapelR >= 0.3"))
  expect_equal(process_requirements_file("testdata/requirements_2.txt")[["versioned"]],
               c("mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice ~= 0.0", "whitechapelR >= 0.3",
                 "dplyr != 0.7.*"))
  expect_equal(process_requirements_file("testdata/requirements_3.txt")[["versioned"]],
               c("mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice ~= 0.0", "whitechapelR >= 0.3",
                 "dplyr != 0.7.*"))
  expect_length(process_requirements_file("testdata/requirements_3.txt")[["unversioned"]], 1)
  expect_length(process_requirements_file("testdata/requirements_3.txt")[["git"]], 1)
  expect_length(process_requirements_file("testdata/requirements_3.txt")[["svn"]], 1)
  expect_length(process_requirements_file("testdata/requirements_3.txt")[["bioc"]], 1)
  expect_length(process_requirements_file("testdata/requirements_3.txt")[["local"]], 1)
  expect_length(process_requirements_file("testdata/requirements_3.txt")[["url"]], 1)
  expect_error(process_requirements_file("testdata/requirements_4_wc.txt"),
               regexp = sprintf(REQ_FILE_EMPTY_ERR, "testdata/requirements_4_wc.txt"))
  expect_error(process_requirements_file("testdata/requirements_6.txt"),
               regexp = sprintf(REQ_FILE_RESOLUTION_ERR, paste("dummy_package", collapse = ", ")))
})
# TODO: Add more tests here
test_that("remove_comments_from_req", {
  expect_equal(
    remove_comments_from_req(
      data.frame(content = c("mgsub >= 1.5", "-r extra_reqs.txt", "#What a world", "## We live in!"),
                 line = 1:4,
                 file = "requirements.txt",
                 stringsAsFactors = FALSE)
      ), c("mgsub >= 1.5")
  )
  expect_equal(
    remove_comments_from_req(
      data.frame(content = c("mgsub >= 1.5", "-r extra_reqs.txt #inline comment",
                           "", "#What a world", "## We live in!", "   "),
                 line = 1:6,
                 file = "requirements.txt",
                 stringsAsFactors = FALSE)
    ), c("mgsub >= 1.5")
  )
  expect_error(
    remove_comments_from_req(
      data.frame(content = c("mgsub >= 1.5", "-r extra_reqs.txt #inline comment",
                           "", "#What a world", "## We live in!", "   ",
                           " mgsub"),
                 line = 1:7,
                 file = "requirements.txt",
                 stringsAsFactors = FALSE)
    ), regex = "Double requirement given"
  )
  expect_equal(tmp <<- remove_comments_from_req(read_requirements_file("testdata/requirements_3.txt")), #nolint
               c("tidyr", "git+https://github.com/bmewing/requirements",
                 "svn+ssh://developername@svn.r-forge.r-project.org/svnroot/robast/",
                 "bioc+release/SummarizedExperiment",
                 "https://github.com/bmewing/mgsub/releases/download/v.1.5/mgsub_1.5.0.tar.gz",
                 "testdata/dummy_package.zip", "mgsub >= 1.5.0", "lexRankr == 0.4.*",
                 "readOffice ~= 0.0", "whitechapelR >= 0.3", "dplyr != 0.7.*"))
})

test_that("capture_special_installs", {
  expect_equal(capture_special_installs(tmp),
               list(git = "git+https://github.com/bmewing/requirements",
                    svn = "svn+ssh://developername@svn.r-forge.r-project.org/svnroot/robast/",
                    bioc = "bioc+release/SummarizedExperiment",
                    url = "https://github.com/bmewing/mgsub/releases/download/v.1.5/mgsub_1.5.0.tar.gz"))
})

test_that("capture_versioned_requirements", {
  expect_equal(capture_versioned_requirements(tmp),
               c("mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice ~= 0.0", "whitechapelR >= 0.3", "dplyr != 0.7.*"))
})

test_that("capture_local_requirements", {
  expect_equal(capture_local_requirements(tmp),
               c("testdata/dummy_package.zip"))
})

test_that("validate_versioning", {
  expect_true(validate_versioning("mgsub >= 1.4"))
  expect_true(validate_versioning("mgsub> 1.4"))
  expect_true(validate_versioning("mgsub <=1.4"))
  expect_true(validate_versioning("mgsub<1.4"))
  expect_true(validate_versioning("mgsub==1.4"))
  expect_false(validate_versioning("mgsub=1.4"))
  expect_true(validate_versioning("mgsub!=1.4"))
  expect_true(validate_versioning("mgsub~=1.4"))
  expect_true(validate_versioning("mgsub==1.*"))
  expect_true(validate_versioning("mgsub==1.*.1.0.15.*"))
  expect_true(validate_versioning("mgsub==994.*.181.0.15.*"))
  expect_false(validate_versioning("mgsub>>1.4"))
  expect_false(validate_versioning("mgsub=*=1.4"))
  expect_true(validate_versioning("mgsub>=1"))
  expect_false(validate_versioning("mgsub>"))
  expect_false(validate_versioning("mgsub"))
  expect_false(validate_versioning("mgsub>=1.4..5"))
})
