context("read_process_req_file.R")

test_that("read_requirements_file", {
  expect_type(read_requirements_file("testdata/requirements_1.txt"), "character")
  expect_equal(read_requirements_file("testdata/requirements_1.txt"),
               c("# to test a mix of possible and impossible",
                 "mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice < 1.0",
                 "whitechapelR >= 0.3"))
  expect_equal(read_requirements_file("testdata/requirements_2.txt"),
               c("# to test comlete set of comparison operators",
                 "mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice ~= 0.0",
                 "whitechapelR >= 0.3", "dplyr != 0.7.*"))
  expect_equal(read_requirements_file("testdata/requirements_3.txt"),
               c("# to test other forms of requirements",
                 "-r testdata/requirements_2.txt", "tidyr",
                 "git+https://github.com/bmewing/requirements",
                 "svn+ssh://developername@svn.r-forge.r-project.org/svnroot/robast/",
                 "bioc+release/SummarizedExperiment",
                 "https://github.com/bmewing/mgsub/releases/download/v.1.5/mgsub_1.5.0.tar.gz",
                 "testdata/dummy_package.zip", "# to test comlete set of comparison operators",
                 "mgsub >= 1.5.0", "lexRankr == 0.4.*",
                 "readOffice ~= 0.0", "whitechapelR >= 0.3", "dplyr != 0.7.*"))
  expect_error(read_requirements_file("testdata/requirements_4.txt"),
               regexp = "is empty")
  expect_error(read_requirements_file("testdata/requirements_fake.txt"),
               regexp = sprintf(REQ_FILE_EXIST_ERR, "testdata/requirements_fake.txt"))
})

test_that("process_requirements_file", {
  expect_type(req1 <- process_requirements_file("testdata/requirements_1.txt"), "list")
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

test_that("remove_comments_from_req", {
  expect_equal(remove_comments_from_req(c("mgsub >= 1.5", "-r extra_reqs.txt", "#What a world", "## We live in!")),
               c("mgsub >= 1.5"))
})

test_that("capture_special_installs", {
  expect_equal(capture_special_installs(read_requirements_file("testdata/requirements_3.txt")),
               list(git = "git+https://github.com/bmewing/requirements",
                    svn = "svn+ssh://developername@svn.r-forge.r-project.org/svnroot/robast/",
                    bioc = "bioc+release/SummarizedExperiment",
                    url = "https://github.com/bmewing/mgsub/releases/download/v.1.5/mgsub_1.5.0.tar.gz"))
})

test_that("capture_versioned_requirements", {
  expect_equal(capture_versioned_requirements(read_requirements_file("testdata/requirements_3.txt")),
               c("mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice ~= 0.0", "whitechapelR >= 0.3", "dplyr != 0.7.*"))
})

test_that("capture_local_requirements", {
  expect_equal(capture_local_requirements(read_requirements_file("testdata/requirements_3.txt")),
               c("testdata/dummy_package.zip"))
})
