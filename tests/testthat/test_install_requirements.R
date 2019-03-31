context('general_helpers')

test_that('read_requirements_file', {
  expect_type(read_requirements_file('testdata/requirements_1.txt'),'character')
  expect_equal(read_requirements_file('testdata/requirements_1.txt'), 
               c("mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice < 1", 
                 "whitechapelR >= 0.3"))
  expect_equal(read_requirements_file('testdata/requirements_2.txt'), 
               c("mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice ~= 0", 
                 "whitechapelR >= 0.3", "dplyr != 0.7.*"))
  expect_equal(read_requirements_file('testdata/requirements_3.txt'), 
               c("mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice ~= 0", "whitechapelR < 0.4", 
                 "dplyr != 0.7.*", "tidyr", "git+https://github.com/bmewing/requirements", 
                 "svn+ssh://developername@svn.r-forge.r-project.org/svnroot/robast/", 
                 "bioc+release/SummarizedExperiment", 
                 "https://github.com/bmewing/mgsub/releases/download/v.1.5/mgsub_1.5.0.tar.gz", 
                 "testdata/dummy_package.zip"))
})

test_that('process_requirements_file',{
  expect_type(req1 <- process_requirements_file('testdata/requirements_1.txt'),'list')
  expect_length(process_requirements_file('testdata/requirements_1.txt'), 7)
  expect_length(process_requirements_file('testdata/requirements_1.txt')[['url']], 0)
  expect_equal(process_requirements_file('testdata/requirements_1.txt')[['versioned']],
               c("mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice < 1", "whitechapelR >= 0.3"))
  expect_equal(process_requirements_file('testdata/requirements_2.txt')[['versioned']],
               c("mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice ~= 0", "whitechapelR >= 0.3", 
                 "dplyr != 0.7.*"))
  expect_equal(process_requirements_file('testdata/requirements_3.txt')[['versioned']],
               c("mgsub >= 1.5.0", "lexRankr == 0.4.*", "readOffice ~= 0", "whitechapelR < 0.4", 
                 "dplyr != 0.7.*"))
  expect_length(process_requirements_file('testdata/requirements_3.txt')[['unversioned']],1)
  expect_length(process_requirements_file('testdata/requirements_3.txt')[['git']],1)
  expect_length(process_requirements_file('testdata/requirements_3.txt')[['svn']],1)
  expect_length(process_requirements_file('testdata/requirements_3.txt')[['bioc']],1)
  expect_length(process_requirements_file('testdata/requirements_3.txt')[['local']],1)
  expect_length(process_requirements_file('testdata/requirements_3.txt')[['url']],1)
})

test_that('compare_version',{ #existing, target, comp
  expect_true(compare_version('12','9','>')) #semantic version 12 should be greater than requested 9
  expect_true(compare_version('9','12','<')) #semantic version 9 should be less than requested 12
  expect_true(compare_version('12a','12','>')) #semantic version 12a should be greater than requested 12
  expect_true(compare_version('1c','11','<')) #semantic version 1c should be less than requested 11
})

test_that('check_version',{ #target, existing, comp
  expect_true(check_version('1.0.1', '1.0.1', '=='))
  expect_true(check_version('1',     '1.0.1', '~='))
  expect_true(check_version('1.*',   '1.0.1', '=='))
  expect_true(check_version('1.0.1', '1.0.1', '>='))
  expect_true(check_version('1.*',   '1.0.1', '>='))
  expect_true(check_version('0.9',   '1.0.1', '>' ))
  expect_true(check_version('1.*',   '1.0.1', '<='))
  expect_true(check_version('1.5',   '1.0.1', '<' ))
  expect_true(check_version('1.5',   '1.0.1', '!='))
  expect_true(check_version('2.*',   '1.0.1', '!='))
  expect_true(check_version('*.0.2', '1.0.1', '!='))
  
  expect_false(check_version('1.0.1', '1.0.2', '=='))
  expect_false(check_version('1',     '2.0.1', '~='))
  expect_false(check_version('1.*',   '2.0.1', '=='))
  expect_false(check_version('1.0.1', '0.0.1', '>='))
  expect_false(check_version('1.*',   '0.0.1', '>='))
  expect_false(check_version('0.9',   '0.9.0', '>' ))
  expect_false(check_version('1.*',   '2.0.1', '<='))
  expect_false(check_version('1.5',   '2.0.1', '<' ))
  expect_false(check_version('1.5',   '1.5.0', '!='))
  expect_false(check_version('2.*',   '2.1.0', '!='))
  expect_false(check_version('*.0.2', '1.0.2', '!='))
})


context('install_requirements')

TMP_DIR = tempdir()
writeLines(c('library(mgsub)','require(lexRankr)'),file.path(TMP_DIR,'dummy.R'))
INST = c("mgsub"="1.5.1.3","lexRankr"="0.4.1","readOffice"="0.2.2")

test_that('Invalid Package Requirement', {
  expect_error(install_reqs(reqs = req1,
                            dryrun = TRUE, verbose = FALSE,
                            dummy = INST))
})

test_that('Getting available package versions', {
  expect_equal(get_available_versions('mgsub')[1:4],
               c("1.0.0", "1.5.0", "1.7.0", "1.7.1"))
  expect_equal(get_available_versions('lexRankr')[1:7],
               c("0.1.0", "0.2.0", "0.3.0", "0.4.0", "0.4.1", "0.5.0", "0.5.2"))
  expect_equal(get_available_versions('dplyr')[1:24],
               c("0.1", "0.1.1", "0.1.2", "0.1.3", "0.2", "0.3", "0.3.0.1", 
                 "0.3.0.2", "0.4.0", "0.4.1", "0.4.2", "0.4.3", "0.5.0", "0.7.0", 
                 "0.7.1", "0.7.2", "0.7.3", "0.7.4", "0.7.5", "0.7.6", "0.7.7", 
                 "0.7.8", "0.8.0", "0.8.0.1"))
  expect_equal(get_available_versions('readOffice')[1],c("0.2.2"))
  expect_null(get_available_versions('lexrankR'))

})
