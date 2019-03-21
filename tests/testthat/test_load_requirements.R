context('load_requirements')

installed.packages <<- function(){return(INST)}

INST = structure(c("mgsub", "lexRankr", "readOffice",
                   "lib1", "lib1", "lib1",
                   "1.5.1.3", "0.4.1", "0.2.2"),
                 .Dim = c(3L, 3L),
                 .Dimnames = list(c("mgsub", "lexRankr", "readOffice"),
                                  c("Package", "LibPath", "Version")))
REQ = c("mgsub >= 1.5.0", "lexRankr == 0.4.*",
        "readOffice < 1", "whitechapelR > 0.3")
writeLines(text = REQ, con = 'testdata/requirements.txt')

test_that('Invalid Package Requirement', {
  expect_error(load_requirements('testdata',install = FALSE, packrat = FALSE,
                                 dryrun = TRUE, verbose = FALSE))
})

REQ = c("mgsub >= 1.5.0", "lexRankr == 0.4.*",
        "readOffice < 1", "whitechapelR >= 0.3")
writeLines(text = REQ, con = 'testdata/requirements.txt')

test_that('Install needed', {
  expect_output(tmp <- capture.output(
                load_requirements('testdata',install = TRUE, packrat = FALSE,
                                  dryrun = TRUE, verbose = TRUE));print(tmp),
                'mgsub is already installed with version 1\\.5\\.1\\.3')
})
