library(assertthat)

source('R/test/test.config.R')
source('R/xsProbands.R')

integrationalTestWithMovisensXSServer_downloadProbands_serverAvailable_probandTable <- function(){
  # build
  # operate
  result <- downloadProbands(test.conf.serverURL, test.conf.studyId, test.conf.apikey)

  #check
  assert_that(length(result[['id']]) > 0)
  assert_that(length(adapter.conf.probandColumns) == ncol(result))
  assert_that(all(adapter.conf.probandColumns %in% names(result)))
}

allXSDownProband_Tests <- function(){
  integrationalTestWithMovisensXSServer_downloadProbands_serverAvailable_probandTable()
}
