library(assertthat)

source('R/test/test.config.R')
source('R/xsUnisens.R')

integrationalTestWithMovisensXSServer_downloadUnisensData <- function(){
  # build
  # operate
  result <- downloadUnisensData(test.conf.serverURL, test.conf.studyId, test.conf.probandId, test.conf.apikey)

  #check
  assert_that(length(result) == 3 + 1)
}

allXSDownloadUnisensDataTests <- function(){
  integrationalTestWithMovisensXSServer_downloadUnisensData()
}
