library(assertthat)

source('R/test/test.config.R')
source('R/xsSubjective.R')

integrationalTestWithMovisensXSServer_downloadSubjectiveData <- function(){
  # build
  # operate
  result <- downloadSubjectiveDataAsJson(test.conf.serverURL, test.conf.studyId, test.conf.probandId, test.conf.apikey)

  #check
  assert_that(nrow(result) > 1)
  expectedColumns <- c('Participant', 'Device', 'Trigger', 'Trigger_counter', 'Form', 'Form_start_date', 'Form_finish_date', 'Form_upload_date', 'Missing')
  assert_that(all(sapply(expectedColumns, function(colName){length(which(names(result) == colName) == 1)})))
}

allXSDownloadSubjectiveDataTests <- function(){
  integrationalTestWithMovisensXSServer_downloadSubjectiveData()
}
