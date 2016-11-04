library(testthat)

setwd('../..')

source('tests/testthat/_test.config.R')

.checkSingleProbandResult <- function(result){
  expect_gt(nrow(result), 1)
  expectedColumns <- c('Participant', 'Device', 'Trigger', 'Trigger_counter', 'Form', 'Form_start_date', 'Form_finish_date', 'Form_upload_date', 'Missing')
  expectedTbl <- readRDS('tests/testthat/data/testSubjectiveResult.rds')
  expect_equal(result, expectedTbl)
}

test_that('downloadSubjectiveDataAsJson_correctRequestParameters_validResult', {
  # build
  # operate
  result <- downloadSubjectiveDataAsJson(test.conf.serverURL, test.conf.studyId, test.conf.probandId, test.conf.apikey)

  #check
  .checkSingleProbandResult(result)
})

.checkOverallProbandsResult <- function(result){
  # is actually the same so far
  .checkSingleProbandResult(result)
}

test_that('downloadOverallSubjectiveDataAsJson_correctRequestParameters_validResult', {
  # build
  # operate
  result <- downloadOverallSubjectiveDataAsJson(test.conf.serverURL, test.conf.studyId, test.conf.apikey)

  #check
  .checkOverallProbandsResult(result)
})

test_that('downloadSubjectiveDataAsJson_wrongAPIKey_error', {
  # build
  errorOccurred <- NULL

  # operate
  wrongAPIKey <- 'aGuessedKey'
  tryCatch({
    downloadSubjectiveDataAsJson(test.conf.serverURL, test.conf.studyId, test.conf.probandId, wrongAPIKey)
  }, error = function(e){
    errorOccurred <<- e
  })

  #check
  expect_equivalent(errorOccurred$message, xsExceptions['invalidAPIKey'])
})

test_that('downloadSubjectiveDataAsJson_probandNotExistent_error', {
  # build
  errorOccurred <- NULL

  # operate
  nonExistingProband <- 10000
  tryCatch({
    downloadSubjectiveDataAsJson(test.conf.serverURL, test.conf.studyId, nonExistingProband, test.conf.apikey)
  }, error = function(e){
    errorOccurred <<- e
  })

  #check
  expect_equivalent(errorOccurred$message, xsExceptions['notFound'])
})

test_that('downloadSubjectiveDataAsJson_studyNotExistent_error', {
  # build
  errorOccurred <- NULL

  # operate
  nonExistingStudy <- 'invalid id'
  tryCatch({
    downloadSubjectiveDataAsJson(test.conf.serverURL, nonExistingStudy, test.conf.probandId, test.conf.apikey)
  }, error = function(e){
    errorOccurred <<- e
  })

  #check
  expect_equivalent(errorOccurred$message, xsExceptions['notFound'])
})

test_that('downloadSubjectiveDataAsXLSX_correctRequestParameters_validResult', {
  # build
  # operate
  resultFile <- downloadSubjectiveDataAsXLSX(test.conf.serverURL, test.conf.studyId, test.conf.probandId, test.conf.apikey)
  #check
  expect_false(is.na(file.info(resultFile)[['size']]))
})

test_that('downloadOverallSubjectiveDataAsXLSX_correctRequestParameters_validResult', {
  # build
  # operate
  resultFile <- downloadOverallSubjectiveDataAsXLSX(test.conf.serverURL, test.conf.studyId, test.conf.apikey)
  #check
  expect_false(is.na(file.info(resultFile)[['size']]))
})
