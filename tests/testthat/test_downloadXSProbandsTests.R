
source('_test.config.R')

test_that('downloadProbandTable_correctRequestParameters_validResult', {
  # build
  # operate
  result <- downloadProbands(test.conf.serverURL, test.conf.studyId, test.conf.apikey)

  #check
  expected_result <- readRDS('data/testProbandResult.rds')
  expect_equal(result, expected_result)
})

test_that('downloadProbandTable_studyNotExistent_error', {
  # build
  errorOccurred <- NULL

  # operate
  nonExistingStudy <- 'invalid_id'
  tryCatch({
    downloadProbands(test.conf.serverURL, nonExistingStudy, test.conf.apikey)
  }, error = function(e){
    errorOccurred <<- e
  })

  #check
  expect_equivalent(errorOccurred$message, xsExceptions['notFound'])
})

test_that('downloadProbandTable_wrongAPIKey_error', {
  # build
  errorOccurred <- NULL

  # operate
  wrongAPIKey <- 'aGuessedKey'
  tryCatch({
    downloadProbands(test.conf.serverURL, test.conf.studyId, wrongAPIKey)
  }, error = function(e){
    errorOccurred <<- e
  })

  #check
  expect_equivalent(errorOccurred$message, xsExceptions['invalidAPIKey'])
})
