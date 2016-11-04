library(assertthat)

setwd('../..')

source('tests/testthat/_test.config.R')

test_that('integrationalTestWithMovisensXSServer_sendMessageToProband_serverAvailable_messageSent', {
  # build
  # operate
  testMessage <- 'Hallo!'
  result <- sendMessageToProband(test.conf.serverURL, test.conf.studyId, test.conf.probandId, test.conf.messageSendingUserEmail, testMessage, test.conf.apikey)

  # check
  expected_result <- readRDS('tests/testthat/data/testMessageResult.rds')
  resultWithoutSendDate <- expected_result[c('id', 'message', 'messageRead', 'fromProband', 'sendingUserEmail')]
  expectedResultWithoutSendDate <- expected_result[c('id', 'message', 'messageRead', 'fromProband', 'sendingUserEmail')]
  expect_equal(resultWithoutSendDate, expectedResultWithoutSendDate)
})

test_that('downloadSubjectiveData_wrongAPIKey_error', {
  # build
  errorOccurred <- NULL

  # operate
  wrongAPIKey <- 'aGuessedKey'
  testMessage <- 'Hallo!'
  tryCatch({
      sendMessageToProband(test.conf.serverURL, test.conf.studyId, test.conf.probandId, test.conf.messageSendingUserEmail, testMessage, wrongAPIKey)
  }, error = function(e){
    errorOccurred <<- e
  })

  #check
  expect_equivalent(errorOccurred$message, xsExceptions['invalidAPIKey'])
})

test_that('downloadSubjectiveData_nonExistentProbandId_error', {
  # build
  errorOccurred <- NULL

  # operate
  testMessage <- 'Hallo!'
  wrongProbandId <- 10000
  tryCatch({
    sendMessageToProband(test.conf.serverURL, test.conf.studyId, wrongProbandId, test.conf.messageSendingUserEmail, testMessage, test.conf.apikey)
  }, error = function(e){
    errorOccurred <<- e
  })

  #check
  expect_equivalent(errorOccurred$message, xsExceptions['notFound'])
})

test_that('downloadSubjectiveData_nonExistentStudyId_error', {
  # build
  errorOccurred <- NULL

  # operate
  testMessage <- 'Hallo!'
  wrongStudyId <- 10000
  tryCatch({
    sendMessageToProband(test.conf.serverURL, wrongStudyId, test.conf.probandId, test.conf.messageSendingUserEmail, testMessage, test.conf.apikey)
  }, error = function(e){
    errorOccurred <<- e
  })

  #check
  expect_equivalent(errorOccurred$message, xsExceptions['notFound'])
})
