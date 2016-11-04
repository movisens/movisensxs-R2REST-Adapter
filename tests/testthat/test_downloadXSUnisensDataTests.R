library(testthat)

setwd('../..')

source('tests/testthat/_test.config.R')

test_that('downloadUnisensData_downloadUnisensData_unisensFiles', {
  # build
  # operate
  result <- downloadUnisensData(test.conf.serverURL, test.conf.studyId, test.conf.probandId, test.conf.apikey)

  #check
  expectedResult <- c(unisensFolder = .getUnisensFolder(), "Location.csv", "unisens.xml")
  expect_equal(result, expectedResult)
  dir <- result[1]
  file1 <- paste(dir, result[2], sep='/')
  file2 <- paste(dir, result[3], sep='/')
  expect_false(is.na(file.info(file1)[['size']]))
  expect_false(is.na(file.info(file2)[['size']]))
})

test_that('downloadUnisensData_wrongAPIKey_error', {
  # build
  errorOccurred <- NULL

  # operate
  wrongAPIKey <- 'aGuessedKey'
  tryCatch({
    downloadUnisensData(test.conf.serverURL, test.conf.studyId, test.conf.probandId, wrongAPIKey)
  }, error = function(e){
    errorOccurred <<- e
  })

  #check
  expect_equivalent(errorOccurred$message, xsExceptions['invalidAPIKey'])
})

test_that('downloadSubjectiveData_probandNotExistent_error', {
  # build
  errorOccurred <- NULL

  # operate
  nonExistingProband <- 10000
  tryCatch({
    downloadUnisensData(test.conf.serverURL, test.conf.studyId, nonExistingProband, test.conf.apikey)
  }, error = function(e){
    errorOccurred <<- e
  })

  #check
  expect_equivalent(errorOccurred$message, xsExceptions['notFound'])
})

test_that('downloadUnisensData_studyNotExistent_error', {
  # build
  errorOccurred <- NULL

  # operate
  nonExistingStudy <- 'invalid id'
  tryCatch({
    downloadUnisensData(test.conf.serverURL, nonExistingStudy, test.conf.probandId, test.conf.apikey)
  }, error = function(e){
    errorOccurred <<- e
  })

  #check
  expect_equivalent(errorOccurred$message, xsExceptions['notFound'])
})
