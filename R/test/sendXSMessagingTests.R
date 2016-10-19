library(assertthat)

source('R/test/test.config.R')
source('R/xsProbands.R')

integrationalTestWithMovisensXSServer_sendMessageToProband_serverAvailable_messageSent <- function(){
  # build
  # operate
  testMessage <- 'Hallo!'
  result <- sendMessageToProband(test.conf.serverURL, test.conf.studyId, test.conf.probandId, test.conf.messageSendingUserEmail, testMessage, test.conf.apikey)
  print(result)
  #check manually
}

allXSDownProband_Tests <- function(){
  integrationalTestWithMovisensXSServer_downloadProbands_serverAvailable_probandTable()
}
