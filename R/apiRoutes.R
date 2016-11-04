#' @import utils
#'
NULL

getProbandsPath <- function(studyId){
  paste("studies", studyId, "probands", sep = '/')
}

getUnisensPath <- function(studyXSId, probandXSId){
  paste(getProbandsPath(studyXSId), probandXSId,'unisens', sep = '/')
}

getMessagePathOfProband <- function(studyXSId, probandXSId, messageSendingUserEmail, textMessage){
  messageQueryString <- paste0('sendingUserEmail=', URLencode(messageSendingUserEmail, reserved = TRUE), "&textMessage=", URLencode(textMessage, reserved = TRUE))
  messagePath <- paste(getProbandsPath(studyXSId), as.numeric(probandXSId), "messages", sep='/')
  paste(messagePath, messageQueryString, sep="?")
}

getOverallResultsPath <- function(studyId){
  paste("studies", studyId, "results", sep='/')
}

getProbandsResultsPath <- function(studyId, probandId){
  paste(getProbandsPath(studyId), probandId, "results", sep='/')
}
