library(httr)
library(digest)
library(logging)
library(jsonlite)


sendMessageToProband = function(xsServerURL, studyXSId, probandXSId, messageSendingUserEmail, textMessage, apiKey){
  source('R/utils.R')
  source('R/config.R')
  getLogger('xs_adapter')
  loginfo(paste("Sending message to proband", paste(studyXSId, probandXSId, sep = " x "), ". Message:", textMessage), logger='xs_adapter')
  messageSendUrl <- paste(getXSAPIURL(xsServerURL), .getMessagePathOfProband(studyXSId, probandXSId, messageSendingUserEmail, textMessage), sep='/')
  authHeader <- auth(apiKey)
  resp <- POST(messageSendUrl, authHeader)
  loginfo(paste('Response', resp), logger='xs_adapter')
  probandsJson <- content(resp, as = "text", encoding = "UTF-8")
  logdebug(paste('Response body', probandsJson), logger='xs_adapter')
  if(nchar(probandsJson) > 0){
    resultingMessage <- fromJSON(probandsJson)
    loginfo(paste('Result:', paste(unlist(resultingMessage), collapse=',')), logger='xs_adapter')
    resultingMessage
  }
  else
    TRUE
}

.getMessagePathOfProband <- function(studyXSId, probandXSId, messageSendingUserEmail, textMessage){
  messageQueryString <- paste0('sendingUserEmail=', URLencode(messageSendingUserEmail, reserved = TRUE), "&textMessage=", URLencode(textMessage, reserved = TRUE))
  messagePath <- paste(.getProbandsPath(studyXSId), as.numeric(probandXSId), "messages", sep='/')
  paste(messagePath, messageQueryString, sep="?")
}
