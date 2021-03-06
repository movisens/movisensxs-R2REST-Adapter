#' @import jsonlite
#' @import httr
#' @import logging
#'
NULL

adapter.conf.xsMessagingColumns <- c('id', 'creationDate', 'message', 'messageRead', 'fromProband', 'sendingUserEmail')

#' Send a message to a proband
#'
#' @param xsServerURL the address of the xs server
#' @param studyXSId the study xs-id
#' @param probandXSId the proband xs-id
#' @param messageSendingUserEmail xs-user that is mentioned as sender
#' @param textMessage the message content
#' @param apiKey the api-key of the study
#' @return Feedback from the xs server or exception
#' @export
#'
sendMessageToProband = function(xsServerURL, studyXSId, probandXSId, messageSendingUserEmail, textMessage, apiKey){
  getLogger('xs_adapter')
  loginfo(paste("Sending message to proband", paste(studyXSId, probandXSId, sep = " x "), ". Message:", textMessage), logger='xs_adapter')
  messageSendUrl <- paste(getXSAPIURL(xsServerURL), getMessagePathOfProband(studyXSId, probandXSId, messageSendingUserEmail, textMessage), sep='/')
  logdebug(paste("Calling XS-API operation:", messageSendUrl), logger='xs_adapter')
  headers <- add_headers(authHeader(apiKey), acceptHeader(jsonMIME))
  response <- POST(messageSendUrl, headers)
  loginfo(paste('Received response code to xs server request:', response$status_code), logger='xs_adapter')
  logdebug(paste('Response', response), logger='xs_adapter')
  .extractResultFromRequestMsg(response)
}

.extractResultFromRequestMsg <- function(response){
  if(response$status_code == 404)
    stop(xsExceptions['notFound'])
  else if(response$status_code == 401)
    stop(xsExceptions['invalidAPIKey'])
  else if(.hasContentType(response, jsonMIME))
    .parseResponseMsg(response)
  else
    stop('The xs server responded with a request of an unknown type.')
}

.parseResponseMsg <- function(response){
  respBody <- content(response, as = "text", encoding = "UTF-8")
  logdebug(paste('Response body', respBody), logger='xs_adapter')
  sendResult <- fromJSON_(respBody)
  if(is.null(sendResult[["error"]])){
    completedResults <- completeMissingColumns(sendResult, adapter.conf.xsMessagingColumns)
    results <- .parseDateColumnsXSMsg(completedResults)
    loginfo(paste('Result:', paste(unlist(results), collapse=',')), logger='xs_adapter')
    results
  } else
    stop(paste("Error sending a message to a proband:", sendResult[1,'error']))
}

.parseDateColumnsXSMsg <- function(resultTbl){
  dateCols <- c('creationDate')
  sapply(dateCols, function(dateCol){
    resultTbl[dateCol] <<- parseMessageDates(resultTbl[[dateCol]])
  })
  resultTbl
}
