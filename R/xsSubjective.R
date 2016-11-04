library(jsonlite)
library(httr)
library(digest)
library(logging)


adapter.conf.xsSubjectiveColumns <- c('Participant', 'Trigger', 'Trigger_date', 'Trigger_counter', 'Form', 'Form_start_date', 'Form_finish_date', 'Form_upload_date', 'Missing')

#' Download all subjective data of a study in one request
#' @param xsServerURL the address of the xs server
#' @param studyId the xs-id of the study
#' @param apiKey the secret api-key of the study
#' @export
downloadOverallSubjectiveDataAsJson <- function(xsServerURL, studyId, apiKey){
  getLogger('xs_adapter')
  operationCallPath <- paste(getXSAPIURL(xsServerURL), getOverallResultsPath(studyId), sep='/')
  .callSubjectiveResultsRESTAPIOperation(operationCallPath, apiKey, acceptHeader(jsonMIME))
}

.callSubjectiveResultsRESTAPIOperation <- function(operationCallPath, apiKey, acceptHeader){
  getLogger('xs_adapter')
  headers <- add_headers(authHeader(apiKey), acceptHeader)
  loginfo(paste("Downloading Forms by URL:", operationCallPath), logger='xs_adapter')
  response <- GET(operationCallPath, headers)
  loginfo(paste('Response:', response), logger='xs_adapter')
  .extractResultFromRequestSubj(response)
}

.extractResultFromRequestSubj <- function(response){
  if(response$status_code == 404)
    stop(xsExceptions['notFound'])
  else if(response$status_code == 401)
    stop(xsExceptions['invalidAPIKey'])
  else if(.hasContentType(response, acceptHeader(jsonMIME)))
    .parseJsonRequestSubj(response)
  else if(.hasContentType(response, acceptHeader(xlsxMIME)))
    .extractXLSXRequestSubj(response)
  else
    stop('The xs server responded with a request of an unknown type.')
}

.parseJsonRequestSubj <- function(response){
  resultsJson <- content(response, as = "text", encoding = "UTF-8")
  logdebug(paste('Response body:', resultsJson), logger='xs_adapter')
  results <- data.frame(fromJSON_(resultsJson))
  logdebug(paste('downloadSubjectiveResultsAsJson:', results), logger='xs_adapter')
  if(is.null(results[["error"]])){
    completedResults <- completeMissingColumns(results, adapter.conf.xsSubjectiveColumns)
    results <- .parseDateColumnsXSSubj(completedResults)
  } else
    stop(paste("Error downloading subjective data as json:", results))
  results
}

.parseDateColumnsXSSubj <- function(resultTbl){
  dateCols <- c('Trigger_date', 'Form_start_date', 'Form_finish_date', 'Form_upload_date')
  sapply(dateCols, function(dateCol){
    resultTbl[dateCol] <<- parseSubjFormDates(resultTbl[[dateCol]])
  })
  resultTbl
}

.extractXLSXRequestSubj <- function(response){
  resultFile <- extractXLSXFile(response)
  logdebug(paste('Response body:', resultFile), logger='xs_adapter')
  resultFile
}

#' Download all subjective data of a study's proband
#' @param xsServerURL the address of the xs server
#' @param studyId the xs-id of the study
#' @param probandId the xs-id of the proband
#' @param apiKey the secret api-key of the study
#' @export
downloadSubjectiveDataAsJson <- function(xsServerURL, studyId, probandId, apiKey){
  getLogger('xs_adapter')
  operationCallPath <- paste(getXSAPIURL(xsServerURL), getProbandsResultsPath(studyId, probandId), sep='/')
  .callSubjectiveResultsRESTAPIOperation(operationCallPath, apiKey, acceptHeader(jsonMIME))
}

#' Download all subjective data of a study in one xlsx-file
#' @param xsServerURL the address of the xs server
#' @param studyId the xs-id of the study
#' @param the secret api-key of the study
#' @export
downloadOverallSubjectiveDataAsXLSX <- function(xsServerURL, studyId, apiKey){
  getLogger('xs_adapter')
  operationCallPath <- paste(getXSAPIURL(xsServerURL), getOverallResultsPath(studyId), sep='/')
  .callSubjectiveResultsRESTAPIOperation(operationCallPath, apiKey, acceptHeader(xlsxMIME))
}

#' Download all subjective data of a study's proband in one xlsx-file
#' @param xsServerURL the address of the xs server
#' @param studyId the xs-id of the study
#' @param probandId the xs-id of the proband
#' @param apiKey the secret api-key of the study
#' @export
downloadSubjectiveDataAsXLSX <- function(xsServerURL, studyId, probandId, apiKey){
  getLogger('xs_adapter')
  operationCallPath <- paste(getXSAPIURL(xsServerURL), getProbandsResultsPath(studyId, probandId), sep='/')
  .callSubjectiveResultsRESTAPIOperation(operationCallPath, apiKey, acceptHeader(xlsxMIME))
}
