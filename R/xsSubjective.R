library(jsonlite)
library(httr)
library(digest)
library(logging)

source('R/utils.R')
source('R/config.R')

downloadOverallSubjectiveDataAsJson <- function(xsServerURL, studyId, apiKey){
  getLogger('xs_adapter')
  operationCallPath <- paste(getXSAPIURL(xsServerURL), .getOverallResultsPath(studyId), sep='/')
  callSubjectiveResultsRESTAPIOperation(operationCallPath, apiKey)
}

.getOverallResultsPath <- function(studyId){paste("studies", studyId, "results", sep='/')}

downloadSubjectiveDataAsJson <- function(xsServerURL, studyId, probandId, apiKey){
  getLogger('xs_adapter')
  operationCallPath <- paste(getXSAPIURL(xsServerURL), .getProbandsResultsPath(studyId, probandId), sep='/')
  callSubjectiveResultsRESTAPIOperation(operationCallPath, apiKey)
}

.getProbandsResultsPath <- function(studyId, probandId){paste("studies", studyId, 'probands', probandId, "results", sep='/')}

callSubjectiveResultsRESTAPIOperation <- function(operationCallPath, apiKey){
  getLogger('xs_adapter')
  authHeader <- auth(apiKey)
  loginfo(paste("Downloading Forms by URL:", operationCallPath), logger='xs_adapter')
  req <- GET(operationCallPath, authHeader)
  loginfo(paste('Response:', req), logger='xs_adapter')
  resultsJson <- content(req, as = "text", encoding = "UTF-8")
  logdebug(paste('Response body:', resultsJson), logger='xs_adapter')
  results <- data.frame(fromJSON(resultsJson))
  logdebug(paste('downloadSubjectiveResultsAsJson:', results), logger='xs_adapter')
  if(is.null(results[["error"]])){
    if(nrow(results) > 0 && .isOldDate(results))
      results <- .correctOldXSTimeFormat(results)
  }
  results
}

.isOldDate = function(tbl){
  length(which(strsplit(tbl[["Form_finish_date"]], "")[[1]] == "+") > 0)
}

.correctOldXSTimeFormat = function(tbl){
  plusPos = sapply(strsplit(tbl[["Form_finish_date"]], ""), function(c){
    pos = which(c == "+")
  })
  tbl["Form_finish_date"] = substr(tbl$Form_finish_date, start = 1, stop = plusPos - 1)
  tbl
}
