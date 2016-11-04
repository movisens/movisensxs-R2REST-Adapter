library(jsonlite)
library(httr)
library(digest)
library(logging)

source('R/utils.R')
source('R/config.R')
source('R/apiRoutes.R')

adapter.conf.probandColumns <- c("id", "currentVersion", "coupleURL", "status", "currentDevice", "coupleDate", "startDate", "endDate")

downloadProbands <- function(xsServerURL, studyId, apiKey){
  getLogger('xs_adapter')
  loginfo("Downloading Probands from XSServer", logger='xs_adapter')
  fromURL <- paste(getXSAPIURL(xsServerURL), getProbandsPath(studyId), sep='/')
  headers <- add_headers(authHeader(apiKey), acceptHeader(jsonMIME))
  response <- GET(fromURL, headers)
  loginfo(paste('AuthHeader attached:', headers), logger='xs_adapter')
  .extractResultFromRequestPrbs(response)
}

.extractResultFromRequestPrbs <- function(response){
  if(response$status_code == 404)
    stop(xsExceptions['notFound'])
  else if(response$status_code == 401)
    stop(xsExceptions['invalidAPIKey'])
  else if(.hasContentType(response, jsonMIME))
    .parseProbandTable(response)
  else
    stop('The xs server responded with a request of an unknown type.')
}

.parseProbandTable <- function(response){
  probandsJson <- content(response, as = "text", encoding = "UTF-8")
  logdebug(paste('Response body:', probandsJson), logger='xs_adapter')
  results <- fromJSON_(probandsJson)
  completedResults <- completeMissingColumns(results, adapter.conf.probandColumns)
  loginfo(paste('Downloaded Probands:', completedResults), logger='xs_adapter')
  if(!is.null(completedResults[["error"]])){
    stop('Proband could not be downloaded.')
  }
  completedResults
}
