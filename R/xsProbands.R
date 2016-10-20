library(jsonlite)
library(httr)
library(digest)
library(logging)

adapter.conf.probandColumns <- c("id", "currentVersion", "status", "currentDevice", "coupleDate", "startDate", "endDate")

downloadProbands <- function(xsServerURL, studyId, apiKey){
  source('utils.R', chdir=T)
  source('config.R', chdir=T)
  getLogger('xs_adapter')
  loginfo("Downloading Probands from XSServer", logger='xs_adapter')
  fromURL <- paste(getXSAPIURL(xsServerURL), .getProbandsPath(studyId), sep='/')
  authHeader <- auth(apiKey)
  req <- GET(fromURL, authHeader)
  loginfo(paste('AuthHeader attached:', authHeader), logger='xs_adapter')
  probandsJson <- content(req, as = "text", encoding = "UTF-8")
  logdebug(paste('Response body:', probandsJson), logger='xs_adapter')
  results <- fromJSON(probandsJson)
  completedResults <- completeMissingColumns(results, adapter.conf.probandColumns)
  loginfo(paste('Downloaded Probands:', completedResults), logger='xs_adapter')
  if(!is.null(completedResults[["error"]])){
    completedResults <- NA
  }
  completedResults
}

.getProbandsPath <- function(studyId){paste0("studies/", studyId, "/probands")}
