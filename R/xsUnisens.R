library(jsonlite)
library(httr)
library(digest)
library(logging)

source('R/utils.R')
source('R/config.R')

downloadUnisensData <- function(xsServerURL, studyXSId, probandXSId, apiKey){
  getLogger('xs_adapter')
  loginfo(paste('Downloading from movisens-server', xsServerURL, '...'), logger='xs_adapter')
  fromURL <- paste(getXSAPIURL(xsServerURL), .getUnisensPath(studyXSId, probandXSId), sep='/')
  authHeader <- auth(apiKey)
  loginfo(paste("Downloading zipped file by URL:", fromURL), logger='xs_adapter')
  req <- GET(fromURL, authHeader)
  result <- if(.hasContentType(req, 'application/json')){
    .returnJSonErrorDescription(req)
  }
  else if(.hasContentType(req, 'application/zip')){
    unisensFiles <- .returnUnzippedUnisensFiles(req)
    c(unisensFolder = .getUnisensFolder(), basename(unisensFiles))
  }
  else
    NA
  loginfo(paste('Downloaded Unisens files:', paste(result, collapse=', ')), logger='xs_adapter')
  result
}

.returnJSonErrorDescription <- function(req){
  resultsJson <- content(req, as = "text", encoding = "UTF-8")
  errorDescr <- data.frame(fromJSON(resultsJson))
  errorDescr
}

.returnUnzippedUnisensFiles <- function(req){
  resultingZippedContent <- content(req, as = "raw", type='application/zip')
  tmpFile <- tempfile("unisensData", fileext = '.zip')
  writeBin(resultingZippedContent, tmpFile)
  unzippedResults <- .prepareFiles(tmpFile)
  unzippedResults
}

.prepareFiles <- function(zipfile){
  unisensFolder <- .getUnisensFolder()
  if(file.exists(unisensFolder))
    unlink(unisensFolder, recursive = TRUE)
  logdebug(paste('Unzip unisens file into:', unisensFolder), logger='xs_adapter')
  isFileCreated <- dir.create(unisensFolder)
  tryCatch({
    if(isFileCreated){
      unzippedResults <- unzip(zipfile, exdir = unisensFolder)
      unzippedResults
    }
    else
      stop('Could not create unisens folder for unzipping!')
  }, error = function(e){
    stop('Could not unzip files!')
  })
}

.getUnisensFolder <- function(){
  unisensFolder <- paste(tempdir(), 'unisens', sep='/')
}

.getUnisensPath <- function(studyXSId, probandXSId){
  paste('studies',studyXSId,'probands',probandXSId,'unisens', sep = '/')
}
