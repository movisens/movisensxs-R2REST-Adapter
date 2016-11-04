#' @import jsonlite
#' @import httr
#' @import logging
NULL


#' Download all Unisens data of a study's proband as a zip file
#' @param xsServerURL the address of the xs server
#' @param studyXSId the xs-id of the study
#' @param probandXSId the xs-id of the proband
#' @param apiKey the secret api-key of the study
#' @export
#'
downloadUnisensData <- function(xsServerURL, studyXSId, probandXSId, apiKey){
  getLogger('xs_adapter')
  loginfo(paste('Downloading from movisens-server', xsServerURL, '...'), logger='xs_adapter')
  fromURL <- paste(getXSAPIURL(xsServerURL), getUnisensPath(studyXSId, probandXSId), sep='/')
  headers <- add_headers(authHeader(apiKey), acceptHeader(jsonMIME))
  loginfo(paste("Downloading zipped file by URL:", fromURL), logger='xs_adapter')
  response <- GET(fromURL, headers)
  .extractResultFromRequestUnis(response)
}

.extractResultFromRequestUnis <- function(response){
  if(response$status_code == 404)
    stop(xsExceptions['notFound'])
  else if(response$status_code == 401)
    stop(xsExceptions['invalidAPIKey'])
  else if(.hasContentType(response, zipMIME))
    .extractZipFilesUnis(response)
  else
    stop('The xs server responded with a request of an unknown type.')
}

.extractZipFilesUnis <- function(response){
  unisensFiles <- .returnUnzippedUnisensFiles(response)
  result <- c(unisensFolder = .getUnisensFolder(), basename(unisensFiles))
  loginfo(paste('Downloaded Unisens files:', paste(result, collapse=', ')), logger='xs_adapter')
  result
}

.returnUnzippedUnisensFiles <- function(response){
  tmpFile <- extractZipFile(response)
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
