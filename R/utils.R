#' @import lubridate
#'
NULL

completeMissingColumns = function(tbl, allColnames){
  additionalCols <- allColnames[!is.element(allColnames, colnames(tbl))]
  extendedTbl = tbl
  tblExtension = data.frame(lapply(additionalCols, function(x){rep(NA, times=nrow(extendedTbl))}))
  if(nrow(tblExtension) > 0){
    extendedTbl = data.frame(tbl, tblExtension)
    names(extendedTbl) = c(names(tbl), additionalCols)
    extendedTbl
  }
  else
    tbl
}

authHeader <- function(apiKey){
  c(Authorization = paste("ApiKey", apiKey, sep=" "))
}

zipMIME <- 'application/zip'
jsonMIME <- 'application/json'
htmlMIME <- 'text/html'
xlsxMIME <- 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'

acceptHeader <- function(mimetypes){
  mimetypes_ <- paste(mimetypes, collapse=';')
  c(Accept = mimetypes_)
}

getXSAPIURL <- function(xsServerURL){
  paste(xsServerURL, movisens.xsadapter.conf.apiPath, sep='/')
}

.hasContentType <- function(response, typeName){
  headers_ <- headers(response)
  contentHeaders_ <- headers_[tolower(names(headers_)) == "content-type"]
  any(sapply(contentHeaders_, function(contentType){
    length(grep(paste0('.*', typeName, '.*'), contentType)) > 0
  }))
}

parseSubjFormDates <- function(dateVec){
  parsedDate <- parse_date_time(dateVec, "%Y%m%dT%H%M%OS%z", exact=TRUE)
  parsedDate
}

parseMessageDates <- function(dateVec){
  parsedDate <- parse_date_time(dateVec, "%Y-%m-%dT%H:%M:%OS%z", exact=TRUE)
  parsedDate
}

extractZipFile <- function(response){
  resultingZippedContent <- content(response, as = "raw", type=zipMIME)
  tmpFile <- tempfile("unisensData", fileext = '.zip')
  writeBin(resultingZippedContent, tmpFile)
  tmpFile
}

extractXLSXFile <- function(response){
  resultingZippedContent <- content(response, as = "raw", type=xlsxMIME)
  tmpFile <- tempfile("subjectiveData", fileext = '.xlsx')
  writeBin(resultingZippedContent, tmpFile)
  tmpFile
}

fromJSON_ <- function(txt){
  as.data.frame(fromJSON(txt))
}
