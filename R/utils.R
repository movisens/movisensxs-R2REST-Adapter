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

auth <- function(apiKey){
  add_headers("Authorization" = paste("ApiKey", apiKey, sep=" "))
}

getXSAPIURL <- function(xsServerURL){
  paste(xsServerURL, movisens.xsadapter.conf.apiPath, sep='/')
}

.hasContentType <- function(req, typeName){
  length(grep(paste0('.*', typeName, '.*'), headers(req)[["Content-Type"]])) > 0
}
