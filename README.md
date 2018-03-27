# movisensxs-R2REST-Adapter

This library is a prototype to demonstrate how to access the BETA movisensXS Api in R.

## How to install movisensxs-R2REST-Adapter
```r
install.packages("devtools")
library(devtools)
devtools::install_github('movisens/movisensxs-R2REST-Adapter')
```

## How to use movisensxs-R2REST-Adapter
```r
library(movisensR2RESTAdapter)
test.conf.studyId <- 5192
test.conf.apikey <-'TODO' # write to xs@movisens.com to request access
test.conf.serverURL <- 'https://xs.movisens.com'
result <- downloadOverallSubjectiveDataAsJson(test.conf.serverURL, test.conf.studyId, test.conf.apikey)
```
