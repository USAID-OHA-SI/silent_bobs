# Where to find DHS API resources

library(tidyverse)
library(datim)
library(httr)
library(readr)
library(XML)
library(RCurl)


bobs_home <- "https://zambia.msidevcloud.com/"
bobs_api_info <- paste0(bobs_home, "api/resources")
url <- paste0(bobs_home,"api/me")

# Service = "bobs 
login <- GET(url, authenticate("", ""))


# Tip sheet
# https://integratehealth.org/wp-content/uploads/2020/09/API-Usage-Tip-Sheet.pdf


bobs_ip_table <- "https://zambia.msidevcloud.com/api/analytics.json?dimension=pe%3ALAST_12_MONTHS&dimension=etirobrLGaO%3ADKdTd0vYIyK%3BXzH6ixuumyb%3BoAlUWzToZBb%3BttFnWAapo1r&dimension=HiOwI2CTwEC%3Ap3FN6FXaAVb%3BjlO7qSJobpZ&dimension=dx%3ALV138qS9eCD%3BtsMR4m66ili%3BJM2OJ2ngzX9%3Bmq7qnFx4tQJ%3BCIQOfMLBAy4%3BZSzLCJxdPMk%3BpSfHxxKapus&showHierarchy=false&hierarchyMeta=false&includeMetadataDetails=true&includeNumDen=true&skipRounding=false&completedOnly=false&outputIdScheme=NAME&filter=ou%3AUSER_ORGUNIT"

