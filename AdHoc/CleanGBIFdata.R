
#clear env
rm(list=ls())


library(dplyr)
library(CoordinateCleaner)
library(readr)

sp_name<-"Farctica"
fname<-"0210347-200613084148143.csv"

sp_name<-"Mbassanus"
fname<-"0210434-200613084148143.csv"

sp_name<-"Sparadisaea"
fname<-"0210436-200613084148143.csv"

sp_name<-"Sdougallii"
fname<-"0211366-200613084148143.csv"


setwd(paste0("C:/Users/Henry/Documents/Research/ZSL/RawData/GBIF/",sp_name))



gbif_download = read_tsv(fname, col_types=cols(gbifID = col_double(),
                                               individualCount = col_double(),
                                               decimalLatitude = col_double(),
                                               decimalLongitude = col_double(),
                                               coordinateUncertaintyInMeters = col_double(),
                                               coordinatePrecision = col_double(),
                                               elevation = col_double(),
                                               elevationAccuracy = col_double(),
                                               depth = col_double(),
                                               depthAccuracy = col_double(),
                                               eventDate = col_datetime(format = ""),
                                               day = col_double(),
                                               month = col_double(),
                                               year = col_double(),
                                               taxonKey = col_double(),
                                               speciesKey = col_double(),
                                               recordNumber = col_character(),
                                               dateIdentified = col_datetime(format = ""),
                                               typeStatus = col_character(),
                                               lastInterpreted = col_datetime(format = ""),
                                               establishmentMeans = col_character(),
                                               verbatimScientificNameAuthorship = col_character(),
                                               rightsHolder = col_character(),
                                               identifiedBy = col_character(),
                                               locality = col_skip()))

gbiffil <- gbif_download %>%
  # set lowercase column names to work with CoordinateCleaner
  setNames(tolower(names(.))) %>% 
  
  #only take present files
  filter(occurrencestatus  == "PRESENT") %>%
  
  #get rid of invalid long/lat
  filter(!is.na(decimallongitude)) %>% 
  filter(!is.na(decimallatitude)) %>% 
  
  #get rid of fossils and captives
  filter(!basisofrecord %in% c("FOSSIL_SPECIMEN","LIVING_SPECIMEN")) %>%
  
  
  filter(!establishmentmeans %in% c("MANAGED", "INTRODUCED", "INVASIVE", "NATURALISED")) %>%
  filter(year >= 1900) %>% 
  
  #we only want high precision data
  filter(coordinateprecision > 0.01 | is.na(coordinateprecision)) %>% 
  
  #want high certainty
  filter(coordinateuncertaintyinmeters < 10000 | is.na(coordinateuncertaintyinmeters)) %>%
  filter(!coordinateuncertaintyinmeters %in% c(301,3036,999,9999)) %>% 
  
  #0,0 urgh
  filter(!decimallatitude == 0 | !decimallongitude == 0) %>%
  
  #don't trust centroid data
  cc_cen(buffer = 2000) %>% # remove country centroids within 2km 
  cc_cap(buffer = 2000) %>% # remove capitals centroids within 2km
  cc_inst(buffer = 2000) %>% # remove zoo and herbaria within 2km 
  
  #get rid of replicates
  distinct(decimallongitude,decimallatitude,specieskey,datasetkey, .keep_all = TRUE) 
  



dim(gbif_download)
dim(gbiffil)


write.csv(gbiffil, paste0("C:/Users/Henry/Documents/Research/ZSL/ProcessedData/GBIF/",sp_name,"/",sp_name,".csv"))



