library(jsonlite)
library(lubridate)
library(tidyverse)

setwd("~/R/EVs")


# Retrieve data from the

codes <- c("AT","BE","BG","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","NL","PL","PT","RO","SK","SI","ES","SE","GB","IS","NO","CH",
           "XK","BA")

dataset <- tibble()

# access the API key
key <- read_file("auth_key.txt")

for (i in codes) {
  
  temp <- fromJSON(paste0("https://api.openchargemap.io/v3/poi/?output=json&countrycode=",i,
                          "&maxresults=100000&key=",key))
  temp$Country <- i
  
  temp <- jsonlite::flatten(temp)
  
  # unnest information on single charging points from each station
  temp %>% select(-ID,-StatusTypeID)%>% unnest(cols = c(Connections)) -> temp
  
  # select only columns that are relevant for the analysis
  temp %>% select(Country,DateCreated,DateLastStatusUpdate,StatusTypeID,UsageTypeID,AddressInfo.Latitude,
                   AddressInfo.Longitude,OperatorID,PowerKW,NumberOfPoints) -> temp
  
  dataset <- bind_rows(dataset,temp) 
}

# keep only operational charging stations
dataset <- dataset %>% filter( StatusTypeID %in% c(50,75,0,NA))

# convert the date
dataset <- dataset %>% mutate(DateCreated =as.POSIXct(DateCreated,format="%Y-%m-%dT%H:%M:%S"))

# save the dataset

save(dataset,"OpenChargeMap_Europe_columns.csv")