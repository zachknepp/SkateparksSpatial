##Toy model on Texas
library(tidyverse)
library(sf)           # Objects and functions for geospatial data
library(rgdal)        # Functions for spatial data input/output
library(ggplot2)      # Graphing functions
library(dplyr)        # Functions for processing tabular data
library(tidyr)        # Functions for processing tabular data
library(scales)       # Additional graphics functions
library(RColorBrewer) # Color ramps for graphs and maps
library(gridExtra)    # Functions for arranging multiple plots on a page
library(readr)
#import parks
try(txparks <- read.csv("C:/Users/zachk/Desktop/Econ Senior Project/Texas/texasParks (1).csv", header = TRUE))
try(txparks <- read.csv("D:\\Dropbox\\Teaching\\ECO499\\Zach Knepper\\skatepark.csv", header = TRUE))
colnames(txparks)[9]<-"zipcode"
#import zip fips
try(txzipfips <- read.csv("D:\\Dropbox\\Teaching\\ECO499\\Zach Knepper\\zip-county.csv", header = TRUE))
#colnames(txzipfips)[1]<-"zipcode"
#only need the first 2 columns

#import texas master
try(txcensus <- read.csv("D:\\Dropbox\\Teaching\\ECO499\\Zach Knepper\\master.csv", header = TRUE))
try(txcensus <- read.csv("C:/Users/zachk/Desktop/Econ Senior Project/Texas/texascensusmaster.csv", header = TRUE))
colnames(txcensus)[2] <- "FIPS"

#use zip fips to add county code to each park (mutate)
txparksfip <- merge(x = txparks, y = txzipfips, by = "zipcode")
#colnames(txparksfip)[15] <- "FIPS"
#omit crappy parks

txparksfip1 <-txparksfip %>%
  group_by(FIPS) %>%
  summarize(no_obs =n())
#Add county info to frame
txskatecensus = merge(x = txcensus, y = txparksfip1, by = "FIPS", all.x=TRUE)
txskatecensus$skateparks <- replace_na(txskatecensus$no_obs, 0)
try(write.csv(txskatecensus,"C:/Users/zachk/Desktop/Econ Senior Project/Texas/txMaster.csv", row.names = FALSE))
    try(write.csv(txskatecensus,"D:\\Dropbox\\Teaching\\ECO499\\Zach Knepper\\skatemaster.csv", row.names = FALSE))

#deadlycensus = merge(x = txskatecensus, y = de, by = "FIPS")
#write.csv(deadlycensus,"C:/Users/zachk/Desktop/Econ Senior Project/Texas/TXtoMap.csv", row.names = FALSE)

library(housingData)
data(geoCounty)
geoCounty <- rename(geoCounty, FIPS = fips)

master <- merge(x = df, y = geoCounty, by = "FIPS")

#Maps and stuff
#First, we need a shapefile


