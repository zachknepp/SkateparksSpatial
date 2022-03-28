#Mapping the US
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
library(tmap)
library(spData)
library(spdep)
library(ggmap)
data(geoCounty)
setwd("C:/Users/zachk/Desktop/Econ Senior Project/Full Model")

#Importing shapefile
dirlink <- "C:/Users/zachk/Desktop/Econ Senior Project/Full Model/CountyShapeFile/cb_2020_us_county_5m.zip"
filename <- basename(dirlink)
unzip(filename, exdir = "C:/Users/zachk/Desktop/Econ Senior Project/Full Model/CountyShapeFile" )
shapefile <- st_read(dsn = "C:/Users/zachk/Desktop/Econ Senior Project/Full Model/CountyShapeFile/2020USCounty", layer = tools::file_path_sans_ext(filename))
colnames(shapefile)[5]<-"FIPS"
shapefile$FIPS<-as.integer(shapefile$FIPS)

msm = read.csv("miniskatemaster", header=TRUE)

#joining em
jointshape <- merge(shapefile , msm, by = "FIPS" , all.x=TRUE)


#ggplot way
ggplot(jointshape) + xlim(125, 67) + ylim(24,50) +
  geom_sf(aes(fill = skateparks)) +  
  scale_fill_viridis_c(option ="B") + 
  labs(fill = "Number of skatparks") + theme(legend.position = "top")
  #Beautiful

ggplot(jointshape) + xlim(125, 67) + ylim(24,50) +
    geom_sf(aes(fill = impoverishedpc)) + 
    scale_fill_viridis_c(option = "D")+ labs(fill = "Percentage of persons living in poverty") +
    theme(legend.position="top")

