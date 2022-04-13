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
##############################
#Loading files
dirlink <- "C:/Users/zachk/Desktop/Econ Senior Project/Full Model/CountyShapeFile/cb_2020_us_county_5m.zip"
filename <- basename(dirlink)
unzip(filename, exdir = "C:/Users/zachk/Desktop/Econ Senior Project/Full Model/CountyShapeFile" )
shapefile <- st_read(dsn = "C:/Users/zachk/Desktop/Econ Senior Project/Full Model/CountyShapeFile/2020USCounty", layer = tools::file_path_sans_ext(filename))
colnames(shapefile)[5]<-"FIPS"
shapefile$FIPS<-as.integer(shapefile$FIPS)
msm = read.csv("miniskatemaster", header=TRUE)

###########################
#Construction of Joint Shapefile
jointshape <- merge(shapefile , msm, by = "FIPS" , all.x=TRUE)

#################################
#"Cartography"
ggplot(jointshape) + xlim(125, 67) + ylim(24,50) +
  geom_sf(aes(fill = skateparks)) +  
  scale_fill_gradient(low="blue", high="red") + 
  labs(fill = "Number of skateparks") + theme(legend.position = "top")
  #Beautiful

ggplot(jointshape) + xlim(125, 67) + ylim(24,50) +
    geom_sf(aes(fill = impoverishedpc)) + 
  scale_fill_gradient(low="green", high="red") + labs(fill = "Percentage of persons living in poverty") +
    theme(legend.position="top")

ggplot(jointshape) + xlim(125, 67) + ylim(24,50) +
  geom_sf(aes(fill = suicides.17.19)) + 
  scale_fill_gradient(low="red", high="black") + labs(fill = "Deaths by suicide per capita from 2017-2019") +
  theme(legend.position="top")

################################
#Goofin with spatial stuff: neighbors based on actual contiguity
#"W" implies row normalized matrix
neigh<-dnearneigh(jointshape)
neighs<-poly2nb(jointshape[which (jointshape$suicides.17.19 > 0),])
W=nb2listw(neigh, style = "W", zero.policy = TRUE)
Ws = nb2listw(neighs, style = "W", zero.policy = TRUE)

moran.controlsuicide <- lm.morantest(lm.controlsuicide, Ws, alternative = "two.sided", zero.policy = TRUE)
moran.controldegen <- lm.morantest(lm.controldegen, W, alternative = "two.sided", zero.policy=TRUE)
moran.alldrop<-lm.morantest(lm.alldrop, W, alternative = "two.sided", zero.policy=TRUE)
  

