##Mapping
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
#Downloading shapefile
#######
#tmp <- tempdir()
#link <- "https://www2.census.gov/geo/tiger/TIGER2019/TRACT/tl_2019_48_tract.zip"
#filename <- basename(link)
#download.file(link, filename)
#unzip(filename, exdir = tmp )
#shapefile <- st_read(dsn = tmp, layer = tools::file_path_sans_ext(filename))
##########
#Extracting FIPS code
shapefile$FIPS <- as.numeric(str_sub(shapefile$GEOID, 1, 5 ))

jointshape <- left_join(shapefile,deadlycensus, by = "FIPS" )

#Actually trying to plot using ggplot
ggplot(jointshape) + geom_sf(aes(fill = skateparks))

#we'll end up trying tmap stuff too
#First go at monte carlo moran;s I
########################################
#Sick, now let's define neighbor relations
nbTX <-poly2nb(jointshape, queen=TRUE)

#and weights
lw <- nb2listw(nbTX, style="B")

#Spatially lagged values
Skate.lag <- lag.listw(lw,jointshape$skateparks)

#Moran's I statistic
RandomMoran=moran.mc(jointshape$skateparks,lw, nsim=500)
#shows high degree of clustering in skateparks
####################
#MOran's I as a function of distance
##########
cord<-coordinates(jointshape)
