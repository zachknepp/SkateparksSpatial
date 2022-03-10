#https://rpubs.com/quarcs-lab/tutorial-spatial-regression
#http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html
#https://spatial.burkeyacademy.com/


rm(list=ls())
library(stargazer)
library(lfe)
#library(stringr)
#library(data.table)
#library(texreg)
#library(tidyverse)
library(spData)
library(spdep)
library(spatialreg)
library(rgdal)
library(rgeos)
library(housingData)
data(geoCounty)

#### Here I am setting workspaces
try(setwd("D:\\Dropbox\\Teaching\\ECO499\\Zach Knepper"))
try(setwd("C:\\Users\\zachk\\Desktop\\Econ Senior Project\\Texas"))

df<- read.csv(".\\skatemaster.csv", header = TRUE)
###PUll hisp from AHRF later
df$perchisp.10 <- 100 - df$percwhite.10 - df$percblack.10 - df$percamind.10 - df$percasian.10 - df$percpi.10
df$impoverishedpc <- df$personsimpoverished/df$pop

stargazer(df, type='text')




lm1 <- lm(er.visits ~ skateparks, data = df)
lm2 <- lm(er.visits ~ skateparks + pop + medage.10 + percwhite.10 + percblack.10 + perchisp.10 + percasian.10 
          + medhhincome + impoverishedpc + singleparenthouseholds.10 
          + perclessthanhs.15 + percwithhsormore.15 + fouryearscollege.15, data = df)


coords <- cbind(df$lon, df$lat)
fivenn <- knearneigh(coords, k=5, longlat = TRUE)
fivenn.nb <- knn2nb(fivenn)
W<-nb2listw(fivenn.nb, style="W", zero.policy=TRUE)

moran.lm <- lm.morantest(lm1, W, alternative="two.sided")

LM<-lm.LMtests(lm1, W, test="all")

sar<-lagsarlm(lm1, data = df, W)
summary(sar)
#impacts(sar, listw = W)
summary(impacts(sar, listw=W, R=500),zstats=TRUE)
