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
####geoData
data(geoCounty)
geoCounty<-rename(geoCounty, FIPS = fips)
geoCounty$FIPS<-as.integer(as.character(geoCounty$FIPS))

#### Here I am setting workspaces
try(setwd("D:\\Dropbox\\Teaching\\ECO499\\Zach Knepper"))
try(setwd("C:\\Users\\zachk\\Desktop\\Econ Senior Project\\Texas"))

df<- read.csv(".\\miniskatemaster.csv", header = TRUE)
###PUll hisp from AHRF later
df$perchisp.10 <- 100 - df$percwhite.10 - df$percblack.10 - df$percamind.10 - df$percasian.10 - df$percpi.10
df$impoverishedpc <- df$personsimpoverished/df$pop

spatdf <- merge(x = df, y = geoCounty) 
na.omit(spatdf, cols=c("lon", "lat"))
attach(spatdf)

stargazer(spatdf, type='text')



#############################Simple Linear Modeling###############################
lm1 <- lm(er.visits ~ skateparks, data = spatdf)
lm2 <- lm(er.visits ~ skateparks + pop + medage.10 + percwhite.10 + percblack.10 + perchisp.10 + percasian.10 
          + medhhincome + impoverishedpc + singleparenthouseholds.10 
          + perclessthanhs.15 + percwithhsormore.15 + fouryearscollege.15, data = spatdf)

###Skateparks as x
lm.parksuicide <- lm(suicides.17.19~skateparks, data = spatdf)
summary(lm.parksuicide)

lm.degenskaters<- lm(popjuvy.10 ~ skateparks, data=spatdf)
summary(lm.degenskaters)

lm(skateparks ~ pop, data = spatdf)

lm(suicides.17.19~pop.10, data = spatdf)
lm(suicides.17.19~pop, data = spatdf)
lm(psych.lt~pop, data = spatdf)

#Psych hospital count as predictor
lm.psychsuicide<-lm(suicides.17.19~psych.st+psych.lt + psych.st*psych.lt, data =mspskatemaster)
          #Shows that psych hospitals increase the suicide rate of the county. This is evidence of selection bias.

#income as a predictor
lm.moneyparks <-lm(skateparks~log(medhhincome), data = mspskatemaster)
summary(lm.moneyparks) #For each pct increase in medhhinc, we get 3.5 more parks. WOW!

lm.moneysuicide = lm(suicides.17.19~ medhhincome, data = mspskatemaster)

#Demographics as predictors
lm(suicides.17.19 ~ percwhite.10 + pop.10, data = mspskatemaster)
lm(suicides.17.19 ~ percasian.10 + pop.10, data = mspskatemaster)
lm(suicides.17.19 ~ percblack.10 + pop.10, data = mspskatemaster)
lm(suicides.17.19 ~ perchisp.10 + pop.10, data = mspskatemaster)
lm(suicides.17.19 ~ percamind.10 + pop.10, data = mspskatemaster)

#Sad stuff as predictors 
lm.degenpobres<- lm(popjuvy.10 ~ personsimpoverished, data=mspskatemaster)
summary(lm.degenpobres)

lm.degenparents<-lm(popjuvy.10~singleparenthouseholds.10, data=mspskatemaster)
summary(lm.degenparents)

############################Graphical summary of models#######################
bestfit <- geom_smooth(
  method = "lm", 
  se = FALSE, 
  colour = alpha("steelblue", 0.5), 
  size = 2
)

geom_lm <- function(formula = y ~ x, colour = alpha("steelblue", 0.5), 
                    size = 2, ...)  {
  geom_smooth(formula = formula, se = FALSE, method = "lm", colour = colour,
              size = size, ...)
}

ggplot(mspskatemaster, aes(skateparks,impoverishedpc)) + geom_point() + 
  scale_x_continuous(trans='log2') + bestfit

################################Spatial stuff#################################

coords <- cbind(spatdf$lon, spatdf$lat)
fivenn <- knearneigh(coords, k=5, longlat = TRUE)
fivenn.nb <- knn2nb(fivenn)
W<-nb2listw(fivenn.nb, style="W", zero.policy=TRUE)

moran.lm <- lm.morantest(lm1, W, alternative="two.sided")

LM<-lm.LMtests(lm1, W, test="all")

sar<-lagsarlm(lm1, data = df, W)
summary(sar)
#impacts(sar, listw = W)
summary(impacts(sar, listw=W, R=500),zstats=TRUE)

#McMillen's Spatial Selection Model
#y1i will be suicides, predicted by skateparks, singleparent, medhhinc, pop, 
#y2i will be psych care centers (need to create an effective measure), predicted by skateparks, singleparent, medhhinc, pop, 
