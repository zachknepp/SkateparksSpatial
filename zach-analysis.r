#https://rpubs.com/quarcs-lab/tutorial-spatial-regression
#http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html
#https://spatial.burkeyacademy.com/

#Analysis

#To do list
#############################
#For each county, calculate (approx) distance to nearest skatepark, dist to nearest 
#############################
##loading packages
##################
library(stargazer)
library(lfe)
#library(stringr)
#library(data.table)
#library(texreg)
library(tidyverse)
library(spData)
library(spatialreg)
library(rgdal)
library(rgeos)
library(housingData)
library(ISLR)
library(gpairs) #makes a nice lil visualizations for correlations
####################



data(geoCounty) #Only has data for 3075 FIPS codes, skate master has 3230. Wuddup fool?
geoCounty<-rename(geoCounty, FIPS = fips)
geoCounty$FIPS<-as.integer(as.character(geoCounty$FIPS))
setwd("C:/Users/zachk/Desktop/Econ Senior Project/Full Model/")



skatemaster=read.csv("skatemaster.csv", header=TRUE)
msm = read.csv("miniskatemaster.csv", header=TRUE) #Skate master, but with columns X, state and state.abb removed

#Correlations w/ pairs plot
trial <- msm[,-c(1, 8:13, 17, 19:25,32, 35)]
#Removing FIPS, races, days spent in care, and high school or more (redundant, treat as base case)
na.omit(trial)
suppressWarnings(corrgram(trial))
########################CURRENT STICKING POINT! MERGING OUR DATA REMOVES A BUNCH OF IT
  #Adding county centroids CU
spskatemaster <- merge(x=skatemaster, y = geoCounty, all.x=TRUE, all.y=TRUE)
mspskatemaster <- merge(x = msm, y = geoCounty) 
###########stagazer()
na.omit(mspskatemaster, cols=c("lon","lat"))
attach(mspskatemaster)
#We need to account for suicides/mental health outcomes in counties without access to those things!
  #If no psych hospitals, give em 
  
##
coords <- cbind(mspskatemaster$lon, mspskatemaster$lat)
#Simple Modeling
####################
#Suicides is a rate: no of deaths due to suicide per 100000 pop,
#https://www.cdc.gov/nchs/pressroom/sosmap/suicide-mortality/suicide.htm  should give us data to interpolate rates (if we want)

#Skateparks as x
lm.parksuicide <- lm(suicides.17.19~skateparks, data = mspskatemaster)
stargazer(lm.parksuicide)

lm.degenskaters<- lm(popjuvy.10 ~ skateparks, data=mspskatemaster)
summary(lm.degenskaters)

lm.dumbskaters<-lm(perclessthanhs.15 ~ skateparks, data=mspskatemaster)

stargazer(lm.parksuicide, lm.degenskaters, lm.dumbskaters, 'text')
#############


#Shows a 58% increase in HS completion rate for each unit of skatepark access

stargazer(lm.pkacc1, lm.pkacc2, lm.pkacc3)
#############
#Population as a predictor
lm(skateparks ~ pop, data = mspskatemaster)

lm(suicides.17.19~pop.10, data = mspskatemaster)
lm(suicides.17.19~pop, data = mspskatemaster)
lm(psych.lt~pop, data = mspskatemaster)

#Psych hospital count as predictor
lm.psychsuicide<-lm(suicides.17.19~psych.st+psych.lt + psych.st*psych.lt, data =mspskatemaster)
lm.psychdegen<-lm(popjuvy.10~psych.st+psych.lt + psych.st*psych.lt, data =mspskatemaster)
lm.psychdrop<-lm(perclessthanhs.15~psych.st+psych.lt + psych.st*psych.lt, data =mspskatemaster)
stargazer(lm.psychsuicide, lm.psychdegen, lm.psychdrop)
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

#Summary shows that access to additional any type of psychiatric facilities is correlated with HIGHER rates of suicide among the population. 
#Selection bias needs to be treated! we had to omit 2273 observations due to missingness!
#After looking through the data, you can see these counties with N/A fo suicides, tend to have zero access to psychiatric care. 
 
#Graphical summary of models
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

#Spatial Modeling
#############
#Spatial Weights
  #5NN
    fivenn <- knearneigh(coords, k=5, longlat = TRUE)
    fivenn.nb <- knn2nb(fivenn)
    #W<-nb2listw(fivenn.nb, style="W", zero.policy=TRUE)
  #10NN
    tennn <- knearneigh(coords, k=10, longlat=TRUE)
    tennn.nb <- knn2nb(tennn)
  #Actual weights  
  W<-nb2listw(fivenn.nb, style = "W", zero.policy = TRUE)
moran.parksuicide <- lm.morantest(lm.parksuicide, W, alternative = "two.sided")
moran.lm2 <- lm.morantest(lm2, W, alternative = "two.sided")
moran.moneyparks<-lm.morantest(lm.moneyparks, W, alternative = "two.sided")
  #Seeing what kind of models we should use
LM1 <-lm.LMtests(lm1, W, test = "all")
LM2 <-lm.LMtests(lm2, W, test = "all")
lm.LMtests(lm.moneyparks, W, test="all")

#Train spatial models and time em
ptm <- proc.time()
sar1<-lagsarlm(suicides.17.19~psych.st+psych.lt+ psych.st*psych.lt ,data = mspskatemaster, W)
proc.time() - ptm

ptm <- proc.time()
sar2 <- lagsarlm(lm.poorsuicide, data = df, W)
proc.time() - ptm

summary(sar1)
impacts(sar1, listw = W)
summary(impacts(sar1, listw=W, R=500),zstats=TRUE)\

#McMillen's Spatial Selection Model ()
###########################
#y1i will be suicides, predicted by skateparks, singleparent, medhhinc, pop, 
#y2i will be psych care centers (need to create an effective measure), predicted by skateparks, singleparent, medhhinc, pop, 
 
