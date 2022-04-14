#https://rpubs.com/quarcs-lab/tutorial-spatial-regression
#http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html
#https://spatial.burkeyacademy.com/

#Analysis

library(stargazer)
library(lfe)
library(stringr)
#library(data.table)
#library(texreg)
library(tidyverse)
library(spData)
library(spdep)
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
try(setwd("C:/Users/zachk/Desktop/Econ Senior Project/Full Model/"))
try(setwd("C:\\Users\\cyencha\\Dropbox\\Teaching\\ECO499\\Zach Knepper\\SkateparksSpatial-main"))
try(setwd("D:\\Dropbox\\Teaching\\ECO499\\Zach Knepper\\SkateparksSpatial-main"))


msm = read.csv("miniskatemaster.csv", header=TRUE) #Skate master, but with columns X, state and state.abb removed

#Correlations w/ pairs plot
msm <- msm[,-c(1, 8:13, 17, 19:25,32, 35)]
#Removing FIPS, races, days spent in care, and high school or more (redundant, treat as base case)
#na.omit(trial)
#suppressWarnings(corrgram(trial))
########################CURRENT STICKING POINT! MERGING OUR DATA REMOVES A BUNCH OF IT
#Adding county centroids CU
#spskatemaster <- merge(x=skatemaster, y = geoCounty, all.x=TRUE, all.y=TRUE)
mspskatemaster <- base::merge(x = msm, y = geoCounty, by="FIPS") 
###########stagazer()
na.omit(mspskatemaster, cols=c("lon","lat"))
#attach(mspskatemaster)
#We need to account for suicides/mental health outcomes in counties without access to those things!
#If no psych hospitals, give em 

dirlink <- "./CountyShapeFile/cb_2020_us_county_5m.zip"
filename <- basename(dirlink)
unzip(filename, exdir = "./CountyShapeFile" )
shapefile <- st_read(dsn = "./CountyShapeFile/cb_2020_us_county_5m.shp", layer = tools::file_path_sans_ext(filename))
colnames(shapefile)[5]<-"FIPS"
shapefile$FIPS<-as.integer(shapefile$FIPS)

shapefile$FIPS<-as.integer(shapefile$FIPS)
jointshape <- merge(shapefile , msm, by = "FIPS" , all.x=TRUE)
jointshape1 <- merge(jointshape, geoCounty, by = "FIPS")

##
coords <- cbind(mspskatemaster$lon, mspskatemaster$lat)

#msm <- msm[,-c(1, 8:13, 17, 19:25,32, 35)]

#Simple Modeling
####################
#Suicides is a rate: no of deaths due to suicide per 100000 pop,
#https://www.cdc.gov/nchs/pressroom/sosmap/suicide-mortality/suicide.htm  should give us data to interpolate rates (if we want)

#Suicides
lm.parksuicide <- lm(suicides.17.19~skateparks, data = mspskatemaster[which(mspskatemaster$suicides.17.19>0),])
lm.psychsuicide<-lm(suicides.17.19~psych.st+psych.lt, data =mspskatemaster[which(mspskatemaster$suicides.17.19>0),])
lm.controlsuicide<-lm(suicides.17.19 ~ skateparks+ psych.st+ psych.lt+ impoverishedpc+
                      rehab.st+ genhosp.lt+ rehab.lt+ er.visits+
                      medage.10+ percurban.10+ medhhincome+ personsimpoverished+
                        percwhite.10 + percblack.10 + perchisp.10 +
                      households.10+ singleparenthouseholds.10+ fouryearscollege.15,
                      data = mspskatemaster[which(mspskatemaster$suicides.17.19>0),])
stargazer(lm.parksuicide, lm.psychsuicide, lm.controlsuicide, no.space=TRUE)


#Juvenile Detention
lm.degenskaters<- lm(popjuvy.10 ~ skateparks, data=mspskatemaster)
lm.psychdegen<-lm(popjuvy.10~psych.st+psych.lt , data=mspskatemaster)
lm.controldegen<-lm(popjuvy.10~ skateparks+ psych.st+ psych.lt+ impoverishedpc+
                      rehab.st+ genhosp.lt+ rehab.lt+  er.visits+
                      medage.10+ percurban.10+ medhhincome+ personsimpoverished+ 
                      percwhite.10 + percblack.10 + perchisp.10 +
                      households.10+ singleparenthouseholds.10+ fouryearscollege.15, data= mspskatemaster)
stargazer(lm.degenskaters, lm.psychdegen, lm.controldegen,no.space = TRUE)

#Dropout rate
lm.dumbskaters<-lm(perclessthanhs.15 ~ skateparks, data=mspskatemaster)
lm.psychdrop<-lm(perclessthanhs.15~psych.st+psych.lt , data =mspskatemaster)
lm.alldrop<- lm(perclessthanhs.15~ skateparks+ psych.st+ psych.lt+ impoverishedpc+
                  rehab.st+ genhosp.lt+ rehab.lt+ er.visits+
                  medage.10+ percurban.10+ medhhincome+ personsimpoverished+ 
                  percwhite.10 + percblack.10 + perchisp.10 +
                  households.10+ singleparenthouseholds.10+ fouryearscollege.15, data = mspskatemaster)
stargazer(lm.dumbskaters, lm.psychdrop, lm.alldrop,  no.space = TRUE)
#############


#Shows a 58% increase in HS completion rate for each unit of skatepark access

#stargazer(lm.pkacc1, lm.pkacc2, lm.pkacc3)
#############
#Population as a predictor
#lm(skateparks ~ pop, data = mspskatemaster)

#lm(suicides.17.19~pop.10, data = mspskatemaster)
#lm(suicides.17.19~pop, data = mspskatemaster)
#lm(psych.lt~pop, data = mspskatemaster)

#Psych hospital count as predictor

#stargazer(lm.psychsuicide, lm.psychdegen, lm.psychdrop)
#Shows that psych hospitals increase the suicide rate of the county. This is evidence of selection bias.

#income as a predictor
#lm.moneyparks <-lm(skateparks~log(medhhincome), data = mspskatemaster)
#summary(lm.moneyparks) #For each pct increase in medhhinc, we get 3.5 more parks. WOW!

#lm.moneysuicide = lm(suicides.17.19~ medhhincome, data = mspskatemaster)

#Sad stuff as predictors 
#lm.degenpobres<- lm(popjuvy.10 ~ personsimpoverished, data=mspskatemaster)
#summary(lm.degenpobres)

#lm.degenparents<-lm(popjuvy.10~singleparenthouseholds.10, data=mspskatemaster)
#summary(lm.degenparents)

#Summary shows that access to additional any type of psychiatric facilities is correlated with HIGHER rates of suicide among the population. 
#Selection bias needs to be treated! we had to omit 2273 observations due to missingness!
#After looking through the data, you can see these counties with N/A fo suicides, tend to have zero access to psychiatric care. 



library(corrplot)
corrplot(corr=cor(mspskatemaster[, c("suicides.17.19", "popjuvy.10", "perclessthanhs.15", "skateparks", "psych.st", "psych.lt" )],
                  use="complete.obs"),
         method="ellipse")

stargazer(cor(mspskatemaster[, c("suicides.17.19", "popjuvy.10", "perclessthanhs.15", "skateparks", "psych.st", "psych.lt" )],
    use="complete.obs"))



#Spatial Modeling
#############
#Spatial Weights
#5NN

#Put together datasets for all counties (raw, cords, W) and +suicide counties (raws, cordss, Ws)
raw <- mspskatemaster
raws <- mspskatemaster[which (mspskatemaster$suicides.17.19 > 0),]
coordss <- cbind(raws$lon, raws$lat)
coords <- cbind(mspskatemaster$lon, mspskatemaster$lat)

fivenn <- knearneigh(coords, k=5, longlat = TRUE)
fivenn.nb <- knn2nb(fivenn)
W<-nb2listw(fivenn.nb, style = "W", zero.policy = TRUE)

fivenns <- knearneigh(coordss, k=5, longlat = TRUE)
fivenns.nb <- knn2nb(fivenns)
Ws <- nb2listw(fivenns.nb, style = "W", zero.policy = TRUE)



###Moran I
moran.controlsuicide <- lm.morantest(lm.controlsuicide, Ws, alternative = "two.sided")
moran.controldegen <- lm.morantest(lm.controldegen, W, alternative = "two.sided")
moran.alldrop<-lm.morantest(lm.alldrop, W, alternative = "two.sided")
#Seeing what kind of models we should use


LM1 <-lm.LMtests(lm.controlsuicide, Ws, test = "all")
LM2 <-lm.LMtests(lm.controldegen, W, test = "all")
LM3 <-lm.LMtests(lm.alldrop, W, test = "all")



fivenns <- knearneigh(coordss, k=5, longlat = TRUE)
fivenns.nb <- knn2nb(fivenns)
Ws <- nb2listw(fivenns.nb, style = "W", zero.policy = TRUE)
#Train spatial models and time em
ptm <- proc.time()
sar1<-lagsarlm(suicides.17.19 ~ skateparks + psych.st+ psych.lt+ impoverishedpc+
                 rehab.st+ genhosp.lt+ rehab.lt+ er.visits+
                 medage.10+ percurban.10+ medhhincome+ personsimpoverished+
                 percwhite.10 + percblack.10 + perchisp.10 +
                 households.10+ singleparenthouseholds.10+ fouryearscollege.15,
               data = raws, Ws, zero.policy=TRUE)
proc.time() - ptm

summary(sar1)
impacts(sar1, listw = Ws)
summary(impacts(sar1, listw=Ws, R=500),zstats=TRUE)


fivenns <- knearneigh(coordss, k=10, longlat = TRUE)
fivenns.nb <- knn2nb(fivenns)
Ws <- nb2listw(fivenns.nb, style = "W", zero.policy = TRUE)
#Train spatial models and time em
ptm <- proc.time()
sar2<-lagsarlm(suicides.17.19 ~ skateparks + psych.st+ psych.lt+ impoverishedpc+
                 rehab.st+ genhosp.lt+ rehab.lt+ er.visits+
                 medage.10+ percurban.10+ medhhincome+ personsimpoverished+
                 percwhite.10 + percblack.10 + perchisp.10 +
                 households.10+ singleparenthouseholds.10+ fouryearscollege.15,
               data = raws, Ws, zero.policy=TRUE)
proc.time() - ptm

summary(sar2)
impacts(sar2, listw = Ws)
summary(impacts(sar2, listw=Ws, R=500),zstats=TRUE)


neighs<-poly2nb(jointshape1[which (jointshape1$suicides.17.19 > 0),])
Ws = nb2listw(neighs, style = "W", zero.policy = TRUE)
#Train spatial models and time em
ptm <- proc.time()
sar3<-lagsarlm(suicides.17.19 ~ skateparks + psych.st+ psych.lt+ impoverishedpc+
                 rehab.st+ genhosp.lt+ rehab.lt+ er.visits+
                 medage.10+ percurban.10+ medhhincome+ personsimpoverished+
                 percwhite.10 + percblack.10 + perchisp.10 +
                 households.10+ singleparenthouseholds.10+ fouryearscollege.15,
               data = raws, Ws, zero.policy=TRUE)
proc.time() - ptm

summary(sar3)
impacts(sar3, listw = Ws)
summary(impacts(sar3, listw=Ws, R=500),zstats=TRUE)

stargazer(sar1, sar2, sar3, no.space=TRUE)

jointshape1$lm<-resid(lm.controlsuicide)
jointshape1$sar<-resid(sar1)

library(ggplot2)
ggplot(jointshape1) + xlim(125, 67) + ylim(24,50) +  # Set up canvas with outcome variable on y-axis
  geom_sf(aes(fill = sqrt(abs(lm)))) + # Color mapped to abs(residuals)
  scale_fill_gradient(low="white", high="red") + theme(legend.position = "top")





####JUVY
fivenn <- knearneigh(coords, k=5, longlat = TRUE)
fivenn.nb <- knn2nb(fivenn)
W <- nb2listw(fivenn.nb, style = "W", zero.policy = TRUE)
#Train spatial models and time em
ptm <- proc.time()
sar1<-lagsarlm(popjuvy.10 ~ skateparks + psych.st+ psych.lt+ impoverishedpc+
                 rehab.st+ genhosp.lt+ rehab.lt+ er.visits+
                 medage.10+ percurban.10+ medhhincome+ personsimpoverished+
                 percwhite.10 + percblack.10 + perchisp.10 +
                 households.10+ singleparenthouseholds.10+ fouryearscollege.15,
               data = raw, W, zero.policy=TRUE)
proc.time() - ptm

summary(sar1)
impacts(sar1, listw = W)
summary(impacts(sar1, listw=W, R=500),zstats=TRUE)


fivenn <- knearneigh(coords, k=10, longlat = TRUE)
fivenn.nb <- knn2nb(fivenn)
W <- nb2listw(fivenn.nb, style = "W", zero.policy = TRUE)
#Train spatial models and time em
ptm <- proc.time()
sar2<-lagsarlm(popjuvy.10 ~ skateparks + psych.st+ psych.lt+ impoverishedpc+
                 rehab.st+ genhosp.lt+ rehab.lt+ er.visits+
                 medage.10+ percurban.10+ medhhincome+ personsimpoverished+
                 percwhite.10 + percblack.10 + perchisp.10 +
                 households.10+ singleparenthouseholds.10+ fouryearscollege.15,
               data = raw, W, zero.policy=TRUE)
proc.time() - ptm

summary(sar2)
impacts(sar2, listw = W)
summary(impacts(sar2, listw=W, R=500),zstats=TRUE)


neigh<-poly2nb(jointshape1)
W = nb2listw(neigh, style = "W", zero.policy = TRUE)
#Train spatial models and time em
ptm <- proc.time()
sar3<-lagsarlm(popjuvy.10 ~ skateparks + psych.st+ psych.lt+ impoverishedpc+
                 rehab.st+ genhosp.lt+ rehab.lt+ er.visits+
                 medage.10+ percurban.10+ medhhincome+ personsimpoverished+
                 percwhite.10 + percblack.10 + perchisp.10 +
                 households.10+ singleparenthouseholds.10+ fouryearscollege.15,
               data = raw, W, zero.policy=TRUE)
proc.time() - ptm

summary(sar3)
impacts(sar3, listw = W)
summary(impacts(sar3, listw=W, R=500),zstats=TRUE)

stargazer(sar1, sar2, sar3, no.space=TRUE)

jointshape1$lm<-resid(lm.controldegen)
jointshape1$sar<-resid(sar1)

library(ggplot2)
ggplot(jointshape1) + xlim(125, 67) + ylim(24,50) +  # Set up canvas with outcome variable on y-axis
  geom_sf(aes(fill = sqrt(abs(lm)))) + # Color mapped to abs(residuals)
  scale_fill_gradient(low="white", high="red") + theme(legend.position = "top")




####perclessthanhs.15
fivenn <- knearneigh(coords, k=5, longlat = TRUE)
fivenn.nb <- knn2nb(fivenn)
W <- nb2listw(fivenn.nb, style = "W", zero.policy = TRUE)
#Train spatial models and time em
ptm <- proc.time()
sar1<-lagsarlm(perclessthanhs.15 ~ skateparks + psych.st+ psych.lt+ impoverishedpc+
                 rehab.st+ genhosp.lt+ rehab.lt+ er.visits+
                 medage.10+ percurban.10+ medhhincome+ personsimpoverished+
                 percwhite.10 + percblack.10 + perchisp.10 +
                 households.10+ singleparenthouseholds.10+ fouryearscollege.15,
               data = raw, W, zero.policy=TRUE)
proc.time() - ptm

summary(sar1)
impacts(sar1, listw = W)
summary(impacts(sar1, listw=W, R=500),zstats=TRUE)


fivenn <- knearneigh(coords, k=10, longlat = TRUE)
fivenn.nb <- knn2nb(fivenn)
W <- nb2listw(fivenn.nb, style = "W", zero.policy = TRUE)
#Train spatial models and time em
ptm <- proc.time()
sar2<-lagsarlm(perclessthanhs.15 ~ skateparks + psych.st+ psych.lt+ impoverishedpc+
                 rehab.st+ genhosp.lt+ rehab.lt+ er.visits+
                 medage.10+ percurban.10+ medhhincome+ personsimpoverished+
                 percwhite.10 + percblack.10 + perchisp.10 +
                 households.10+ singleparenthouseholds.10+ fouryearscollege.15,
               data = raw, W, zero.policy=TRUE)
proc.time() - ptm

summary(sar2)
impacts(sar2, listw = W)
summary(impacts(sar2, listw=W, R=500),zstats=TRUE)


neigh<-poly2nb(jointshape1)
W = nb2listw(neigh, style = "W", zero.policy = TRUE)
#Train spatial models and time em
ptm <- proc.time()
sar3<-lagsarlm(perclessthanhs.15 ~ skateparks + psych.st+ psych.lt+ impoverishedpc+
                 rehab.st+ genhosp.lt+ rehab.lt+ er.visits+
                 medage.10+ percurban.10+ medhhincome+ personsimpoverished+
                 percwhite.10 + percblack.10 + perchisp.10 +
                 households.10+ singleparenthouseholds.10+ fouryearscollege.15,
               data = raw, W, zero.policy=TRUE)
proc.time() - ptm

summary(sar3)
impacts(sar3, listw = W)
summary(impacts(sar3, listw=W, R=500),zstats=TRUE)

stargazer(sar1, sar2, sar3, no.space=TRUE)


jointshape1$lm<-resid(lm.alldrop)
jointshape1$sar<-resid(sar1)

library(ggplot2)
ggplot(jointshape1) + xlim(125, 67) + ylim(24,50) +  # Set up canvas with outcome variable on y-axis
  geom_sf(aes(fill = sqrt(abs(lm)))) + # Color mapped to abs(residuals)
  scale_fill_gradient(low="white", high="red") + theme(legend.position = "top")


#ggplot(jointshape1) + xlim(125, 67) + ylim(24,50) +  # Set up canvas with outcome variable on y-axis
#  geom_sf(aes(fill = sqrt(abs(sar - lm)))) + # Color mapped to abs(residuals)
#  scale_fill_gradient(low="cyan", high="red") +
#  theme_bw()













#######Heckit Stuff

#library(spatialprobit)


mspskatemaster$suicides.17.19[is.na(mspskatemaster$suicides.17.19)] <- 0
mspskatemaster$suicidesd <- ifelse(mspskatemaster$suicides.17.19 , 1, 0)

#suicidesd ~ skateparks+ psych.st+ psych.lt+ psych.st*psych.lt+ impoverishedpc+
#  rehab.st+ genhosp.lt+ rehab.lt+ popjuvy.10+ er.visits+
#  pop+ medage.10+ percurban.10+ medhhincome+ personsimpoverished+ 
#  households.10+ singleparenthouseholds.10+ perclessthanhs.15+ fouryearscollege.15,
# data = mspskatemaster[which(mspskatemaster$suicides.17.19>0),], Ws, zero.policy=TRUE


#seleqnsar <- sar_probit_mcmc(y=mspskatemaster$suicides.17.19, X=skateparks+ psych.st+ psych.lt+ psych.st*psych.lt+ impoverishedpc+
#                               rehab.st+ genhosp.lt+ rehab.lt+ popjuvy.10+ er.visits+
#                               pop+ medage.10+ percurban.10+ medhhincome+ personsimpoverished+ 
#                               households.10+ singleparenthouseholds.10+ perclessthanhs.15+ fouryearscollege.15,
#                       W=Ws, ndraw = 1000, burn.in = 100, thinning = 1)

seleqn1 <- glm(suicidesd ~ skateparks+ psych.st+ psych.lt+ psych.st*psych.lt+ impoverishedpc+
                 rehab.st+ genhosp.lt+ rehab.lt+ popjuvy.10+ er.visits+
                 pop+ medage.10+ percurban.10+ medhhincome+ personsimpoverished+ 
                 households.10+ singleparenthouseholds.10+ perclessthanhs.15+ fouryearscollege.15,
               family = "binomial"(link="logit"), data = mspskatemaster)

seleqn2 <- lm(suicidesd ~ skateparks+ psych.st+ psych.lt+ psych.st*psych.lt+ impoverishedpc+
                 rehab.st+ genhosp.lt+ rehab.lt+ popjuvy.10+ er.visits+
                 pop+ medage.10+ percurban.10+ medhhincome+ personsimpoverished+ 
                 households.10+ singleparenthouseholds.10+ perclessthanhs.15+ fouryearscollege.15,
              data = mspskatemaster)

ptm <- proc.time()
seleqn3<-lagsarlm(suicidesd ~ skateparks+ psych.st+ psych.lt+ psych.st*psych.lt+ impoverishedpc+
                    rehab.st+ genhosp.lt+ rehab.lt+ popjuvy.10+ er.visits+
                    pop+ medage.10+ percurban.10+ medhhincome+ personsimpoverished+ 
                    percwhite.10 + percblack.10 + perchisp.10 +
                    households.10+ singleparenthouseholds.10+ fouryearscollege.15,
               data = mspskatemaster, W, zero.policy=TRUE)
proc.time() - ptm

mspskatemaster$IMR1 <- dnorm(seleqn1$linear.predictors)/pnorm(seleqn1$linear.predictors)
mspskatemaster$IMR2 <- dnorm(seleqn2$effects)/pnorm(seleqn2$effects)
mspskatemaster$IMR3 <- dnorm(seleqn3$tary)/pnorm(seleqn3$tary)


ptm <- proc.time()
sar4<-lagsarlm(suicides.17.19 ~ skateparks+ psych.st+ psych.lt+ psych.st*psych.lt+ impoverishedpc+
                 rehab.st+ genhosp.lt+ rehab.lt+ popjuvy.10+ er.visits+
                 pop+ medage.10+ percurban.10+ medhhincome+ personsimpoverished+ 
                 households.10+ singleparenthouseholds.10+ perclessthanhs.15+ fouryearscollege.15 + IMR1,
               data = mspskatemaster, W, zero.policy=TRUE)
proc.time() - ptm



summary(sar4)
impacts(sar4, listw = W)
summary(impacts(sar4, listw=W, R=500),zstats=TRUE)


stargazer(seleqn1, sar4, no.space = TRUE)


#McMillen's Spatial Selection Model ()
###########################
#y1i will be suicides, predicted by skateparks, singleparent, medhhinc, pop, 
#y2i will be psych care centers (need to create an effective measure), predicted by skateparks, singleparent, medhhinc, pop, 
corrgram(msm)

