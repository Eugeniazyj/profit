setwd("~/Desktop/Thesis/data /Agri4cast")

library(raster)
library(data.table) #for fread
#define locations
oneyear <- subset(panel, panel$year=='2011') 
locations <- data.frame(oneyear$long,oneyear$lat)
colnames(locations)[1:2] <- c("Longitude", "Latitude") 

#转换成2011-2019
vap1019 <- read.csv("meant2010-2019.csv", sep=";")
vap1019$year <- substr(vap1019$DAY,1,4)
vap1119 <- subset(vap1019, year != "2010")
vap1119 <- vap1119[,-1]
vap1119 <- vap1119[,-7]
write.csv(vap1119, "~/Desktop/Thesis/data /Agri4cast/vap2011-2019.csv")
meant1019 <- read.csv("meant2010-2019.csv", sep=";")
meant1019$year <- substr(meant1019$DAY,1,4)
meant1119 <- subset(meant1019, year != "2010")
meant1119 <- meant1119[,-7]
write.csv(meant1119, "~/Desktop/Thesis/data /Agri4cast/meant2011-2019.csv")
maxt1019 <- read.csv("maxt2010-2019.csv", sep=";")
maxt1019$year <- substr(maxt1019$DAY,1,4)
maxt1119 <- subset(maxt1019, year != "2010")
maxt1119 <- maxt1119[,-1]
maxt1119 <- maxt1119[,-7]
write.csv(maxt1119, "~/Desktop/Thesis/data /Agri4cast/maxt2011-2019.csv")


##match the weather data and firm locations
# select only one year for mean temperature data
meant1119 <- fread("meant2011-2019.csv")
meant1119 <- meant1119[,-1]
meant1119$year <- substr(meant1119$DAY,1,4)
meant2011 <- subset(meant1119, year == "2011") #number of obervations:8177064
meant2012 <- subset(meant1119, year == "2012") #number of obervations:8199435
meant2013 <- subset(meant1119, year == "2013") #number of obervations:8177039
meant2014 <- subset(meant1119, year == "2014") #number of obervations:8177039
meant2015 <- subset(meant1119, year == "2015") #number of obervations:8177039
meant2016 <- subset(meant1119, year == "2016") #number of obervations:8199435
meant2017 <- subset(meant1119, year == "2017") #number of obervations:8177039
meant2018 <- subset(meant1119, year == "2018") #number of obervations:8177034
meant2019 <- subset(meant1119, year == "2019") #number of obervations:8177039
write.csv(meant1119, "~/Desktop/Thesis/data /Agri4cast/maxt2011-2019.csv")

#select the mean temperadata based on locations
meant2012 <- data.frame(meant2012)
coordinates(meant2012) <- ~ LONGITUDE + LATITUDE # I choose 2012 because the the number of observations is largest
proj4string(meant2012) <- CRS("+init=epsg:3035")
coordinates(locations) <- ~ Longitude + Latitude
proj4string(locations) <- CRS("+init=epsg:4326")
locations <- spTransform(locations,"+proj=epsg: 3035")

#import weather data(mean temperature, vapour pressure, max temperature) to calculate max THI
meant1019 <- read.csv("meant2010-2019.csv")

#select the mean temperadata based on locations
coordinates(meant1019) <- ~ LONGITUDE + LATITUDE
proj4string(meant1019) <- CRS("+init=epsg:3035")
coordinates(locations) <- ~ Longitude + Latitude
proj4string(locations) <- CRS("+init=epsg:4326")
locations <- spTransform(locations,"+proj=epsg: 3035")


meant1019 <- read.csv("meant2010-2019.csv")
maxt1019 <- read.csv("maxt2010-2019.csv")
# However, the size of the data files is too large
#So, import weather data based on the locations(coordinates),How?
#plan B: import the yearly seperated data?



#calculate relative humidity
##merge mean temperature and vapor pressure
rhlocations <- merge(vaplocations,meantlocations, by=c("GRID_NO","LATITUDE","LONGITUDE","ALTITUDE","DAY"))
#relative humidity = water vapor pressure/saturation vapor pressure *100
#saturation vapor pressure(temperature) = saturation vapor pressure (temp_0) * exp(L/R_w*(1/temp_0 - 1/temp))
# = 6.11 hPA * exp(2.5*10^6 J/kg / 461.52 J/kgK * (1/273.15K - 1/temp [K] --> temp in Kelvin))
# 0 ?C = 273.15 K

#saturation vapor pressure
rhlocations$sat_vp <- 6.11 * exp(2.5*10^6/461.52*(1/273.15 - 1/(meantlocations$TEMPERATURE_AVG+273.15)))

#relative humidity
rhlocations$rh <- rhlocations$VAPOURPRESSURE/rhlocations$sat_vp *100
# some RH is larger than 100%, how to deal with them???
write.csv(rhlocations, "~/Desktop/Thesis/data /Agri4cast/rhlocations.csv")

###calculate daily max THI 
#import max temperature 
maxtlocations <- 
  #merge max temperature and RH
  THIlocations <- merge (maxtlocations, rhlocations, by=c("GRID_NO","LATITUDE","LONGITUDE","ALTITUDE","DAY"))
###THI=(1.8T+32)-(0.55-0.0055RH)*(1.8T-26)
THIlocations$maxTHI <- (1.8*maxtlocations$TEMPERATURE_MAX+32)-(0.55-0.0055*rhlocations$rh)*(1.8*maxtlocations$TEMPERATURE_MAX-26)
write.csv(THIlocations, "~/Desktop/Thesis/data /Agri4cast/THIlocations.csv")

#calculate the number of max THI above 75 per year 
#...
fTHIyear <-
  colnames(fTHIyear)<-c("company","year","fTHIyear") 
panel <- merge(panel,fTHI, by=c("company","year"))