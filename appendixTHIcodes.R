setwd("~/Desktop/Thesis/data/Agri4cast")

library(raster)
library(data.table) 
library(rJava)
library(reshape2)
library(dplyr)

#define locations
oneyear <- subset(panel, panel$year=='2011') 
locations <- data.frame(oneyear$long,oneyear$lat)
colnames(locations)[1:2] <- c("Longitude", "Latitude") 

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

#working part(select the mean temperadata based on locations)
meant2012 <- data.frame(meant2012) 
class(meant2012$TEMPERATURE_AVG) 
oneday <- subset(meant2012, meant2012$DAY=='20120101') 
oneday <- oneday[,c(1,3,2,6)] 
coordinates(oneday) <- ~ LONGITUDE + LATITUDE 
proj4string(oneday) <- CRS("+init=EPSG:3035")
coordinates(locations) <- ~ Longitude + Latitude
proj4string(locations) <- CRS("+init=EPSG:4326")
plot(oneday)
plot(locations, pch=15, col="red", add=T)
mapView(SpatialPointsDataFrame(coords=locations[,c(1:2)],data= locations, proj4string=CRS("+init=epsg:4326")))  # show the locations of companies in the world map

#the weather data are not gridded
#link company location with the nearest weather point  

library(raster)
# calculate distance matrix between grid cells (oneday) and firms (locations)
distance_matrix <- pointDistance(oneday,locations,lonlat=TRUE)

# use meaningful row (GRID_NO) and column (Company) names
rownames(distance_matrix)<-oneday@data$GRID_NO
colnames(distance_matrix)<-oneyear$company

# create empty vector of NA as placeholders for the GRID_NO with the closest distance to each of the companies
closest_cell<-as.vector(rep(NA,ncol(distance_matrix)))

# loop through the firms and get the GRID_NO with the smallest distance to the firm
for (i in 1:ncol(distance_matrix)){
  closest_cell[i]<-rownames(distance_matrix)[which.min(distance_matrix[,i])]
}

# create an empty matrix with as many columns as companies and as many rows as days in the weather data
meant_firm<-matrix(,nrow=(as.Date("2019-12-31")-as.Date("2011-01-01"))+1,ncol=length(closest_cell))
date_firm <-matrix(,nrow=(as.Date("2019-12-31")-as.Date("2011-01-01"))+1,ncol=length(closest_cell))

# loop through the firms and subset the weather by taking only the GRID_NO that is closest to firm i
# as a check we write the day from the weather data (DAY from meant1119) into the date_firm matrix
# each column of the date_firm matrix should be similar to each other 
for (i in 1:ncol(meant_firm)){
  meant_firm[,i] <- subset(meant1119, GRID_NO == as.numeric(closest_cell[i]))$TEMPERATURE_AVG
  date_firm [,i] <- subset(meant1119, GRID_NO == as.numeric(closest_cell[i]))$DAY
  print(i/ncol(meant_firm)*100)
}
#save the .Rdata

# melt the data
meant_firm <- data.frame(meant_firm)
date_firm <- data.frame(date_firm)
colnames(meant_firm)<-oneyear$company 
meant_firm$DAY<- date_firm$X1 
head(melt(meant_firm[,c(1609,1:1608)], id="DAY"))  
meant_firm_melted<-melt(meant_firm[,c(1609,1:1608)], id="DAY")
colnames(meant_firm_melted)[2:3]<-c("company","meantemperature")



# replicate for vapour pressure
# create an empty matrix with as many columns as companies and as many rows as days in the weather data
vap1119 <- fread("vap2011-2019.csv") #read data
vap_firm<-matrix(,nrow=(as.Date("2019-12-31")-as.Date("2011-01-01"))+1,ncol=length(closest_cell))

# loop through the firms and subset the weather by taking only the GRID_NO that is closest to firm i
for (i in 1:ncol(vap_firm)){
  vap_firm[,i] <- subset(vap1119, GRID_NO == as.numeric(closest_cell[i]))$VAPOURPRESSURE
  print(i/ncol(vap_firm)*100)
}

# melt the data
vap_firm <- data.frame(vap_firm)
colnames(vap_firm)<-oneyear$company
vap_firm$DAY<- date_firm$V1
head(melt(vap_firm[,c(1609,1:1608)], id="DAY"))  
vap_firm_melted<-melt(vap_firm[,c(1609,1:1608)], id="DAY")
colnames(vap_firm_melted)[2:3]<-c("company","vapourpressure")

# calculate humidity from vapour pressure and mean temperature
##merge mean temperature and vapor pressure
rh_firm <- merge(vap_firm_melted,meant_firm_melted, by=c("DAY","company"))
rh_firm$sat_vp <- 6.11 * exp(2.5*10^6/461.52*(1/273.15 - 1/(rh_firm$meantemperature+273.15)))

#relative humidity
rh_firm$rh <- rh_firm$vapourpressure/rh_firm$sat_vp *100 
write.csv(rh_firm, "~/Desktop/Thesis/data/Agri4cast/rhlocations.csv")

# calculcate daily max THI from daily max temperature and humidity
# create an empty matrix with as many columns as companies and as many rows as days in the weather data
maxt1119 <- fread("maxt2011-2019.csv")
maxt_firm<-matrix(,nrow=(as.Date("2019-12-31")-as.Date("2011-01-01"))+1,ncol=length(closest_cell))

for (i in 1:ncol(maxt_firm)){
  maxt_firm[,i] <- subset(maxt1119, GRID_NO == as.numeric(closest_cell[i]))$TEMPERATURE_MAX
  print(i/ncol(maxt_firm)*100)
}

# melt the data
maxt_firm <- data.frame(maxt_firm)
colnames(maxt_firm)<-oneyear$company
maxt_firm$DAY<- date_firm$X1
head(melt(vap_firm[,c(1609,1:1608)], id="DAY"))  
maxt_firm_melted<-melt(maxt_firm[,c(1609,1:1608)], id="DAY")
colnames(maxt_firm_melted)[2:3]<-c("company","maxtemperature")

#calculate dailt max THI
#merge max temperature and RH
THI_firm <- merge (maxt_firm_melted, rh_firm, by=c("company","DAY"))
###THI=(1.8T+32)-(0.55-0.0055RH)*(1.8T-26)
THI_firm$maxTHI <- (1.8*THI_firm$maxtemperature+32)-(0.55-0.0055*THI_firm$rh)*(1.8*THI_firm$maxtemperature-26)
write.csv(THI_firm, "~/Desktop/Thesis/data/Agri4cast/THIlocations.csv")

#  count the no of days above THI threshold 
THI_firm$year <- substr(THI_firm$DAY,1,4)
THI_above75 <- subset(THI_firm,maxTHI>75)
THI_above75$newcolumn <- 1
THI_days75<-aggregate(THI_above75$newcolumn, by=list(THI_above75$company,THI_above75$year), FUN=sum) #sum the number of days that daily max THI above 75 in each year
colnames(THI_days75)<-c("company","year","THI_DAYS75") 

THI_above80 <- subset(THI_firm,maxTHI>80)
THI_above80$newcolumn <- 1
THI_days80<-aggregate(THI_above80$newcolumn, by=list(THI_above80$company,THI_above80$year), FUN=sum) #sum the number of days that daily max THI above 75 in each year
colnames(THI_days80)<-c("company","year","THI_DAYS80") 

THI_above85 <- subset(THI_firm,maxTHI>85)
THI_above85$newcolumn <- 1
THI_days85<-aggregate(THI_above85$newcolumn, by=list(THI_above85$company,THI_above85$year), FUN=sum) #sum the number of days that daily max THI above 75 in each year
colnames(THI_days85)<-c("company","year","THI_DAYS85") 



#  merge all the data together to get the final data frame for the regression 
THI_days7580<-  merge (THI_days75,THI_days80, by=c("company","year"),all=TRUE)
THI_days758085<-  merge (THI_days7580,THI_days85, by=c("company","year"),all=TRUE)
panel_final<-  merge (THI_days758085,panel, by=c("company","year"),all=TRUE)
panel_final$THI_DAYS75[is.na(panel_final$THI_DAYS75)] <- 0 
panel_final$THI_DAYS80[is.na(panel_final$THI_DAYS80)] <- 0
panel_final$THI_DAYS85[is.na(panel_final$THI_DAYS85)] <- 0
panel_final<-panel_final[,c(1,2,3,4,5,16,6:15)]






