setwd("~/Desktop/Thesis/data/EOBS ")

# load libraries
library(rJava)
library(reshape2)
library(OSMscale)
library(sp)
library(mapview)
library(stringr)
library(raster)
library(data.table) 

#read orbis data
dat_finance<-read.csv2("Export 07_12_2020 09_27 (1).csv")
dat_financell <- dat_finance[-which(dat_finance$Latitude == ""), ]
dat_financell$company <- paste(dat_financell$X, dat_financell$Company.name.Latin.alphabet, sep="_")
colnames(dat_financell)[20:28]<-2019:2011

head(dat_financell[,c(50,20:28)])
head(melt(dat_financell[,c(50,20:28)], id="company"))
turnover_melted<-melt(dat_financell[,c(50,20:28)], id="company")

colnames(turnover_melted)[2:3]<-c("year","turnover")
head(turnover_melted)

#repeat for ROE
colnames(dat_financell)[31:39]<-2019:2011
head(dat_financell[,c(50,31:39)])
head(melt(dat_financell[,c(50,31:39)], id="company"))
ROE_melted<-melt(dat_financell[,c(50,31:39)], id="company")
colnames(ROE_melted)[2:3]<-c("year","ROE")
head(ROE_melted)

#merge ROE and turnover
turnover_ROE <- merge (ROE_melted, turnover_melted, by=c("company","year"))
keep_finance <- dat_financell[,c(50,8:10,13:15)]
#merge profit data and other info
profit <- merge (turnover_ROE, keep_finance, by="company")

#convert degree coordinates into decimal coordinates using OSMsclae()
profit$Longitude <- str_replace(profit$Longitude, "\"" , "'")
profit$Latitude <- str_replace(profit$Latitude,"\"","'")

profit$Longitude <- str_replace_all(profit$Longitude, " " , "")
profit$Latitude <- str_replace_all(profit$Latitude," ","")

profit <- cbind(profit,degree(Latitude, Longitude, data=profit, digits=15)) 

plot(SpatialPoints(profit[,c(12:11)])) 
typeof(profit) 

mapView(SpatialPointsDataFrame(coords=profit[,c(12:11)],data= profit[,c(1:10)], proj4string=CRS("+init=epsg:4326")))  # show the locations of companies in the world map

str(profit) 

# sum of precipitation in growing seasons in each year for each firm 
precipitation <- brick("rr_precipitation2011-2020_v22.0e.nc") 
oneyear <- subset(profit, profit$year=='2011') 
locations <- data.frame(oneyear$long,oneyear$lat) 
colnames(locations)[1:2] <- c("Longitude", "Latitude") 
precipitationlocations5 <- raster::extract(precipitation,head(locations,5))
precipitationlocations <- raster::extract(precipitation,locations) 
write.csv(precipitationlocations, "~/Desktop/Thesis/data /EOBS /preclocations.csv") 
precipitationlocations <- fread ("preclocations.csv",dec=",") 
precipitationlocations$company<-oneyear$company 
precipitationlocations <- precipitationlocations[,-1] 
precipitationlocations_melted<-(reshape2::melt(precipitationlocations, id="company")) 

precipitationlocations_melted$year<-substr(precipitationlocations_melted$variable,2,5) 
precipitationlocations_melted$month<-substr(precipitationlocations_melted$variable,7,8)  
precipitationlocations_melted <- subset(precipitationlocations_melted, month == "04" | month == "05" |month == "06" |month == "07" |month == "08" |month == "09" |month == "10") # keep the precipitation data in growing seasons
precipitationlocations_melted$value <- as.numeric(precipitationlocations_melted$value) 
precipitationyear<-aggregate(precipitationlocations_melted$value, by=list(precipitationlocations_melted$company,precipitationlocations_melted$year), FUN=sum) #sum the precipitation in each year
colnames(precipitationyear)<-c("company","year","precipitation") 
panel<-merge(profit,precipitationyear, by=c("company","year")) 
panel<-arrange(panel, company) 
