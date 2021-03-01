setwd("~/Desktop/Thesis/data/EOBS ")

# load libraries

library(rJava)
library(reshape2)
library(OSMscale)
library(sp)
library(mapview)
library(stringr)
library(raster)
library(data.table) #for fread
#read orbis data

dat_finance<-read.csv2("Export 07_12_2020 09_27 (1).csv")

# 1. subset orbis data based on lat long availability
# 2. create new dataframe in long (instead of wide) format for turnover
# 3. do the same for ROE
# 4. merge the two together and then merge with the other info from data_finance (lat, long, employees etc.)
# 5. convert degree coordinates into decimal coordinates using OSMsclae()
# 6. find the weatehr data at the firms location and subset the weather 

dat_financell <- dat_finance[-which(dat_finance$Latitude == ""), ]#delete the companies without lat and long
dat_financell$company <- paste(dat_financell$X, dat_financell$Company.name.Latin.alphabet, sep="_")# I do this because there are some companies sharing the same name
colnames(dat_financell)[20:28]<-2019:2011# change the colume names to 2019 to 2011(turnover)

head(dat_financell[,c(50,20:28)])#查看第 50 20～28列数据（前6行）
head(melt(dat_financell[,c(50,20:28)], id="company"))#原本横轴的2011-2019变成纵轴
turnover_melted<-melt(dat_financell[,c(50,20:28)], id="company")

colnames(turnover_melted)[2:3]<-c("year","turnover")
head(turnover_melted)
#repeat for ROE
colnames(dat_financell)[31:39]<-2019:2011#(ROE)
head(dat_financell[,c(50,31:39)])
head(melt(dat_financell[,c(50,31:39)], id="company"))
ROE_melted<-melt(dat_financell[,c(50,31:39)], id="company")
colnames(ROE_melted)[2:3]<-c("year","ROE")
head(ROE_melted)

#merge ROE and turnover
turnover_ROE <- merge (ROE_melted, turnover_melted, by=c("company","year"))
keep_finance <- dat_financell[,c(50,8:10,13:15)]#delete the irrelevant variables
#merge profit data and other info
profit <- merge (turnover_ROE, keep_finance, by="company")
#convert degree coordinates into decimal coordinates using OSMsclae()

profit$Longitude <- str_replace(profit$Longitude, "\"" , "'")#delete \ in longitude data
profit$Latitude <- str_replace(profit$Latitude,"\"","'")

profit$Longitude <- str_replace_all(profit$Longitude, " " , "")#delete the space in longitude data
profit$Latitude <- str_replace_all(profit$Latitude," ","")


profit <- cbind(profit,degree(Latitude, Longitude, data=profit, digits=15)) # convert the long and lat data into decimal coordinates



plot(SpatialPoints(profit[,c(12:11)])) # plot the location of companies
typeof(profit) # check the type of the profit data

mapView(SpatialPointsDataFrame(coords=profit[,c(12:11)],data= profit[,c(1:10)], proj4string=CRS("+init=epsg:4326")))  # show the locations of companies in the world map

str(profit) #structure, check classification of variables,紧凑的显示对象内部结构,即对象里有什么

# 1.read in weather data 
# 2.for importing and matching the .nc files ask Maarten for his code 
# 3.for importing and matching the agri4cast ask stephanie vuille for the new code 
# 4.ask stephanie for the THI calculation code

# sum of precipitation in growing seasons in each year for each firm 
precipitation <- brick("rr_precipitation2011-2020_v22.0e.nc") # load the .nc file for precipitation
oneyear <- subset(profit, profit$year=='2011') # choose one year to extract the company locations 
locations <- data.frame(oneyear$long,oneyear$lat) # create a dataset only containing lonitude and latitude
colnames(locations)[1:2] <- c("Longitude", "Latitude") #change the colume names to longitude and latitude
precipitationlocations5 <- raster::extract(precipitation,head(locations,5))#test first 5 lines if the command is correct
precipitationlocations <- raster::extract(precipitation,locations) # extract the precipitation data based on the locations
write.csv(precipitationlocations, "~/Desktop/Thesis/data /EOBS /preclocations.csv") # save the ectracted precipitation data in csv files
precipitationlocations <- fread ("preclocations.csv",dec=",") #read the precipitation data
precipitationlocations$company<-oneyear$company # add a new colume named company
precipitationlocations <- precipitationlocations[,-1] #delete the useless colume
precipitationlocations_melted<-(reshape2::melt(precipitationlocations, id="company")) #melt the precipitation data by company

precipitationlocations_melted$year<-substr(precipitationlocations_melted$variable,2,5) # create a colume for year#把第二列的年月日拆分成年和月 2～5是2011所在的位置，第2至第5
precipitationlocations_melted$month<-substr(precipitationlocations_melted$variable,7,8) # create a colume for month 
precipitationlocations_melted <- subset(precipitationlocations_melted, month == "04" | month == "05" |month == "06" |month == "07" |month == "08" |month == "09" |month == "10") # keep the precipitation data in growing seasons
precipitationlocations_melted$value <- as.numeric(precipitationlocations_melted$value) # change the type of value to numeric
precipitationyear<-aggregate(precipitationlocations_melted$value, by=list(precipitationlocations_melted$company,precipitationlocations_melted$year), FUN=sum) #sum the precipitation in each year
colnames(precipitationyear)<-c("company","year","precipitation") #change the colume names
panel<-merge(profit,precipitationyear, by=c("company","year")) #merge the precipitation data with profit data
panel<-arrange(panel, company) #排序
