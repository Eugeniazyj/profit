# Part one: basic regression
library(fixest)
library(sjPlot)
library(ggplot2)
library(dplyr)
#drop the observations that turnover < 0
panel_final<- subset(panel_final, panel_final$company != "2707._CASEIFICIO RISORTA SRL")
panel_final<- subset(panel_final, panel_final$company != "3202._SICILMILK S.R.L.")
mapView(SpatialPointsDataFrame(coords=panel_final[,c(16:15)],data= panel_final[,c(1:14)], proj4string=CRS("+init=epsg:4326")))  # show the locations of companies in the world map

panel_final$turnover <- as.numeric(panel_final$turnover) 
panel_final$ROE <- as.numeric(gsub(",",".",panel_final$ROE)) 
panel_final$Number.of.employees.Last.avail..yr <- as.numeric(panel_final$Number.of.employees.Last.avail..yr)

#summary statistics
summary(panel_final$turnover)
hist(panel_final$turnover)
hist(log(panel_final$turnover*1000),col = "lightgreen", xlab="Natural logarithm of turnover", main="Histogram of natural logarithm of turnover")
summary(panel_final$ROE) 
hist(panel_final$ROE,,xlim = c(-100,100),breaks=200,col = "lightgreen",xlab="ROE", main="Histogram of ROE")
ROEleft<- subset(panel_final, panel_final$ROE <= -100)
ROEright<- subset(panel_final, panel_final$ROE >= 100)
summary(panel_final$Number.of.employees.Last.avail..yr)
hist(panel_final$Number.of.employees.Last.avail..yr,xlim = c(0,500),col = "lightgreen")
panel_final$companysize <-if_else(panel_final$Number.of.employees.Last.avail..yr <= 9, 1, if_else(panel_final$Number.of.employees.Last.avail..yr <= 50, 2, if_else(panel_final$Number.of.employees.Last.avail..yr<= 250, 3, 4)))
summary(panel_final$companysize)
hist(panel_final$companysize,col = "lightgreen",  main = "Histogram of firm sizes of dairy processors", xlab = "Firm size")
summary(panel_final$THI_DAYS75)
summary(panel_final$THI_DAYS80)
summary(panel_final$THI_DAYS85)
summary(panel_final$precipitation)

#turnover
#threshold=75
panelregressionTO75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company") , panel_final)#do the fixed effects regression
summary(panelregressionTO75)
plot_model(panelregressionTO75,type = "pred",terms = "precipitation[all]")
#threshold=80
panelregressionTO80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company") , panel_final)#do the fixed effects regression
summary(panelregressionTO80)
plot_model(panelregressionTO80,type = "pred",terms = "precipitation[all]")
#threshold=85
panelregressionTO85<- feols(log(turnover) ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company") , panel_final)#do the fixed effects regression
summary(panelregressionTO85)
plot_model(panelregressionTO85,type = "pred",terms = "precipitation[all]")

#ROE
panelregressionROE75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panel_final)#NOTE: 2,403 observations removed because of NA values (LHS: 735, RHS: 1,811).
summary(panelregressionROE75)
plot_model(panelregressionROE75,type = "pred",terms = "precipitation[all]")

panelregressionROE80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panel_final)#NOTE: 2,403 observations removed because of NA values (LHS: 735, RHS: 1,811).
summary(panelregressionROE80)
plot_model(panelregressionROE80,type = "pred",terms = "precipitation[all]")

panelregressionROE85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panel_final)#NOTE: 2,403 observations removed because of NA values (LHS: 735, RHS: 1,811).
summary(panelregressionROE85)
plot_model(panelregressionROE85,type = "pred",terms = "precipitation[all]")

#Part two:robust check
panelrobust <- panel_final
panelrobust$turnover <- as.numeric(panelrobust$turnover)
panelrobust$ROE <- as.numeric(gsub(",",".",panelrobust$ROE)) 
summary(panelrobust$long)
paneleast <- subset(panelrobust, panelrobust$long > 15.79)
panelwest <- subset(panelrobust, panelrobust$long < 15.79)

summary(panelrobust$lat)
panelnorth <- subset(panelrobust,panelrobust$lat > 45.4)
panelsouth <- subset(panelrobust,panelrobust$lat < 45.4)

#turnover, east 
panelregressionTOeast75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),paneleast)
summary(panelregressionTOeast75)
plot_model(panelregressionTOeast75,type = "pred",terms = "precipitation[all]", title   ="Predicted values of turnover (East)")

panelregressionTOeast80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),paneleast)
summary(panelregressionTOeast80)
plot_model(panelregressionTOeast80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (East)")

panelregressionTOeast85<- feols(log(turnover) ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),paneleast)
summary(panelregressionTOeast85)
plot_model(panelregressionTOeast85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (East)")

#turnover,west
panelregressionTOwest75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelwest)
summary(panelregressionTOwest75)
plot_model(panelregressionTOwest75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (West)")

panelregressionTOwest80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelwest)
summary(panelregressionTOwest80)
plot_model(panelregressionTOwest80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (West)")

panelregressionTOwest85<- feols(log(turnover) ~ THI_DAYS85 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelwest)
summary(panelregressionTOwest85)
plot_model(panelregressionTOwest85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (West)")

#turnover,north
panelregressionTOnorth75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelnorth)
summary(panelregressionTOnorth75)
plot_model(panelregressionTOnorth75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (North)")

panelregressionTOnorth80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelnorth)
summary(panelregressionTOnorth80)
plot_model(panelregressionTOnorth80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (North)")

panelregressionTOnorth85<- feols(log(turnover)~ THI_DAYS85 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelnorth)
summary(panelregressionTOnorth85)
plot_model(panelregressionTOnorth85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (North)")

#turnover,south
panelregressionTOsouth75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelsouth)
summary(panelregressionTOsouth75)
plot_model(panelregressionTOsouth75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (South)")

panelregressionTOsouth80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelsouth)
summary(panelregressionTOsouth80)
plot_model(panelregressionTOsouth80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (South)")

panelregressionTOsouth85<- feols(log(turnover)~ THI_DAYS85 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelsouth)
summary(panelregressionTOsouth85)
plot_model(panelregressionTOsouth85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (South)")

#ROE, east
panelregressionROEeast75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),paneleast)
summary(panelregressionROEeast75)
plot_model(panelregressionROEeast75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (East)")

panelregressionROEeast80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),paneleast)
summary(panelregressionROEeast80)
plot_model(panelregressionROEeast80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (East)")

panelregressionROEeast85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),paneleast)
summary(panelregressionROEeast85)
plot_model(panelregressionROEeast85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (East)")

#ROE,west
panelregressionROEwest75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelwest)
summary(panelregressionROEwest75)
plot_model(panelregressionROEwest75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (West)")

panelregressionROEwest80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelwest)
summary(panelregressionROEwest80)
plot_model(panelregressionROEwest80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (West)")

panelregressionROEwest85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelwest)
summary(panelregressionROEwest85)
plot_model(panelregressionROEwest85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (West)")

#ROE,north
panelregressionROEnorth75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelnorth)
summary(panelregressionROEnorth75)
plot_model(panelregressionROEnorth75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (North)")

panelregressionROEnorth80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelnorth)
summary(panelregressionROEnorth80)
plot_model(panelregressionROEnorth80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (North)")

panelregressionROEnorth85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelnorth)
summary(panelregressionROEnorth85)
plot_model(panelregressionROEnorth85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (North)")

#ROE,south
panelregressionROEsouth75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelsouth)
summary(panelregressionROEsouth75)
plot_model(panelregressionROEsouth75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (South)")

panelregressionROEsouth80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelsouth)
summary(panelregressionROEsouth80)
plot_model(panelregressionROEsouth80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (South)")

panelregressionROEsouth85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelsouth)
summary(panelregressionROEsouth85)
plot_model(panelregressionROEsouth85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (South)")


#Part Three:
#Run the same model again for subsamples based on the firm's employee size class
panel_final$Number.of.employees.Last.avail..yr <- as.numeric(panel_final$Number.of.employees.Last.avail..yr)
panel_final$companysize <-if_else(panel_final$Number.of.employees.Last.avail..yr <= 9, 1, if_else(panel_final$Number.of.employees.Last.avail..yr <= 50, 2, if_else(panel_final$Number.of.employees.Last.avail..yr<= 250, 3, 4)))

#turnover,size1
panelsize1 <- subset(panel_final, panel_final$companysize == 1)
panel1regressionTO75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company,  cluster = c("year","company"),panelsize1)
summary(panel1regressionTO75)
plot_model(panel1regressionTO75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (micro)")

panel1regressionTO80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company,  cluster = c("year","company"),panelsize1)
summary(panel1regressionTO80)
plot_model(panel1regressionTO80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (micro)")

panel1regressionTO85<- feols(log(turnover) ~ THI_DAYS85 + precipitation + precipitation^2 | year+company,  cluster = c("year","company"),panelsize1)
summary(panel1regressionTO85)
plot_model(panel1regressionTO85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (micro)")

#turnover, size2
panelsize2 <- subset(panel_final, panel_final$companysize == 2)
panel2regressionTO75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company,  cluster = c("year","company"),panelsize2)
summary(panel2regressionTO75)
plot_model(panel2regressionTO75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (small)")

panel2regressionTO80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company,  cluster = c("year","company"),panelsize2)
summary(panel2regressionTO80)
plot_model(panel2regressionTO80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (small)")

panel2regressionTO85<- feols(log(turnover)~ THI_DAYS85 + precipitation + precipitation^2 | year+company,  cluster = c("year","company"),panelsize2)
summary(panel2regressionTO85)
plot_model(panel2regressionTO85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (small)")

#turnover, size3
panelsize3 <- subset(panel_final, panel_final$companysize == 3)
panel3regressionTO75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelsize3)
summary(panel3regressionTO75)
plot_model(panel3regressionTO75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (medium)")

panel3regressionTO80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelsize3)
summary(panel3regressionTO80)
plot_model(panel3regressionTO80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (medium)")

panel3regressionTO85<- feols(log(turnover) ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelsize3)
summary(panel3regressionTO85)
plot_model(panel3regressionTO85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (medium)")

#turnover, size4
panelsize4 <- subset(panel_final, panel_final$companysize == 4)
panel4regressionTO75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize4)
summary(panel4regressionTO75)
plot_model(panel4regressionTO75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (large)")

panel4regressionTO80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize4)
summary(panel4regressionTO80)
plot_model(panel4regressionTO80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (large)")

panel4regressionTO85<- feols(log(turnover) ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize4)
summary(panel4regressionTO85)
plot_model(panel4regressionTO85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of turnover (large)")

#ROE. size1
panel1regressionROE75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize1)
summary(panel1regressionROE75)
plot_model(panel1regressionROE75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (micro)")

panel1regressionROE80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize1)
summary(panel1regressionROE80)
plot_model(panel1regressionROE80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (micro)")

panel1regressionROE85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize1)
summary(panel1regressionROE85)
plot_model(panel1regressionROE85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (micro)")

#ROE,size2
panel2regressionROE75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize2)
summary(panel2regressionROE75)
plot_model(panel2regressionROE75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (small)")

panel2regressionROE80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize2)
summary(panel2regressionROE80)
plot_model(panel2regressionROE80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (small)")

panel2regressionROE85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize2)
summary(panel2regressionROE85)
plot_model(panel2regressionROE85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (small)")

#ROE,size3
panel3regressionROE75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize3)
summary(panel3regressionROE75)
plot_model(panel3regressionROE75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (medium)")

panel3regressionROE80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize3)
summary(panel3regressionROE80)
plot_model(panel3regressionROE80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (medium)")

panel3regressionROE85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize3)
summary(panel3regressionROE85)
plot_model(panel3regressionROE85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (medium)")

#ROE,size4
panel4regressionROE75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize4)
summary(panel4regressionROE75)
plot_model(panel4regressionROE75,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (large)")

panel4regressionROE80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize4)
summary(panel4regressionROE80)
plot_model(panel4regressionROE80,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (large)")

panel4regressionROE85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize4)
summary(panel4regressionROE85)
plot_model(panel4regressionROE85,type = "pred",terms = "precipitation[all]",title   ="Predicted values of ROE (large)")
