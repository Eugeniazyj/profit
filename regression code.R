# next steps
# 1. read through the documentation of fixest
# https://www.rdocumentation.org/packages/fixest/versions/0.8.0
# 2. get familiar with feols
# take ROE as dependent variable and THI as well as precipitation and precipitation squared (PREC+I(PREC^2) as independent variables
# include a company and a year fixed effect
# and cluster by company and year
# you can do this all with feols
# 3. look at results
# you get the marginal impact of THI directly from the regression output
# use summary(regression_output) to get it
# for rainfall you need a marginal effects plot
# see here https://www.patrickbaylis.com/blog/2018-04-12-svycontrasts/


# Part one: basic regression
library(fixest)
library(sjPlot)
library(ggplot2)
library(dplyr)
#drop the observations that turnover < 0
panel_final<- subset(panel_final, panel_final$company != "2707._CASEIFICIO RISORTA SRL")
panel_final<- subset(panel_final, panel_final$company != "3202._SICILMILK S.R.L.")
panel_final$turnover <- as.numeric(panel_final$turnover) #change the type of turnover from character to numeric
panel_final$ROE <- as.numeric(gsub(",",".",panel_final$ROE)) # change "," to "." in order to make ROE numeric
panel_final$Number.of.employees.Last.avail..yr <- as.numeric(panel_final$Number.of.employees.Last.avail..yr)

#summary statistics
summary(panel_final$turnover)
hist(panel_final$turnover)
hist(log(panel_final$turnover),col = "lightgreen")
summary(panel_final$ROE) 
hist(panel_final$ROE,xlim = c(-300,200),col = "lightgreen")
summary(panel_final$Number.of.employees.Last.avail..yr)
hist(panel_final$Number.of.employees.Last.avail..yr,xlim = c(0,500),col = "lightgreen")
panel_final$companysize <-if_else(panel_final$Number.of.employees.Last.avail..yr <= 9, 1, if_else(panel_final$Number.of.employees.Last.avail..yr <= 50, 2, if_else(panel_final$Number.of.employees.Last.avail..yr<= 250, 3, 4)))
summary(panel_final$companysize)
hist(panel_final$companysize)
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

# summary(panelregressionTO, se="twoway") #we do not put clustered se in summary but put in regression
# or: summary(panelregressionTO, cluster = ~year+company)
# or: summary(panelregression, cluster = panelregression[,c("year","company")])

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
###How to tell whether it is robust or not? 
panelrobust <- panel_final
panelrobust$turnover <- as.numeric(panelrobust$turnover)
panelrobust$ROE <- as.numeric(gsub(",",".",panelrobust$ROE)) 
summary(panelrobust$long)
#paneleast <- subset(panelrobust, panelrobust$long > 14.2) # use the same standard as Maarten
#panelwest <- subset(panelrobust, panelrobust$long < 14.2)
paneleast <- subset(panelrobust, panelrobust$long > 15.79)
panelwest <- subset(panelrobust, panelrobust$long < 15.79)

summary(panelrobust$lat)
#panelnorth <- subset(panelrobust,panelrobust$lat > 46.75)
#panelsouth <- subset(panelrobust,panelrobust$lat < 46.75)
panelnorth <- subset(panelrobust,panelrobust$lat > 45.4)
panelsouth <- subset(panelrobust,panelrobust$lat < 45.4)

#turnover, east 
panelregressionTOeast75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),paneleast)
summary(panelregressionTOeast75)
plot_model(panelregressionTOeast75,type = "pred",terms = "precipitation[all]")

panelregressionTOeast80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),paneleast)
summary(panelregressionTOeast80)
plot_model(panelregressionTOeast80,type = "pred",terms = "precipitation[all]")

panelregressionTOeast85<- feols(log(turnover) ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),paneleast)
summary(panelregressionTOeast85)
plot_model(panelregressionTOeast85,type = "pred",terms = "precipitation[all]")

#turnover,west
panelregressionTOwest75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelwest)
summary(panelregressionTOwest75)
plot_model(panelregressionTOwest75,type = "pred",terms = "precipitation[all]")

panelregressionTOwest80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelwest)
summary(panelregressionTOwest80)
plot_model(panelregressionTOwest80,type = "pred",terms = "precipitation[all]")

panelregressionTOwest85<- feols(log(turnover) ~ THI_DAYS85 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelwest)
summary(panelregressionTOwest85)
plot_model(panelregressionTOwest85,type = "pred",terms = "precipitation[all]")

#turnover,north
panelregressionTOnorth75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelnorth)
summary(panelregressionTOnorth75)
plot_model(panelregressionTOnorth75,type = "pred",terms = "precipitation[all]")

panelregressionTOnorth80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelnorth)
summary(panelregressionTOnorth80)
plot_model(panelregressionTOnorth80,type = "pred",terms = "precipitation[all]")

panelregressionTOnorth85<- feols(log(turnover)~ THI_DAYS85 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelnorth)
summary(panelregressionTOnorth85)
plot_model(panelregressionTOnorth85,type = "pred",terms = "precipitation[all]")

#turnover,south
panelregressionTOsouth75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelsouth)
summary(panelregressionTOsouth75)
plot_model(panelregressionTOsouth75,type = "pred",terms = "precipitation[all]")

panelregressionTOsouth80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelsouth)
summary(panelregressionTOsouth80)
plot_model(panelregressionTOsouth80,type = "pred",terms = "precipitation[all]")

panelregressionTOsouth85<- feols(log(turnover)~ THI_DAYS85 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelsouth)
summary(panelregressionTOsouth85)
plot_model(panelregressionTOsouth85,type = "pred",terms = "precipitation[all]")

#ROE, east
panelregressionROEeast75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),paneleast)
summary(panelregressionROEeast75)
plot_model(panelregressionROEeast75,type = "pred",terms = "precipitation[all]")

panelregressionROEeast80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),paneleast)
summary(panelregressionROEeast80)
plot_model(panelregressionROEeast80,type = "pred",terms = "precipitation[all]")

panelregressionROEeast85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),paneleast)
summary(panelregressionROEeast85)
plot_model(panelregressionROEeast85,type = "pred",terms = "precipitation[all]")

#ROE,west
panelregressionROEwest75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelwest)
summary(panelregressionROEwest75)
plot_model(panelregressionROEwest75,type = "pred",terms = "precipitation[all]")

panelregressionROEwest80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelwest)
summary(panelregressionROEwest80)
plot_model(panelregressionROEwest80,type = "pred",terms = "precipitation[all]")

panelregressionROEwest85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelwest)
summary(panelregressionROEwest85)
plot_model(panelregressionROEwest85,type = "pred",terms = "precipitation[all]")

#ROE,north
panelregressionROEnorth75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelnorth)
summary(panelregressionROEnorth75)
plot_model(panelregressionROEnorth75,type = "pred",terms = "precipitation[all]")

panelregressionROEnorth80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelnorth)
summary(panelregressionROEnorth80)
plot_model(panelregressionROEnorth80,type = "pred",terms = "precipitation[all]")

panelregressionROEnorth85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company,cluster = c("year","company"), panelnorth)
summary(panelregressionROEnorth85)
plot_model(panelregressionROEnorth85,type = "pred",terms = "precipitation[all]")

#ROE,south
panelregressionROEsouth75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelsouth)
summary(panelregressionROEsouth75)
plot_model(panelregressionROEsouth75,type = "pred",terms = "precipitation[all]")

panelregressionROEsouth80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelsouth)
summary(panelregressionROEsouth80)
plot_model(panelregressionROEsouth80,type = "pred",terms = "precipitation[all]")

panelregressionROEsouth85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelsouth)
summary(panelregressionROEsouth85)
plot_model(panelregressionROEsouth85,type = "pred",terms = "precipitation[all]")


#Part Three:
#Run the same model again for subsamples based on the firm's employee size class
panel_final$Number.of.employees.Last.avail..yr <- as.numeric(panel_final$Number.of.employees.Last.avail..yr)
panel_final$companysize <-if_else(panel_final$Number.of.employees.Last.avail..yr <= 9, 1, if_else(panel_final$Number.of.employees.Last.avail..yr <= 50, 2, if_else(panel_final$Number.of.employees.Last.avail..yr<= 250, 3, 4)))

#turnover,size1
panelsize1 <- subset(panel_final, panel_final$companysize == 1)
panel1regressionTO75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company,  cluster = c("year","company"),panelsize1)
summary(panel1regressionTO75)
plot_model(panel1regressionTO75,type = "pred",terms = "precipitation[all]")

panel1regressionTO80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company,  cluster = c("year","company"),panelsize1)
summary(panel1regressionTO80)
plot_model(panel1regressionTO80,type = "pred",terms = "precipitation[all]")

panel1regressionTO85<- feols(log(turnover) ~ THI_DAYS85 + precipitation + precipitation^2 | year+company,  cluster = c("year","company"),panelsize1)
summary(panel1regressionTO85)
plot_model(panel1regressionTO85,type = "pred",terms = "precipitation[all]")

#turnover, size2
panelsize2 <- subset(panel_final, panel_final$companysize == 2)
panel2regressionTO75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company,  cluster = c("year","company"),panelsize2)
summary(panel2regressionTO75)
plot_model(panel2regressionTO75,type = "pred",terms = "precipitation[all]")

panel2regressionTO80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company,  cluster = c("year","company"),panelsize2)
summary(panel2regressionTO80)
plot_model(panel2regressionTO80,type = "pred",terms = "precipitation[all]")

panel2regressionTO85<- feols(log(turnover)~ THI_DAYS85 + precipitation + precipitation^2 | year+company,  cluster = c("year","company"),panelsize2)
summary(panel2regressionTO85)
plot_model(panel2regressionTO85,type = "pred",terms = "precipitation[all]")

#turnover, size3
panelsize3 <- subset(panel_final, panel_final$companysize == 3)
panel3regressionTO75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelsize3)
summary(panel3regressionTO75)
plot_model(panel3regressionTO75,type = "pred",terms = "precipitation[all]")

panel3regressionTO80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelsize3)
summary(panel3regressionTO80)
plot_model(panel3regressionTO80,type = "pred",terms = "precipitation[all]")

panel3regressionTO85<- feols(log(turnover) ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"),panelsize3)
summary(panel3regressionTO85)
plot_model(panel3regressionTO85,type = "pred",terms = "precipitation[all]")

#turnover, size4
panelsize4 <- subset(panel_final, panel_final$companysize == 4)
panel4regressionTO75<- feols(log(turnover) ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize4)
summary(panel4regressionTO75)
plot_model(panel4regressionTO75,type = "pred",terms = "precipitation[all]")

panel4regressionTO80<- feols(log(turnover) ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize4)
summary(panel4regressionTO80)
plot_model(panel4regressionTO80,type = "pred",terms = "precipitation[all]")

panel4regressionTO85<- feols(log(turnover) ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize4)
summary(panel4regressionTO85)
plot_model(panel4regressionTO85,type = "pred",terms = "precipitation[all]")

#ROE. size1
panel1regressionROE75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize1)
summary(panel1regressionROE75)
plot_model(panel1regressionROE75,type = "pred",terms = "precipitation[all]")

panel1regressionROE80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize1)
summary(panel1regressionROE80)
plot_model(panel1regressionROE80,type = "pred",terms = "precipitation[all]")

panel1regressionROE85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize1)
summary(panel1regressionROE85)
plot_model(panel1regressionROE85,type = "pred",terms = "precipitation[all]")

#ROE,size2
panel2regressionROE75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize2)
summary(panel2regressionROE75)
plot_model(panel2regressionROE75,type = "pred",terms = "precipitation[all]")

panel2regressionROE80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize2)
summary(panel2regressionROE80)
plot_model(panel2regressionROE80,type = "pred",terms = "precipitation[all]")

panel2regressionROE85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize2)
summary(panel2regressionROE85)
plot_model(panel2regressionROE85,type = "pred",terms = "precipitation[all]")

#ROE,size3
panel3regressionROE75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize3)
summary(panel3regressionROE75)
plot_model(panel3regressionROE75,type = "pred",terms = "precipitation[all]")

panel3regressionROE80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize3)
summary(panel3regressionROE80)
plot_model(panel3regressionROE80,type = "pred",terms = "precipitation[all]")

panel3regressionROE85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize3)
summary(panel3regressionROE85)
plot_model(panel3regressionROE85,type = "pred",terms = "precipitation[all]")

#ROE,size4
panel4regressionROE75<- feols(ROE ~ THI_DAYS75 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize4)
summary(panel4regressionROE75)
plot_model(panel4regressionROE75,type = "pred",terms = "precipitation[all]")

panel4regressionROE80<- feols(ROE ~ THI_DAYS80 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize4)
summary(panel4regressionROE80)
plot_model(panel4regressionROE80,type = "pred",terms = "precipitation[all]")

panel4regressionROE85<- feols(ROE ~ THI_DAYS85 + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panelsize4)
summary(panel4regressionROE85)
plot_model(panel4regressionROE85,type = "pred",terms = "precipitation[all]")
###Question:How to make graphs for these results?