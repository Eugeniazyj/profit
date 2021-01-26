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

library(fixest)
library(sjPlot)
library(ggplot2)
library(dplyr)
#threshold=75
panel_final$turnover <- as.numeric(panel_final$turnover) #change the type of turnover from character to numeric
panelregressionTO<- feols(log(turnover+70) ~ THI_DAYS + precipitation + precipitation^2 | year+company, cluster = c("year","company") , panel_final)#do the fixed effects regression
#NOTE: 1,867 observations removed because of NA values (LHS: 56, RHS: 1,811).
summary(panelregressionTO)
hist((panel_final$turnover+70))
summary(panelregressionTO, se="twoway") #show the regression output
# or: summary(panelregressionTO, cluster = ~year+company)
# or: summary(panelregression, cluster = panelregression[,c("year","company")])
### Question: I don't fully understand cluster here
# when the threshold is 75, THI_DAYS is not significant; but when threhold changes to 80, all variables are significant at the 10% level
summary(panel_final$ROE)
panel_final$ROE <- as.numeric(gsub(",",".",panel_final$ROE)) # change "," to "." in order to make ROE numeric
panelregressionROE <- feols(ROE ~ THI_DAYS + precipitation + precipitation^2 | year+company, cluster = c("year","company"), panel_final)#NOTE: 2,403 observations removed because of NA values (LHS: 735, RHS: 1,811).
summary(panelregressionROE, se="twoway")
# whatever 75 or 80, not significant
plot_model(panelregressionTO,type = "pred",terms = "precipitation[all]")
plot_model(panelregressionROE,type = "pred",terms = "precipitation[all]")
### Question: How to interpret marginal effects for precipitation properly?


#robust check
###Question: I also do not fully understand robust check, I only have an abstract understanding. 
###How to tell whether it is robust or not? 
panelrobust <- panel_final
panelrobust$turnover <- as.numeric(panelrobust$turnover)
panelrobust$ROE <- as.numeric(gsub(",",".",panelrobust$ROE)) 
summary(panelrobust$long)
paneleast <- subset(panelrobust, panelrobust$long > 14.2) # use the same standard as Maarten
panelwest <- subset(panelrobust, panelrobust$long < 14.2)
summary(panelrobust$lat)
panelnorth <- subset(panelrobust,panelrobust$lat > 46.75)
panelsouth <- subset(panelrobust,panelrobust$lat < 46.75)

panelregressionTOeast<- feols(turnover ~ THI_DAYS + precipitation + precipitation^2 | year+company, paneleast)
summary(panelregressionTOeast,se="twoway")
panelregressionTOwest<- feols(turnover ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelwest)
summary(panelregressionTOwest,se="twoway")
panelregressionTOnorth<- feols(turnover ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelnorth)
summary(panelregressionTOnorth,se="twoway")
panelregressionTOsouth<- feols(turnover ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelsouth)
summary(panelregressionTOsouth,se="twoway")

panelregressionROEeast<- feols(ROE ~ THI_DAYS + precipitation + precipitation^2 | year+company, paneleast)
summary(panelregressionROEeast,se="twoway")
panelregressionROEwest<- feols(ROE ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelwest)
summary(panelregressionROEwest,se="twoway")
panelregressionROEnorth<- feols(ROE ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelnorth)
summary(panelregressionROEnorth,se="twoway")
panelregressionROEsouth<- feols(ROE ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelsouth)
summary(panelregressionROEsouth,se="twoway")


#Should I do like this?
#Run the same model again for subsamples based on the firm's employee size class
panel_final$Number.of.employees.Last.avail..yr <- as.numeric(panel_final$Number.of.employees.Last.avail..yr)
panel_final$companysize <-if_else(panel_final$Number.of.employees.Last.avail..yr <= 9, 1, if_else(panel_final$Number.of.employees.Last.avail..yr <= 50, 2, if_else(panel_final$Number.of.employees.Last.avail..yr<= 250, 3, 4)))
panelsize1 <- subset(panel_final, panel_final$companysize == 1)
panel1regressionTO<- feols(turnover ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelsize1)
summary(panel1regressionTO,se="twoway")
panelsize2 <- subset(panel_final, panel_final$companysize == 2)
panel2regressionTO<- feols(turnover ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelsize2)
summary(panel2regressionTO,se="twoway")
panelsize3 <- subset(panel_final, panel_final$companysize == 3)
panel3regressionTO<- feols(turnover ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelsize3)
summary(panel3regressionTO,se="twoway")
panelsize4 <- subset(panel_final, panel_final$companysize == 4)
panel4regressionTO<- feols(turnover ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelsize4)
summary(panel4regressionTO,se="twoway")

panel1regressionROE<- feols(ROE ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelsize1)
summary(panel1regressionROE,se="twoway")
panel2regressionROE<- feols(ROE ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelsize2)
summary(panel2regressionROE,se="twoway")
panel3regressionROE<- feols(ROE ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelsize3)
summary(panel3regressionROE,se="twoway")
panel4regressionROE<- feols(ROE ~ THI_DAYS + precipitation + precipitation^2 | year+company, panelsize4)
summary(panel4regressionROE,se="twoway")
###Question:How to make graphs for these results?