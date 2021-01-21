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
panel_final$turnover <- as.numeric(panel_final$turnover)
panelregression1<- feols(turnover ~ THI_DAYS + precipitation + precipitation^2 | year+company, panel_final)#NOTE: 1,867 observations removed because of NA values (LHS: 56, RHS: 1,811).
summary(panelregression1, se="twoway")
# or: summary(panelregression1, cluster = ~year+company)
# or: summary(panelregression1, cluster = panelregression[,c("year","company")])
# I don't fully understand cluster here, when clustered only by company, precipitation becomes more significant
panel_final$ROE <- as.numeric(gsub(",",".",panel_final$ROE)) # change "," to "." in order to make ROE numeric
panelregression2 <- feols(ROE ~ THI_DAYS + precipitation + precipitation^2 | year+company, panel_final)#NOTE: 2,403 observations removed because of NA values (LHS: 735, RHS: 1,811).
summary(panelregression2, se="twoway")
plot_model(panelregression1,type = "pred",terms = "precipitation[all]")
plot_model(panelregression2,type = "pred",terms = "precipitation[all]")
# How to interpret marginal effects for precipitation.