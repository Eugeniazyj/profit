#regression
library(plm)
panel_final$turnover <- as.numeric(panel_final$turnover) #Warning message:NAs introduced by coercion 
panelregression <- plm(formula = log(turnover) ~ THI_DAYS + precipitation + as.factor(year), data = panel_final, index = c("company", "year"), model = "within")#plm:panel lm(OLS)
#should I use log here?
#what about the NA values?
#two seperated regression for turnover and ROE or together?
# answer: seperately
summary(panelregression)
coeftest(panelregression, vcov = vcovDC(panelregression))

# from Maarten, he run the same model again for subsamples based on the firm's employee size class,should I do the same?




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