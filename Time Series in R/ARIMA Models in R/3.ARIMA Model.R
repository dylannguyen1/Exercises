#
## ARIMA - integrated ARMA
#
library(astsa)
## Simulation ARIMA (p =1,d=1,q=0)
x <- arima.sim(list(order = c (1,1,0), ar = 0.9), n =200)
plot(x, main ="ARIMA (p=1,d=1,q=0)")
plot(diff(x),main = "ARMA (p=1,d=0,q=0)")

x<- arima.sim(list(order = c(1,1,0),ar =0.9), n = 200)
acf2(x)

## Oil and oil difference test
data(oil)
oil_diff <-diff(oil)
plot(oil_diff)
acf2(oil_diff)
acf2(oil)

#
## ARIMA -plug and play ARIMA(1,1,0)
#
x <- arima.sim(model = list(order = c(1, 1, 0), ar = .9), n = 200)
# Plot x
plot(x)

# Plot the P/ACF pair of x
plot(acf2(x))

# Plot the differenced data
plot(diff(x))

# Plot the P/ACF pair of the differenced data
plot(acf2(diff(x)))
#
## Simulated ARIMA (2,1,0)
# 
x_2 <- arima.sim(model = list(order = c(2, 1, 0), ar = c(1.5,-.75)), n = 200)

# Plot sample P/ACF of differenced data and determine model
plot(acf2(diff(x_2)))


# Estimate parameters and examine output
x_2_fitted <-sarima(x_2,2,1,0)
x_2_fitted$ttable

#
## Global warning,fitting 2 ARIMA models
#
data("globtemp") ## annual global temperature standard deviation
# Plot the sample P/ACF pair of the differenced data 
plot(acf2(diff(globtemp)))

# Fit an ARIMA(1,1,1) model to globtemp and calculate AIC and BIC
globtemp_fitted <-sarima(globtemp,1,1,1)
globtemp_fitted$AIC
globtemp_fitted$BIC

# Fit an ARIMA(0,1,2) model to globtemp and and calculate AIC and BIC
globtemp_fitted_2 <-sarima(globtemp,0,1,2)
globtemp_fitted_2$AIC
globtemp_fitted_2$BIC

#
## ARIMA diagnostics
#
oil <- window(oil, end =2006)
oil_fitted1 <-sarima(oil, p=1,d=1,q=1) ## fit ARIMA(1,1,1) into oil
oil_fitted1$ttable

oil_fitted2 <- sarima(oil,p=1,1,2) # fit ARIMA(1,1,2) into oil
oil_fitted2$ttable ## new parameter not significant

#
## Diagnostics - simulated overfitting
#
# Simulating ARIMA (0,1,1)
x <- arima.sim(model = list(order = c(0, 1, 1), ma = .9), n = 250)

# Plot sample P/ACF pair of the differenced data
plot(acf2(diff(x)))

# Fit the first model, compare parameters, check diagnostics
x_fitted <- sarima(x,0,1,1)
x_fitted$ttable
# Fit the second model and compare the overfit of new ma2
x_fitted_2 <-sarima(x,0,1,2)
x_fitted_2$ttable

#
## Diagnostics - global temperature, compare AIC and BIC of 2 different ARIMA models
#

# Fit ARIMA(0,1,2) to globtemp and check diagnostics  
temp <- sarima(globtemp,0,1,2) 
temp$ttable
temp$AIC
temp$BIC
# Fit ARIMA(1,1,1) to globtemp and check diagnostics
temp_2 <- sarima(globtemp,1,1,1)
temp_2$ttable
temp_2$AIC
temp_2$BIC
# Which is the better model?
"ARIMA(0,1,2)"

#
## Forecasting ARIMA oil
#
## use sarima.for()
oil <- window(astsa::oil, end = 2006)
oilf <- window(astsa::oil, end = 2007)
sarima.for(oil,n.ahead = 52,1,1,1) ## forecast by 52 weeks from 2006 to 2007
lines(oilf)

#
## Forecasting simulated ARIMA
#
## Simulate ARIMA(1,1,0) with ar = 0.9
x <- arima.sim(model = list(order = c(1, 1, 0), ar = .9), n = 120)

# Plot P/ACF pair of differenced data 
plot(acf2(diff(x)))

x_100 <- window(x,end = 100)
xf <- window(x,end = 120)
# Fit model - check t-table and diagnostics
x_fitted<-sarima(x_100,1,1,0)
x_fitted$ttable

# Forecast the data 20 time periods ahead
sarima.for(x_100, n.ahead = 20, p = 1, d = 1, q = 0) 
lines(x)  
#
## Forecasting global temperature
#
# Fit an ARIMA(0,1,2) to globtemp and check the fit
sarima(globtemp,0,1,2)

# Forecast data 35 years into the future
sarima.for(globtemp,n.ahead = 35, 0,1,2)



