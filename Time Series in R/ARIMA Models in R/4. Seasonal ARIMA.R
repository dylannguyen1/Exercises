#
## Pure Seasonal model
#
library(forecast)

## Seasonal arima with 250 observations SARMA(P=1,Q=1)s=12
## ????simulate.Arima()

# Plot sample P/ACF to lag 60 and compare to the true values
acf2(x, max.lag = 60)

# Fit the seasonal model to x
sarima(x, p = 0, d = 0, q = 0, P = 1, D = 0, Q = 1, S = 12)

#
## Mixed seasonal models
#
data(AirPassengers)
#turn x into a vector and an AirPassenger
x <- ts(as.vector(AirPassengers))

lx <-log(x)
plot(lx)

dlx <- diff(lx) # detrend lx
plot(dlx)

ddlx <- diff(dlx,12) ## seasonal difference to deal with seasonal persistence
plot(ddlx)

acf2(ddlx) #ACF and PACF

## fit a SARIMA(1,1,1), (0,1,1) s =12
air_fitted <- sarima(log(AirPassengers), p = 1,
                     d = 1, q = 1, P = 0,
                     D = 1, Q = 1, S = 12)
air_fitted$ttable 

## non-seasonal AR component not significant
#-> fit new model with p = 0
## Fit new SARIMA(0,1,1)(0,1,1) s = 12
airpass_fit2 <- sarima(log(AirPassengers),0,1,1,0,1,1,12)
airpass_fit2$ttable


#
## Fit mixed seasonal model
#
# Plot sample P/ACF pair to lag 60 and compare to actual
acf2(x,max.lag = 60)

# Fit the seasonal model to x
x_fitted <- sarima(x,p=0,d=0,q=1,P=0,D=0,Q=1,S=12)

#
## Data analysis - unemployment I
#
data("unemp")
# Plot unemp 
plot(unemp)

# Difference unemp and plot
d_unemp <- diff(unemp)
plot(d_unemp)

# Seasonally difference d_unemp and plot
dd_unemp <- diff(d_unemp, lag = 12)
plot(dd_unemp)

# Plot P/ACF pair of the fully differenced data to lag 60
dd_unemp <- diff(diff(unemp), lag = 12)
acf2(dd_unemp, max.lag = 60)

# Fit an appropriate model
unemp_fitted <- sarima(unemp, p = 2, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 12)
unemp_fitted$ttable

#
## Data analysis - Commodity price
#
data("chicken")
plot(chicken)
# Plot differenced chicken
plot(diff(chicken))

# Plot P/ACF pair of differenced data to lag 60
acf2(diff(chicken),max.lag=60)
#notice AR(2) seems appropriate
# Fit ARIMA(2,1,0) to chicken - not so good
arima_fit <- sarima(chicken,2,1,0)
arima_fit$ttable
# Fit SARIMA(2,1,0,1,0,0,12) to chicken - looking better
arima_fit <- sarima(chicken,2,1,0,1,0,0,12)

#
## Fitting birth dataset
#
data("birth")
plot(birth)
# Plot P/ACF to lag 60 of differenced data
d_birth <- diff(birth)
acf2(d_birth,max.lag = 60)

# Plot P/ACF to lag 60 of seasonal differenced data
dd_birth <- diff(d_birth, lag = 12)

acf2(dd_birth,max.lag = 60)
# Fit SARIMA(0,1,1)x(0,1,1)_12. residuals doesn't look like white noise
sarima(birth,0,1,1,0,1,1,12)

# Add AR term ar = 1 to account for additional correlation
sarima(birth,1,1,1,0,1,1,12)

#
## Forecasting SARIMA for AirPassengers
#

sarima.for(log(AirPassengers),n.ahead = 24,
           0,1,1,0,1,1,12)

#
## Predicting monthly unemployment
#
# Fit your previous unemployment model to unemp and check the diagnostics
unemp_fitted <- sarima(unemp, p = 2, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 12)
unemp_fitted$ttable

# Forecast the data 3 years into the future
sarima.for(unemp,n.ahead = 36, 2,1,0,0,1,1,12)

#
## Predicting chicken commodity
#
# Fit the chicken model again and check diagnostics
arima_fit <- sarima(chicken,2,1,0,1,0,0,12)
arima_fit$ttable

# Forecast the chicken data 5 years into the future
sarima.for(chicken,n.ahead = 60, 2,1,0,1,0,0,S=12)
