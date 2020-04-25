##
#### AR recursion  Yt = u + Slope times * (t-1) + Et
##
# Simulate an AR model with 0.5 slope and 100 observations
x <- arima.sim(model = list(ar =0.5), n = 100)

# Simulate an AR model with 0.9 slope
y <- arima.sim(model = list(ar=0.9), n = 100)

# Simulate an AR model with -0.75 slope
z <-  arima.sim(model = list(ar= -0.75), n =100)

# Plot your simulated data
plot.ts(cbind(x, y, z))

# Calculate the ACF for x
acf(x)

# Calculate the ACF for y
acf(y)

# Calculate the ACF for z
acf(z)

#
## Compare the random walk (RW) and autoregressive (AR) models
#
# Simulate and plot AR model with slope 0.9 
x <- arima.sim(model = list(ar = 0.9), n = 200)
ts.plot(x)
acf(x)

# Simulate and plot AR model with slope 0.98
y <- arima.sim(model = list(ar = 0.98), n = 200)
ts.plot(y)
acf(y)

# Simulate and plot RW model
z <- arima.sim(model = list(order=c(0,1,0)), n = 200)
ts.plot(z)
acf(z) 


##
### AR model estimation and forecasting
##
data(Mishkin, package = "Ecdat")
inflation <- as.ts(Mishkin[,1]) # get the first column and turn it into time-series
ts.plot(inflation) ; acf(inflation)

AR_inflation <- arima(inflation, order = c(1,0,0)) # first-order AR model
print(AR_inflation) # summary
ts.plot(inflation)
AR_inflation_fitted <- inflation - residuals(AR_inflation)
points(AR_inflation_fitted, type = "1",
       col = "red", lty = 2) ## draw points function

predict(AR_inflation)$pred # prediction
predict(AR_inflation)$se # standard error

## Forecasting 
predict(AR_inflation, n.ahead = 6)$pred
predict(AR_inflation, n.ahead = 6)$se

# Fit the AR model to x
arima(x, order = c(1,0,0))

# the slope (ar1) estimate

0.8575
# mean (intercept) estimate
-0.0948

# Innovation variance (sigma^2) estimate
1.022
# Fit the AR model to AirPassengers
AR <- arima(AirPassengers, order = c(1,0,0))
print(AR)

# Plot the series and fitted values
ts.plot(AirPassengers)
AR_fitted <- AirPassengers - residuals(AR)
points(AR_fitted, type = "l", col = 2, lty = 2)

##
####Apply AR model on Nile dataset to predict Nile flow from 1971 to 1980
##
# Fit an AR model to Nile
AR_fit <- arima(Nile, order  = c(1,0,0))
print(AR_fit)

# Use predict() to make a 1-step forecast
predict_AR <- predict(AR_fit,n.ahead = 1)

# Obtain the 1-step forecast using $pred[1]
predict_AR$pred[1]

# Use predict to make 1-step through 10-step forecasts
predict(AR_fit, n.ahead = 10)

# Run to plot the Nile series plus the forecast and 95% prediction intervals
ts.plot(Nile, xlim = c(1871, 1980))
AR_forecast <- predict(AR_fit, n.ahead = 10)$pred
AR_forecast_se <- predict(AR_fit, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)