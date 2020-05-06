#
## Transformation of variance stabilization
#
data("usmelec")
autoplot(usmelec) + xlab ("Year")+ ylab("") + ggtitle("US montly net electricity generation")
autoplot(usmelec^0.5) + xlab ("Year")+ ylab("") + ggtitle("US montly net electricity generation")
autoplot(log(usmelec)) + xlab ("Year")+ ylab("") + ggtitle("US montly net electricity generation")

## inverse a bit too much
autoplot(-1/usmelec) + xlab ("Year")+ ylab("") + ggtitle("US montly net electricity generation")

# Box-Cox transformation
BoxCox.lambda(usmelec) ## choose lambda -0.477

usmelec %>% ets(lambda = -0.47) %>%
  forecast(h=60) %>% ## forecast wil undo box-cox transformation to original scale
  autoplot()

#
## Box-Cox transformations for time series
#
data(a10)
# Plot the series
autoplot(a10)

# Try four values of lambda in Box-Cox transformations
a10 %>% BoxCox(lambda =0.0) %>% autoplot()
a10 %>% BoxCox(lambda =0.1) %>% autoplot()
a10 %>% BoxCox(lambda =0.2) %>% autoplot()
a10 %>% BoxCox(lambda =0.3) %>% autoplot()

# Compare with BoxCox.lambda()
BoxCox.lambda(a10)

#
## Non-seasonal differencing for stationarity
#
data("wmurders")
# Plot the US female murder rate
autoplot(wmurders)

# Plot the differenced murder rate
autoplot(diff(wmurders))

# Plot the ACF of the differenced murder rate
ggAcf(diff(wmurders))

#
## Seasonal differencing for stationarity
#
data("h02")
# Plot the data
autoplot(h02)

# Take logs and seasonal differences of h02
difflogh02 <- diff(log(h02), lag = 12)

# Plot difflogh02
autoplot(difflogh02)

# Take another difference and plot
ddifflogh02 <- diff(difflogh02)
autoplot(ddifflogh02)
# Plot ACF of ddifflogh02
ggAcf(ddifflogh02)

#
## ARIMA models
#
autoplot(usnetelec)
fit <- auto.arima(usnetelec)
summary(fit)
fit %>% forecast() %>% autoplot()


#
## Fitting ARIMA models
#
# Fit an automatic ARIMA model to the austa series
fit <- auto.arima(austa)

# Check that the residuals look like white noise
checkresiduals(fit)
residualsok <- TRUE

# Summarize the model
summary(fit)

# Find the AICc value and the number of differences used
AICc <- -14.46
d <- 1

# Plot forecasts of fit
fit %>% forecast(h = 10) %>% autoplot()

#
## Fitting different ARIMA models  to austa
#
# Plot forecasts from an ARIMA(0,1,1) model with no drift
austa %>% Arima(order = c(0, 1, 1), include.constant = FALSE) %>%
  forecast() %>% autoplot()

# Plot forecasts from an ARIMA(2,1,3) model with drift
austa %>% Arima(order = c(2,1,3),include.constant = TRUE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(0,0,1) model with a constant
austa %>% Arima(order = c(0,0,1),include.constant = TRUE) %>% forecast() %>% autoplot()
# Plot forecasts from an ARIMA(0,2,1) model with no constant
austa %>% Arima(order = c(0,2,1),include.constant = FALSE) %>% forecast() %>% autoplot()

#
## Compare auto.arima() and ets() on non-seasonal data
#
# Set up forecast functions for ETS and ARIMA models
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}

# Compute CV errors for ETS on austa as e1
e1 <- tsCV(austa, fets, h = 1)

# Compute CV errors for ARIMA on austa as e2
e2 <- tsCV(austa, farima, h=1)

# Find MSE of each model class
mean(e1^2,na.rm = TRUE )
mean(e2^2, na.rm = TRUE)

# Plot 10-year forecasts using the best model class
austa %>% farima(h=10)  %>% autoplot()

#
## Seasonal ARIMA models
#
data("debitcards") ## Iceland debit cards useage
autoplot(debitcards) + xlab("Year") + ylab("millions ISK")+
  ggtitle("Retail debit card usage in Iceland")
fit_seasonal <-auto.arima(debitcards,lambda = 0) ## box-cox lambda = 0 = log transform
fit_seasonal

fit_seasonal %>% forecast(h=36) %>%
  autoplot() + xlab("Year")

#
## Automatic ARIMA models for seasonal time series
#
# Check that the logged h02 data have stable variance
h02 %>% log()%>% autoplot()

# Fit a seasonal ARIMA model to h02 with lambda = 0
fit <- auto.arima(h02, lambda = 0)

# Summarize the fitted model
summary(fit)

# Record the amount of lag-1 differencing and seasonal differencing used
d <- 1
D <- 1

# Plot 2-year forecasts
fit %>% forecast(24) %>% autoplot()

#
## Exploring auto.arima() options
#
# Find an ARIMA model for euretail
fit1 <- auto.arima(euretail)

# Don't use a stepwise search
fit2 <- auto.arima(euretail, stepwise = FALSE)

# Compute 2-year forecasts from better model based on AICc
fit2 %>% forecast(h = 8) %>% autoplot()

#
## Exponential smoothing vs ARIMA on qcement dataset
#
# Use 20 years of the qcement data beginning in 1988
train <- window(qcement, start = 1988, end = c(2007,4))

# Fit an ARIMA and an ETS model to the training data
fit1 <- auto.arima(train)
fit2 <- ets(train)

# Check that both models have white noise residuals
checkresiduals(fit1)
checkresiduals(fit2)

# Produce forecasts for each model
fc1 <- forecast(fit1, h = 25)
fc2 <- forecast(fit2, h = 25)

# Use accuracy() to find better model based on RMSE
accuracy(fc1, qcement)
accuracy(fc2, qcement)
bettermodel <- fit2

#
## Dynamic harmonic regression with Fourier term
#
library(fpp2)
library(fpp)
data(cafe)
fit <- auto.arima(cafe,xreg = fourier(cafe, K=1),
                  seasonal = FALSE,## set seasonal error = false,arima error - non-seasonal
                  lambda = 0)  ## box-cox transformation with lambda = 0
fit %>% forecast(xreg = fourier(cafe, K=1, h =24)) %>%
  autoplot() +ylim(1.6,5.1)

autoplot(fit)

#
## Forecast weekly data
#
# Set up harmonic regressors of order 13
harmonics <- fourier(gasoline, K = 13)

# Fit regression model with ARIMA errors
fit <- auto.arima(gasoline, xreg = harmonics, seasonal = FALSE)

# Forecasts next 3 years
newharmonics <- fourier(gasoline, K = 13, h = 156)
fc <- forecast(fit, xreg = newharmonics)

# Plot forecasts fc
autoplot(fc)
## try to select k with lowest AICc values

#
##Harmonic regression for multiple seasonality
#
data(taylor)#half-hourly electricity demand in England
## 2 seasonality 48(daily seasonality) and 3*48 = 336 (weekly seasonality)
# Fit a harmonic regression using order 10 for each type of seasonality
fit <- tslm(taylor ~ fourier(taylor, K = c(10, 10)))

# Forecast 20 working days ahead
fc <- forecast(fit, newdata = data.frame(fourier(taylor, K = c(10, 10), h = 20*48)))

# Plot the forecasts
autoplot(fc)

# Check the residuals of fit
checkresiduals(fit) ## residuals failed the test badly

#
## Forecast call bookings
#
data("calls") ## 5-minute periods in a working day
## 2 period: 169 periods of 5-minutes in each working day
## 5*169 = 845 periods in a working week

# Plot the calls data
autoplot(calls)

# Set up the xreg matrix
xreg <- fourier(calls, K = c(10,0))

# Fit a dynamic regression model
fit <- auto.arima(calls, xreg = xreg, seasonal = FALSE, stationary = TRUE)

# Check the residuals
checkresiduals(fit)

# Plot forecasts for 10 working days ahead
fc <- forecast(fit, xreg =  fourier(calls, c(10, 0), h = 1690))
autoplot(fc)
