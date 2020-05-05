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