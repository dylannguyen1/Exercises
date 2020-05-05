#
## Exponentially weighted forecasts
#
library(forecast)
library(fpp2)
library(fpp)
data(oil)
oildata <- window(oil, start = 1996)
fc <- ses(oildata, h = 5) ## Simple Exponential smoothing 5 years
summary(fc)
autoplot(fc) +
  ylab("Oil(millions of tonnes)") + xlab("Year")

#
## Simple exponential smoothing
#
data("marathon")
# Use ses() to forecast the next 10 years of winning times
fc <- ses(marathon, h = 10)

# Use summary() to see the model parameters
summary(fc)

# Use autoplot() to plot the forecasts
autoplot(fc)

# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))

#
## Simple Exponential smoothing vs naive
#
# Create a training set using subset()
data(marathon)
train <- subset(marathon, end = length(marathon) - 20)

# Compute SES and naive forecasts, save to fcses and fcnaive
fcses <- ses(train, h = 20)
fcnaive <- naive(train, h = 20)

# Calculate forecast accuracy measures
accuracy(fcses, marathon)
accuracy(fcnaive, marathon)

# Save the best forecasts as fcbest
fcbest <- fcnaive

#
## Exponential smoothing methods with trend
#
data("AirPassengers")
AirPassengers %>% holt(h=5) %>% autoplot() ## seasonal prediction with holt method

## Holt and damped trend
fc1 <- holt(AirPassengers, h =15, PI = FALSE) # linear trend
fc2 <- holt(AirPassengers, damped = TRUE, h =15, PI = FALSE) # Damped

autoplot(AirPassengers) + xlab("Year") + ylab("millions")+
  autolayer(fc1, series = "Linear Trend") +
  autolayer(fc2, series =" Damped trend")
#
## Holt's trend methods
#
data(austa)
# Produce 10 year forecasts of austa using holt()
fcholt <- holt(austa,h=10)

# Look at fitted model using summary()
summary(fcholt)

# Plot the forecasts
autoplot(fcholt)

# Check that the residuals look like white noise
checkresiduals(fcholt)

#
## Holt Winters Exponential smoothing methods with trend and seasonality
#
aust <- window(austourists, start =2005)
fc1 <- hw(aust, seasonal = "additive")
fc2 <- hw(aust, seasonal = "multiplicative")
autoplot(austourists) + xlab("Year") +ylab("millions of visitors") +
  autolayer(fc1, series = "HW additive forecast")+
  autolayer(fc2, series = "HW multiplicative forecast")

#
## Holt-Winters with monthly data a10
#
data(a10)
# Plot the data
autoplot(a10)

# Produce 3 year forecasts
fc <- hw(a10, seasonal = "multiplicative", h = 36)

# Check if residuals look like white noise
checkresiduals(fc)
whitenoise <- FALSE

# Plot forecasts
autoplot(fc)

#
## Holt-Winters method with daily data hyndsight
#
data("hyndsight")
# Create training data with subset()
train <- subset(hyndsight, end = length(hyndsight)-28)

# Holt-Winters additive forecasts as fchw
fchw <- hw(train, seasonal = "additive", h = 28)

# Seasonal naive forecasts as fcsn
fcsn <- snaive(train, h = 28)

# Find better forecasts with accuracy()
accuracy(fchw, hyndsight)
accuracy(fcsn, hyndsight)

# Plot the better forecasts
autoplot(fchw)

#
## State Space models for exponential smoothing
#
data("ausair") ## Australian air traffic
ausair %>% ets() %>% forecast() %>% autoplot()

data(h02) ## Monthly cortecosteroid drug sales
h02 %>% ets() %>% forecast() %>% autoplot()

#
## Automatic forecasting with exponential smoothing 
#
data(austa)
# Fit ETS model to austa in fitaus
fitaus <- ets(austa)

# Check residuals
checkresiduals(fitaus)

# Plot forecasts
autoplot(forecast(fitaus))

data("hyndsight")
# Repeat for hyndsight data in fiths
fiths <- ets(hyndsight)
checkresiduals(fiths)
autoplot(forecast(fiths))

# Which model(s) fails test? (TRUE or FALSE)
fitausfail <- FALSE
fithsfail <- TRUE

#
## ETS vs seasonal naive
#
## NOT WORKING, DIFFERENT CEMENT DATA REQUIRED
data(qcement)
# Function to return ETS forecasts
fets <- function(y, h) {
  forecast(ets(y), h = h)
}

# Apply tsCV() for both ETS and seasonal naive to calculate forecast errors
e1 <- tsCV(qcement, fets, h = 4)
e2 <- tsCV(qcement, snaive, h = 4)

# Compute MSE of resulting errors (watch out for missing values)
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)

#
## When does ETS fail? using lynx dataset
#
data("lynx")
# Plot the lynx series
autoplot(lynx)

# Use ets() to model the lynx series
fit <- ets(lynx)

# Use summary() to look at model and parameters
summary(fit)

# Plot 20-year forecasts of the lynx series
fit %>% forecast(h=20) %>% autoplot()
