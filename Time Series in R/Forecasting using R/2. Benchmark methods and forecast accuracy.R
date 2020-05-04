library(forecast)
library(fpp2)

data(goog)
data("ausbeer")

# Use naive() to forecast the 20 next series of goog series
fcgoog <- naive(goog, 20)

# Plot and summarize the forecasts
autoplot(fcgoog)
summary(fcgoog)

# Use snaive() to forecast the next 16 values of ausbeer seasonal series
fcbeer <- snaive(ausbeer, h = 16)

# Plot and summarize the forecasts
autoplot(fcbeer)
summary(fcbeer)

#
## Fitted values and residuals
#
data(oil)
fc <- naive(oil) # naive prediction on oil
autoplot(oil,series = "Data") + xlab("Year") +
  autolayer(fitted(fc), series = "Fitted") +
  ggtitle("Oil production in Saudi Arabia")

autoplot(residuals(fc))
checkresiduals((fc))
## p value for Ljung Box test is 0.2475 over 0.05 -> no problems with
# autocorrelation

#
## Checking time series residuals
#
data("goog")
data("ausbeer")
# Check the residuals from the naive forecasts applied to the goog series
## same as checkresiduals(naive(goog))
goog %>% naive() %>% checkresiduals()

# Do they look like white noise (TRUE or FALSE)
googwn <- TRUE

# Check the residuals from the seasonal naive forecasts applied to the ausbeer series
ausbeer %>% snaive() %>% checkresiduals()

# Do they look like white noise (TRUE or FALSE)
beerwn <- FALSE

#
## Training and test set 
#
data(oil)
training <- window(oil, end =2003)
test <- window(oil, end = 2004)
fc <-naive(training, h = 10)
autoplot(fc) + autolayer(test, series = "Test data")

## accuracy
accuracy(fc, test)

#
## Evaluating forecast accuracy of non-seasonal methods
#
data(gold)
# Create the training data with first 1000 observations
train <- subset(gold, end = 1000)

# Compute naive forecasts and save to naive_fc
naive_fc <- naive(train, h = 108)

# Compute mean forecasts and save to mean_fc
mean_fc <- meanf(train, h = 108)

# Use accuracy() to compute RMSE statistics
accuracy(naive_fc, gold)
accuracy(mean_fc, gold)

# Assign one of the two forecasts as bestforecasts
bestforecasts <- naive_fc