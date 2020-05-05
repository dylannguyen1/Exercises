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

#
## Evaluating forecast accuracy of seasonal methods
#
library(fpp)
data("vn")
# Create three training series omitting the last 1, 2, and 3 years
train1 <- window(vn[, "Melbourne"], end = c(2010, 4))
train2 <- window(vn[,"Melbourne"],end = c (2009,4))
train3 <-  window(vn[,"Melbourne"],end = c (2008,4))

# Produce forecasts using snaive()
fc1 <- snaive(train1, h = 4)
fc2 <- snaive(train2, h = 4)
fc3 <- snaive(train3, h = 4)

# Use accuracy() to compare the MAPE of each series
accuracy(fc1, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc2, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc3, vn[, "Melbourne"])["Test set", "MAPE"]

#
## Time series cross validation
#
data(oil)
## Cross-validation, forecast 1 steps ahead in time
e <- tsCV(oil, forecastfunction  = naive, h =1)
mean(e^2, na.rm= TRUE) ## compute root mean squared error, remove na

sq <-function(u){u^2} ## square function
for(h in 1:10)
{
  oil %>% tsCV(forecastfunction = naive, h =h) %>%
    sq() %>% mean(na.rm = TRUE) %>% print()
}
### further ahead you trying to forecast, the least accurate they are

#
## Time series cross validation with goog data
#
data("goog")
# Compute cross-validated errors for up to 8 steps ahead
e <- tsCV(goog, forecastfunction = naive, h = 8)

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)

# Plot the MSE values against the forecast horizon
## create a dataframe with value from 1 to 8 and mse
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()