library(readxl)
library(forecast)

# Read the data from Excel into R
mydata <- read_excel("exercise1.xlsx")

# Look at the first few lines of mydata
head(mydata)

# Create a ts object for sales
myts <- ts(mydata['Sales'], start = c(1881, 3), frequency = 4)
plot(myts)

# Create a ts object for all columns
myts <- ts(mydata[, 2:4], start = c(1981, 1), frequency = 4)

#
## Time series plot
# 
data("gold") # containing gold prices in US dollars
data("woolyrnq") #the production of woollen yarn in Australia
data("gas") # containing Australian gas production
# Plot the data with facetting
autoplot(myts, facets = T)

# Plot the data without facetting
autoplot(myts,facets= F)

# Plot the three series
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)
plot(gas)
# Find the outlier in the gold series , smallest index of the maximum number
goldoutlier <- which.max(gold)

# Look at the seasonal frequencies of the three series
frequency(gold)
frequency(woolyrnq)
frequency(gas)

#
## Seasonal plots
#
# Load the fpp2 package
library(fpp2)

# Load the data
data(a10) #monthly sales volumes for anti-diabetic drugs in Australia
data(ausbeer) #contains quarterly beer production for Australia

# Create plots of the a10 data
autoplot(a10)
ggseasonplot(a10)

# Produce a polar coordinate season plot for the a10 data
ggseasonplot(a10, polar = T)

# Restrict the ausbeer data to start in 1992
beer <- window(ausbeer, start = 1992)

# Make plots of the beer data
autoplot(beer)
ggsubseriesplot(beer)

#
## Autocorrelation of non-seasonal time series
#
data(oil)
# Create an autoplot of the oil data
autoplot(oil)

# Create a lag plot of the oil data
gglagplot(oil)

# Create an ACF plot of the oil data
ggAcf(oil)

#
## Autocorrelation of seasonal and cyclic time series
#
data("sunspot.year")
data("hyndsight")
# Plot the annual sunspot numbers
autoplot(sunspot.year)
ggAcf(sunspot.year)

# Save the lag corresponding to maximum autocorrelation
maxlag_sunspot <- 1

# Plot the traffic on the Hyndsight blog
autoplot(hyndsight)
ggAcf(hyndsight)

# Save the lag corresponding to maximum autocorrelation
maxlag_hyndsight <- 7

#
## White noise
#
set.seed(3)
wn <-ts(rnorm(36)) # White noise
autoplot(wn)
ggAcf(wn) + ggtitle("Sample ACF for white noise")

#
## Stock prices and white noise
#
data(goog)
# Plot the original series
autoplot(goog)

# Plot the differenced series
autoplot(diff(goog))

# ACF of the differenced series
ggAcf(diff(goog))

# Ljung-Box test of the differenced series
Box.test(diff(goog), lag = 10, type = "Ljung")