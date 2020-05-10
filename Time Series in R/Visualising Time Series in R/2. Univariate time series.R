library(TTR)
library(xts)

data<-read.csv("Daily-returns-for-Apple.csv",sep = " ",header = TRUE)
data <- xts(data[,-1], order.by = as.POSIXct(data$Index)) ## convert to time-based
colnames(data) <-"apple"
# Plot Apple's stock price 
plot(data$apple,main = "Apple stock price")

# Create a time series called rtn
rtn <- ROC(data) # ROC = rate of change

# Plot Apple daily price and daily returns 
par(mfrow = c(1,2))
plot(data)
plot(rtn)

#
## Histogram of returns
#
rtn <- rtn[-1,] # remove the first row
# Create a histogram of Apple stock returns
hist(rtn,probability = TRUE,
     main = "Apple stock return distribution" )

# Add a density line
lines(density(rtn))

# Redraw a thicker, red density line
lines(density(rtn),col = "red",lwd = 2)

#
## Boxplot of returns and compare to normal distribution
#
## Loading data as csv and didnt transform it to xts
data<-read.csv("Daily-returns-for-Apple.csv",sep = " ",header = TRUE)
data <- data[,-1]
rtn1 <- ROC(data)
# Draw box and whisker plot for the Apple returns
boxplot(rtn1,horizontal = TRUE)

# Draw a box and whisker plot of a normal distribution
boxplot(rnorm(1000),horizontal = TRUE)

# Redraw both plots on the same graphical window
par(mfrow = c(2,1))
boxplot(rtn1,horizontal = TRUE)
boxplot(rnorm(1000),horizontal = TRUE)

#
## Autocorrelation
#
# Draw autocorrelation plot
acf(rtn, main = "Apple return autocorrelation")

# Redraw with a maximum lag of 10
acf(rtn, main = "Apple return autocorrelation",lag.max = 10)

#
##
#
# Create q-q plot
qqnorm(rtn,main = "Apple return QQ-plot")

# Add a red line showing normality
qqline(rtn, col = "red") ## doesnt look like a fnorm

#
## A comprehensive time series diagnostic
#
# Draw histogram and add red density line
# Set up 2x2 graphical window
par(mfrow = c(2,2))
## Histogram with probability
hist(rtn,probability = TRUE)
lines(density(rtn),col = "red" )

# Draw box and whisker plot
boxplot(rtn)

# Draw autocorrelogram
acf(rtn)

# Draw q-q plot and add a red line for normality
qqnorm(rtn)
qqline(rtn, col = "red")


