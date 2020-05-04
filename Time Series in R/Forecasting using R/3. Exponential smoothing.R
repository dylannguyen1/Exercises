#
## Exponentially weighted forecasts
#
oildata <- window(oil, start = 1996)
fc <- ses(oildata, h = 5) ## Simple Exponential smoothing 5 years
summary(fc)
autoplot(fc) +
  ylab("Oil(millions of tonnes)") + xlab("Year")

#
##
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
