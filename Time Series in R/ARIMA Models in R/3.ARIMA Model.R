#
## ARIMA - integrated ARMA
#
library(astsa)
## Simulation ARIMA (p =1,d=1,q=0)
x <- arima.sim(list(order = c (1,1,0), ar = 0.9), n =200)
plot(x, main ="ARIMA (p=1,d=1,q=0)")
plot(diff(x),main = "ARMA (p=1,d=0,q=0)")

x<- arima.sim(list(order = c(1,1,0),ar =0.9), n = 200)
acf2(x)

## Oil and oil difference test
data(oil)
oil_diff <-diff(oil)
plot(oil_diff)
acf2(oil_diff)
acf2(oil)

#
## ARIMA -plug and play ARIMA(1,1,0)
#
x <- arima.sim(model = list(order = c(1, 1, 0), ar = .9), n = 200)
# Plot x
plot(x)

# Plot the P/ACF pair of x
plot(acf2(x))

# Plot the differenced data
plot(diff(x))

# Plot the P/ACF pair of the differenced data
plot(acf2(diff(x)))
#
## Simulated ARIMA (2,1,0)
# 
x_2 <- arima.sim(model = list(order = c(2, 1, 0), ar = c(1.5,-.75)), n = 200)

# Plot sample P/ACF of differenced data and determine model
plot(acf2(diff(x_2)))


# Estimate parameters and examine output
x_2_fitted <-sarima(x_2,2,1,0)
x_2_fitted$ttable

#
## Global warning
#
