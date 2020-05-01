#
## First things first
#
## Plot Johnson and johnson time series data
library(astsa)
plot(jj, main = "Johnson and Johnson quarterly earning per share",type ="c")
text(jj,labels = 1:4, col = 1:4)
# seasonal components

## Global temparature deviations
library(astsa)
plot(globtemp, main = "Global Temperature Deviations", type = "o")
# no seasonal component and homoscedastic

## S&P 500 Weekly Returns
# no trends or seasonality
library(xts)
plot(sp500w,main = "S&P 500 Weekly return")

#
##Data play, plotting different charts
#
# View a detailed description of AirPassengers
help(AirPassengers)

# Plot AirPassengers
plot(AirPassengers)

# Plot the DJIA time series daily closings 
plot(djia$Close)

# Plot the Southern Oscillation Index
plot(soi)

#
## Non-stationary and stationary
#
# Plot detrended y (trend stationary)
plot(diff(y))

# Plot detrended x (random walk)
plot(diff(x))

#
##Detrending data
#
library(astsa)
data(globtemp)
data(cmort)

# Plot globtemp and detrended globtemp
par(mfrow = c(2,1))
plot(globtemp) 
plot(diff(globtemp))

# Plot cmort and detrended cmort
par(mfrow = c(2,1))
plot(cmort)
plot(diff(cmort))

#
##Exercise,Dealing with trend and heteroscedasticity
#
library(xts)
data(djia)
data(gnp)

# Plot GNP series (gnp) and its growth rate
par(mfrow = c(2,1))
plot(gnp)
plot(diff(log(gnp)))


# Plot DJIA closings (djia$Close) and its returns
par(mfrow = c(2,1))
plot(djia$Close)
plot(diff(log(djia$Close)))

#
## Stationary time series:ARMA
#
##Generate MA
x <-arima.sim(list(order = c(0,0,1),ma = 0.9), n= 100)
plot(x)
## Generate AR
y <- arima.sim(list(order = c(2,0,0),ar = c(0,-0.9)), n =100)
plot(y)

#
## Simulating ARMA models
#
# Generate and plot white noise
WN <- arima.sim( model = list(order=c(0,0,0)),n=200)
plot(WN)

# Generate and plot an MA(1) with parameter .9 
MA <- arima.sim(model = list(order = c(0,0,1),ma=0.9),n=200)
plot(MA)

# Generate and plot an AR(2) with parameters 1.5 and -.75
AR <- arima.sim(list(order = c(2,0,0),ar = c(1.5,-0.75)), n =200)
plot(AR)


