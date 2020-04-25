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

# Plot the DJIA daily closings
plot(djia$Close)

# Plot the Southern Oscillation Index
plot(soi)

