## Trend spotting

## Sample transformation log() to remove or filter trend
#linearize rapid growth trend or increase variance
# only can apply for positively valued time series

##diff() - remove linear trend, called difference series or change series
## diff(..,s = ?) to remove seasonal trends

##log transform example
# Log rapid_growth
linear_growth <- log(rapid_growth)

# Plot linear_growth using ts.plot()
ts.plot(linear_growth)

#
## Removing trends in level by differencing
#

# Generate the first difference of z
dz <- diff(z)

# Plot dz
ts.plot(dz)

# View the length of z and dz, respectively
length(z)
length(dz)

#
##Removing seasonal trends with seasonal differencing
#
# Generate a diff of x with lag = 4. Save this to dx
dx <- diff(x,lag = 4)

# Plot dx
ts.plot(dx)  

# View the length of x and dx, respectively 

length(x)
length(dx)

#
## White noise model
#
## Simulate 50 observations from the WN model
WN_1 <-arima.sim(model = list(order = c(0,0,0)), n =50)
head(WN_1)
ts.plot(WN_1)