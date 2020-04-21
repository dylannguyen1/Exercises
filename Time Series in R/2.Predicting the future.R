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

## Simulate white noise with mean = 4, sd = 2
WN_2 <- arima.sim(model = list(order = c(0,0,0)), n =50,
                  mean = 4, sd = 2)
ts.plot(WN_2)
# estimiate the white noise with arima()
arima(WN_2, order = c(0,0,0))
# calculate mean and variance instead of using arima()
mean(WN_2)
var(WN_2)
#
##Simulate the white noise model exercise
#
# Simulate a WN model with list(order = c(0, 0, 0))
white_noise <- arima.sim(model = list(order =c(0,0,0)), n = 100)

# Plot your white_noise data
ts.plot(white_noise)

# Simulate from the WN model with: mean = 100, sd = 10
white_noise_2 <- arima.sim(model = list(order =c(0,0,0)), n = 100, mean = 100, sd = 10)

# Plot your white_noise_2 data
ts.plot(white_noise_2)
