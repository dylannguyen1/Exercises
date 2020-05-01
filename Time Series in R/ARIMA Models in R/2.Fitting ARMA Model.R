#
## AR(Autoregressive) and MA(Moving Average) model
#
x<- arima.sim(list(order = c(1,0,0),ar = -0.7),n = 200) # AR
y<- arima.sim(list(order = c(0,0,1),ar = -0.7),n = 200) # MA
par(mfrow=c(1,2))
plot(x, main ="AR(1)")
plot(y, main = "MA(1)")

library(astsa)
## Simulate AR(2) with mean 50
x<- arima.sim(list(order = c(2,0,0),ar = c(1.5,-0.75)),n = 200) +50

## Estimation for time series using ASTSA
x_fit <-sarima(x, p =2, d=0, q=0)
x_fit$ttable ## show t table

## Simulate MA(1) with mean 0
y<- arima.sim(list(order = c(0,0,1),ar = -0.7),n = 200)

y_fit <-sarima(y, p=0, d=0, q= 1)
y_fit$ttable

#
## Fitting an AR(1) model
#
# Generate 100 observations from the AR(1) model
x <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 100) 

# Plot the generated data 
plot(x)

# Plot the sample P/ACF pair
plot(acf2(x))

# Fit an AR(1) to the data and examine the t-table
x_fit <- sarima(x,p=1,d=0,q=0)
x_fit$ttable
#
## Fitting an AR(2) model
#
x <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -.75)), n = 200)
plot(x)
# astsa is preloaded

# Plot x
plot(x)

# Plot the sample P/ACF of x
plot(acf2(x))

# Fit an AR(2) to the data and examine the t-table
x_fitted <- sarima(x,p=2,d=0,q=0)
x_fitted$ttable

#
## Fitting an MA(1) model
#
