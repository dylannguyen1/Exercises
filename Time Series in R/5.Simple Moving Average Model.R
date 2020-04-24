##
###Simulate the simple moving average model
##
# Generate MA model with slope 0.5
x <- arima.sim(model = list(ma= 0.5), n = 100)

# Generate MA model with slope 0.9
y <-  arima.sim(model = list(ma= 0.9), n = 100)


# Generate MA model with slope -0.5
z <- arima.sim(model = list(ma= -0.5), n = 100)

# Plot all three models together
plot.ts(cbind(x, y, z))

# Calculate ACF for x
acf(x)

# Calculate ACF for y
acf(y)

# Calculate ACF for z
acf(z)


##
### MA model estimation and forecasting
##
data(Mishkin, package = "Ecdat")
inflation <- as.ts(Mishkin[,1])
#calcute change in inflation
inflation_changes <- diff(inflation) ## change in 1-month inflation rate
ts.plot(inflation) ; ts.plot(inflation_changes) ## plot

acf(inflation_changes, lag.max = 24)

## fit ma model into inflation
MA_inflation_changes <- arima(inflation_changes, order = c(0,0,1)) ## first-order ma model
print(MA_inflation_changes)
## fitted values
MA_inflation_changes_fitted <- inflation_changes - residuals(MA_inflation_changes)
## plot fitted values
plot(MA_inflation_changes_fitted)
points(MA_inflation_changes_fitted, type = "l", col = "red", lty = 2)


