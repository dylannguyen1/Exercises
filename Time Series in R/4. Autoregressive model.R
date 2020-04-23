##
#### AR recursion  Yt = u + Slope times * (t-1) + Et
##
# Simulate an AR model with 0.5 slope and 100 observations
x <- arima.sim(model = list(ar =0.5), n = 100)

# Simulate an AR model with 0.9 slope
y <- arima.sim(model = list(ar=0.9), n = 100)

# Simulate an AR model with -0.75 slope
z <-  arima.sim(model = list(ar= -0.75), n =100)

# Plot your simulated data
plot.ts(cbind(x, y, z))

# Calculate the ACF for x
acf(x)

# Calculate the ACF for y
acf(y)

# Calculate the ACF for z
acf(z)
