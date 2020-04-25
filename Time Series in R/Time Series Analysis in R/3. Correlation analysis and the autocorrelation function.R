#
##Scatter Plots
#
ts.plot(cbind(stock_A,stock_B))

## scatterplot of stock B vs A
plot(stock_A, stock_B)

## log returns
stock_A_logreturn = diff(log(stock_A))
stock_B_logreturn = diff(log(stock_B))

## Time series of log returns are all closed to zero
ts.plot(cbind(stock_A_logreturn,stock_B_logreturn))

#
## Asset prices vs asset returns
#
#
## Plotting a time series object
#
data(EuStockMarkets)
eu_stocks <- EuStockMarkets
# Plot eu_stocks
plot(eu_stocks)

# Convert prices to returns
returns <- eu_stocks[-1,] / eu_stocks[-1860,] - 1

# Convert returns to ts
returns <- ts(returns, start = c(1991, 130), frequency = 260)

# Plot returns
plot(returns)

# Use this code to convert prices to log returns
logreturns <- diff(log(eu_stocks))

# Plot logreturns
plot(logreturns)

#
## Characteristics of financial time series
#
eu_percentreturns <- returns
# Generate means from eu_percentreturns
colMeans(eu_percentreturns)

# Use apply to calculate sample variance from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = var)

# Use apply to calculate standard deviation from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = sd)


# Display a histogram of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = hist, main = "", xlab = "Percentage Return")

# Display normal quantile plots of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = qqnorm, main = "")
qqline(eu_percentreturns)

#
## Plotting pairs of data
#
DAX <- eu_stocks[,1]
FTSE <- eu_stocks[,4]

## scatter Plot of DAX and FTSE
plot(DAX,FTSE)

## Scatter Plot matrix of eu_stocks
pairs(eu_stocks)

## Convert eu_stocks to log returns
logreturns <- diff(log(eu_stocks))

## Plot logreturns
plot(logreturns)

## Make a scatterplot matrix of logreturns
pairs(logreturns)

#
##Calculating sample covariances and correlations
#
DAX_logreturns <- logreturns[,1]
FTSE_logreturns <- logreturns[,4]

# Use cov() with DAX_logreturns and FTSE_logreturns
cov(DAX_logreturns, FTSE_logreturns)

# Use cov() with logreturns
cov(logreturns)

# Use cor() with DAX_logreturns and FTSE_logreturns
cor(DAX_logreturns, FTSE_logreturns)

# Use cor() with logreturns
cor(logreturns)

#
## Autocorrelation
#
## correlation for today and yesterday
cor(stock_A[-100],stock_A[-1])
## correlation fortoday and two days earlier
cor(stock_A[-(99:100)],stock_A[-(1:2)])
## ACF funtion: Autocorrelation by lag
acf(stock_A, lag.max = 2, plot = FALSE) # not plotting the acf

#
## Calculating autocorrelations
#

# Define x_t0 as x[-1]
x_t0 <- x[-1]

# Define x_t1 as x[-n]
x_t1 <- x[-n]

# Confirm that x_t0 and x_t1 are (x[t], x[t-1]) pairs  
head(cbind(x_t0, x_t1))

# Plot x_t0 and x_t1
plot(x_t0, x_t1)

# View the correlation between x_t0 and x_t1
cor(x_t0, x_t1)

# Use acf with x
acf(x, lag.max = 1, plot = FALSE)

# Confirm that difference factor is (n-1)/n
cor(x_t1, x_t0) * (n-1)/n

# Generate ACF estimates for x up to lag-10
acf(x, lag.max = 10, plot = FALSE)

## view the ACF of x,y,z
# View the ACF of x
acf(z)

# View the ACF of y
acf(y)

# View the ACF of z
acf(z)
