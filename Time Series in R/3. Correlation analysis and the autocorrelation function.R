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
