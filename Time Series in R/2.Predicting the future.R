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
