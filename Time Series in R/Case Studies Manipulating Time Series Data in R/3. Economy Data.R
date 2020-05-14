#
## Handling missingness
#
#fill nas with last observation
y <- na.locf(x) ## last observation carried forward(LOCF)
plot(x)
plot(y)
na.locf(x, fromLast = TRUE) ## next observation carried backward(NOCB)
na.approx(x) ## linear interpolation
library(zoo)
library(fpp2)
library(xts)
#
## Exploring economic data
#
gdp <- readRDS("us_gdp.RData")
# Get a summary of your GDP data
summary(gdp)

# Convert GDP date column to time object
gdp$date <- as.yearqtr(gdp$date)

# Convert GDP data to xts
gdp_xts <- as.xts(gdp[, -1], order.by = gdp$date)

# Plot GDP data over time
plot.xts(gdp_xts)

#
## Replace missing data
# 
# Fill NAs in gdp_xts with the last observation carried forward
gdp_locf <- na.locf(gdp_xts)

# Fill NAs in gdp_xts with the next observation carried backward 
gdp_nocb <- na.locf(gdp_xts,fromLast = TRUE)

# Produce a plot for each of your new xts objects
par(mfrow = c(2,1))
plot.xts(gdp_locf, major.format = "%Y")
plot.xts(gdp_nocb, major.format = "%Y")

# Query for GDP in 1993 in both gdp_locf and gdp_nocb
gdp_locf["1993"]
gdp_nocb['1993']

# 
## Replace missing data - II using linear approximation
#
# Fill NAs in gdp_xts using linear approximation
gdp_approx <- na.approx(gdp_xts) 

# Plot your new xts object
plot.xts(gdp_approx, major.format = "%Y")

# Query for GDP in 1993 in gdp_approx
gdp_approx["1993"]


#
## Lagging and differencing
#

unemployment <- readRDS("unemployment.RData")

lty = c(1,2)
labels = c("US Unemployment (%)","MA Unemployment (%)")
# View a summary of your unemployment data
summary(unemployment)

# Use na.approx to remove missing values in unemployment data
unemployment <- na.approx(unemployment)

# Plot new unemployment data
plot.zoo(unemployment, plot.type = "single", lty = lty)
legend("topright", lty = lty, legend = labels, bg = "white")

#
## Lagging and unemployment
#
# Create a one month lag of US unemployment
us_monthlag <- lag(unemployment$us, k = 1)

# Create a one year lag of US unemployment
us_yearlag <- lag(unemployment$us, k =12)

# Merge original unemployment data with your new lags 
unemployment_lags <- merge(unemployment, us_monthlag, us_yearlag)

# View the first 15 rows of unemployment_lags
head(unemployment_lags, n =15)

#
## Differencing unemployment
#
# Generate monthly difference in unemployment
unemployment$us_monthlydiff <- diff(unemployment$us, lag = 1, differences = 1)

# Generate yearly difference in unemployment
unemployment$us_yearlydiff <- diff(unemployment$us, lag = 12, differences = 1)


# Plot US unemployment and annual difference
par(mfrow = c(2,1))
plot.xts(unemployment$us)
plot.xts(unemployment$us_yearlydiff, type = "h") # barplot

#
## Add a discrete rolling sum to GDP data
#
gdp <- readRDS("us_gdp.RData")
# Convert GDP date column to time object
gdp$date <- as.yearqtr(gdp$date)

# Convert GDP data to xts
gdp <- as.xts(gdp[, -1], order.by = gdp$date)
colnames(gdp) <- c("gdp")

## Replace missing data - II using linear approximation

# Fill NAs in gdp_xts using linear approximation
gdp_approx <- na.approx(gdp_xts) 

# Plot your new xts object
plot.xts(gdp_approx, major.format = "%Y")

# Query for GDP in 1993 in gdp_approx
gdp_approx["1993"]


# Add a quarterly difference in gdp
gdp$quarterly_diff <- diff(gdp$gdp, lag = 1, differences = 1)

# Split gdp$quarterly_diff into years
gdpchange_years <- split(gdp$quarterly_diff, f = "years")

# Use lapply to calculate the cumsum each year
gdpchange_ytd <- lapply(gdpchange_years, FUN = cumsum)

# Use do.call to rbind the results
gdpchange_xts <- do.call(rbind, gdpchange_ytd)

# Plot cumulative year-to-date change in GDP
plot.xts(gdpchange_xts, type = "h")