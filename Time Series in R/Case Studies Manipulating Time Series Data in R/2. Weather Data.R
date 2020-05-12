#
## Merging time series data by row using rbind()
#
# View the structure of each object
str(temps_1)
str(temps_2)

# View the first and last rows of temps_1
head(temps_1)
tail(temps_1)

# View the first and last rows of temps_2
head(temps_2)
tail(temps_2)

#
## Merging using rbind()
#
# Confirm that the date column in each object is a time-based class
class(temps_1$date)
class(temps_2$date)

# Encode your two temperature data frames as xts objects
temps_1_xts <- as.xts(temps_1[, -4], order.by = temps_1$date)
temps_2_xts <- as.xts(temps_2[,-4], order.by = temps_2$date)

# View the first few lines of each new xts object to confirm they are properly formatted
head(temps_1_xts)
head(temps_2_xts)

# Use rbind to merge your new xts objects
temps_xts <- rbind(temps_1_xts,temps_2_xts)

# View data for the first 3 days of the last month of the first year in temps_xts
first(last(first(temps_xts, "1 year"), "1 month"), "3 days")

#
## Visualising Boston Winters
#
# Identify the periodicity of temps_xts
periodicity(temps_xts)

# Generate a plot of mean Boston temperature for the duration of your data
plot.xts(temps_xts$mean)

# Generate a plot of mean Boston temperature from November 2010 through April 2011
plot.xts(temps_xts$mean["201011/201104"])
lty = c(3,1,3)
# Use plot.zoo to generate a single plot showing mean, max, and min temperatures during the same period 
plot.zoo(temps_xts["201011/201104"], plot.type = "single", lty = lty)

#
## Merging data by column
#
## subset data to include similiar coverage from 2010 to 2015
temps_xts_2 <- temps_xts["2010/2015"]

## convert periodicity from dailty to monthly
temps_monthly <- to.period(temps_xts_2, period = "months")

## using merge() with xts
flights_temps <- merge(flights_xts, temps_monthly)

## check new object
head(flights_temps)

#
##Subsetting and adjusting periodicity / Converting daily data to monthly
#
# Subset your temperature data to include only 2010 through 2015: temps_xts_2
temps_xts_2 <- temps_xts["2010/2015"]

# Use to.period to convert temps_xts_2 to monthly periodicity
# OhLC = false to avoid generating new columns, indexAt to "firstof" to select 1st observation of each month
temps_monthly <- to.period(temps_xts_2, period = "months", OHLC = FALSE, indexAt = "firstof")

# Compare the periodicity and duration of temps_monthly and flights_xts 
periodicity(temps_monthly)
periodicity(flights_xts)

#
##Generating a monthly average
#
# Split temps_xts_2 into separate lists per month from mean column
monthly_split <- split(temps_xts_2$mean , f = "months")

# Use lapply to generate the monthly mean of mean temperatures
mean_of_means <- lapply(monthly_split, FUN = mean)

# Use as.xts to generate an xts object of average monthly temperature data
temps_monthly <- as.xts(as.numeric(mean_of_means), order.by = index)

# Compare the periodicity and duration of your new temps_monthly and flights_xts 
periodicity(temps_monthly)
periodicity(flights_xts)

#
## Using merge() and plotting over time
#
# Use merge to combine your flights and temperature objects
flights_temps <- merge(flights_xts, temps_monthly)

# Examine the first few rows of your combined xts object
head(flights_temps)

lty = c(1,2)
# Use plot.zoo to plot these two columns in a single panel
plot.zoo(flights_temps[,c("pct_delay", "temps_monthly")], plot.type = "single", lty = lty)
legend("topright", lty = lty, legend = labels, bg = "white")

#
## Flight cancellations are more likely in colder months, but flight delays are not strongly related to temperature.
#

#
## Time Series data workflow mergin
#
## encode time series to xts
data_1_xts <- as.xts(data_1,order.by = index)
## Examine and adjust periodicity if necessary
periodicity(data_1_xts)
to.period(data_1_xts, period = "years")
## merge xts objects
merged_data <-merge(data_1_xts, data_2_xts)

#
## 3 steps 1,4,5
#
#Q:Which of the following steps would you take before merging these data with your existing monthly xts object,
#1) Encode the data to an xts object with a time-based index.
#4) Convert the data to monthly periodicity using split() and lapply() to generate monthly averages.
#5) Check the periodicity and duration of your xts objects before using merge().

#
##Expanding your data
#
# Confirm the periodicity and duration of the vis and wind data
periodicity(vis)
periodicity(wind)

# Merge vis and wind with your existing flights_temps data
flights_weather <-  merge(flights_temps,vis,wind)

# View the first few rows of your flights_weather data
head(flights_weather)

#
##
#
#There is no clear relationship between visibility and flight delays.
#Higher wind speeds show a weak correlation with percentage of delayed flights, but further analysis is required.
#Neither wind nor visibility can explain the high percentage of flights delayed in early 2011.
#We have reason to doubt the quality of data on visibility prior to 2012.
