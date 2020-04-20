library(datasets)
Nile <-as.vector(Nile)
# Print the Nile dataset
print(Nile)

# List the number of observations in the Nile dataset

length(Nile)
# Display the first 10 elements of the Nile dataset
head(Nile, n = 10)

# Display the last 12 elements of the Nile dataset
tail(Nile,n= 12)


## Basic time series plot
# Plot the Nile data
plot(Nile)

# Plot the Nile data with xlab and ylab arguments
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})")

# Plot the Nile data with xlab, ylab, main, and type arguments
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})",main = "Annual River Nile Volume at Aswan, 1871-1970",type = "b" )
#
##What does the time index tell us?
#
# Plot the continuous_series using continuous time indexing
par(mfrow=c(2,1))
plot(continuous_time_index,continuous_series, type = "b")

# Make a discrete time index using 1:20 
discrete_time_index <-c(1:20)

# Now plot the continuous_series using discrete time indexing
plot(discrete_time_index,continuous_series, type = "b")

#
##Identifying the sampling frequency
#
# Plot AirPassengers
data("AirPassengers")
plot(AirPassengers)
# View the start and end dates of AirPassengers
start(AirPassengers)
end(AirPassengers)

# Use time(), deltat(), frequency(), and cycle() with AirPassengers 
time(AirPassengers)
deltat(AirPassengers)
frequency(AirPassengers)
cycle(AirPassengers)


# Compute the mean of AirPassengers
mean(AirPassengers,na.rm = TRUE)

# Impute mean values to NA in AirPassengers for missing data
AirPassengers[85:96] <- mean(AirPassengers, na.rm = TRUE)

# Generate another plot of AirPassengers

plot(AirPassengers)
# Add the complete AirPassengers data to your plot
rm(AirPassengers)
points(AirPassengers, type = "l", col = 2, lty = 3)
#
###Creating a time series object with ts()
# Use print() and plot() to view data_vector
print(data_vector)
plot(data_vector)

# Convert data_vector to a ts object with start = 2004 and frequency = 4
time_series <-  ts(data_vector,start = 2004, frequency = 4)

# Use print() and plot() to view time_series
print(time_series)
plot(time_series)


