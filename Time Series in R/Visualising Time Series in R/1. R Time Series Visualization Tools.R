#
##plot() function - basic parameters
#
data <-read.csv("dataset.csv")
# Display the first few lines of the data
head(data)

# Display the column names of the data
colnames(data)

# Plot yahoo data and add title
plot(data$yahoo,main ="yahoo")

# Replot yahoo data with labels for X and Y axes
plot(data$yahoo,main ="yahoo",xlab = "date",ylab = "price")

#
## plot() function - basic parameters 2
#
# Plot the second time series and change title
plot(data$microsoft,main = "microsoft")

# Replot with same title, add subtitle, use bars
plot(data$microsoft, main = "microsoft", sub = "Daily closing price since 2015", type = "h")

# Change line color to red
lines(data$microsoft, col = "red")

#
## Current portfolio description
#
