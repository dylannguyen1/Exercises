#
##plot() function - basic parameters
#
data<-read.csv("Daily-stocks-for-YHOO-MSFT-C-and-DOW.csv",sep = " ",header = TRUE)
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
data<-read.csv("Daily-stocks-for-YHOO-MSFT-C-and-DOW.csv",sep = " ",header = TRUE)
# Plot the second time series and change title
plot(data$microsoft,main = "microsoft")

# Replot with same title, add subtitle, use bars
plot(data$microsoft, main = "microsoft", sub = "Daily closing price since 2015", type = "h")

# Change line color to red
lines(data$microsoft, col = "red")


#
##Control graphic parameters
#
library(fpp2)
data<-read.csv("Daily-stocks-for-YHOO-MSFT-C-and-DOW.csv",sep = " ",header = TRUE)
# Plot two charts on same graphical window
par(mfrow = c(2,1))
plot(data$yahoo,main="yahoo")
plot(data$microsoft,main="microsoft")


# Replot with reduced margin to 60% and character sizes to 80%
par(mfrow = c(2, 1), mex = 0.6, cex = 0.8)
plot(data$Index,data$yahoo,main="yahoo")
plot(data$microsoft,main="microsoft")

#
## The function to tailor the chart parameters is par(), the option to change character size is cex, to display more than one chart on a single window you use mfrow
#

#
## Other useful visualizing tools
#
my_ts <- data$yahoo
my_ts2 <- data$microsoft
plot(my_ts,main = "My Stocks")
lines(my_ts2,col = "red")
axis(side = 4, at = pretty(my_ts2)) # scale new axis
legend(x = "bottomright", legend = c("Stock X","Stock Y"),
       col = c("black","red"),lty = c(1,1))
abline(h=10) # value of stock >=10

# highlight certain date with abline
abline(v = as.Date("2016-04-14"))
abline(h = 1) ## h is horizontal line

#highlight a period with Performance Analytics
library(PerformanceAnalytics)
period <- c("2014-01/2015-06")
chart.TimeSeries(my_ts,period.areas = period,main ="my_ts")

#
## Adding an extra series to an existing chart
#
data<-read.csv("Daily-stocks-for-YHOO-MSFT-C-and-DOW.csv",sep = " ",header = TRUE)
# Plot the "microsoft" series
plot(data$microsoft,main = "Stock prices since 2015")

# Add the "dow_chemical" series in red
lines(data$dow_chemical,col = "red")

# Add a Y axis on the right side of the chart
axis(side = 4, at = pretty(data$dow_chemical))

# Add a legend in the bottom right corner
legend(x = "bottomright", legend = c("microsoft","dow_chemical"), col = c("black","red"), lty = c(1,1))

#
##Highlighting events in a time series
#
# Plot the "citigroup" time series
plot(data$citigroup,main = "Citigroup")

# Create vert_line to identify January 4th, 2016 in citigroup
vert_line <- which(index(data$citigroup) == as.Date("2016-01-04"))

# Add a red vertical line using vert_line
abline( v = .index(data$citigroup)[vert_line], col = "red")


# Create hori_line to identify average price of citigroup
hori_line <- mean(data$citigroup)

# Add an average blue horizontal line using hori_line
abline(h = hori_line, col = "blue")

#
## Highlighting a specific period in a time series
#
data<-read.csv("Daily-stocks-for-YHOO-MSFT-C-and-DOW.csv",sep = " ",header = TRUE)
data <- xts(data[,-1], order.by = as.POSIXct(data$Index)) ## convert to time-based
# Create period to hold the 3 months of 2015
period <- c("2015-01/2015-03")

# Highlight the first three months of 2015 
chart.TimeSeries(data$citigroup,period.areas = period)

# Highlight the first three months of 2015 in light grey
chart.TimeSeries(data$citigroup,period.areas = period,period.color ='lightgrey')

#
## Fancy stock chart
#
# Plot the microsoft series
plot(data$microsoft, main = "Dividend date and amount")

# Add the citigroup series
lines(data$citigroup, col = "orange", lwd = 2)

# Add a new y axis for the citigroup series
axis(side = 4, at = pretty(data$citigroup), col = "orange")

# Create the two legend strings
micro <- paste0("Microsoft div. of ", "$0.39"," on ", "15 Nov. 2016")
citi <- paste0("Citigroup div. of ","$0.16"," on ", "13 Nov. 2016")

# Create the legend in the bottom right corner
legend(x = "bottomright", legend = c(micro, citi), col = c("black", "orange"), lty = c(1, 1))

