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