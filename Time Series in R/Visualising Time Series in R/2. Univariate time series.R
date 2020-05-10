library(TTR)
data<-read.csv("Daily-returns-for-Apple.csv",sep = " ",header = TRUE)
data <- xts(data[,-1], order.by = as.POSIXct(data$Index)) ## convert to time-based
colnames(data) <-"apple"
# Plot Apple's stock price 
plot(data$apple,main = "Apple stock price")

# Create a time series called rtn
rtn <- ROC(data) # ROC = rate of change

# Plot Apple daily price and daily returns 
par(mfrow = c(1,2))
plot(data)
plot(rtn)

