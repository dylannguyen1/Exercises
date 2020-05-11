#
## Current portfolio description
#
data<-read.csv("Existing-portfolio.csv",header=TRUE)
data <- xts(data[,-1], order.by = as.POSIXct(data$Index)) ## convert to time-based

# Plot the portfolio value 
plot(data$value, main = "Portfolio Value")

# Plot the portfolio return
plot(data$return, main = "Portfolio Return")

# Plot a histogram of portfolio return 
hist(data$return, probability = TRUE)

# Add a density line in red and twice as thick as normal
lines(density(data$return), col = "red", lwd = 2)

#
## New stock description
#
data <- read.csv("Stock-data-for-GS-KO-DIS-and-CAT.csv",header = TRUE, sep = ",")
data <- xts(data[,-1], order.by = as.POSIXct(data$Index)) ## convert to time-based

# Plot the four stocks on the same window with 80% margin and character size
par(mfrow = c(2,2),mex = 0.8,cex = 0.8)
plot(data$GS)
plot(data$KO)
plot(data$DIS)
plot(data$CAT)


#
## New stock description 2
#
data1<-read.csv("Existing-portfolio.csv",header=TRUE)
portfolio <- data1$return 
data <- read.csv("Stock-data-for-GS-KO-DIS-and-CAT.csv",header = TRUE, sep = ",")

## return of gs,ko,dis and cat
gs <-ROC(data$GS)[-1]
ko <-ROC(data$KO)[-1]
dis <- ROC(data$DIS)[-1]
cat <- ROC(data$CAT)[-1]

## set mfrom to (1,1)
par(mfrow = c(1,1))
# Draw the scatterplot of gs against the portfolio
plot(x = gs, y = portfolio)

# Add a regression line in red with twice as thick as normal
abline(reg = lm(gs ~ portfolio), col = "red", lwd = 2)

# Plot scatterplots and regression lines to a 2x2 window
par(mfrow = c(2, 2))

plot(x = gs, y = portfolio)
abline(reg = lm(gs ~ portfolio), col = "red", lwd = 2)

plot(x = ko, y = portfolio)
abline(reg = lm(ko ~ portfolio), col = "red", lwd = 2)

plot(x = dis, y = portfolio)
abline(reg = lm(dis ~ portfolio), col = "red", lwd = 2)

plot(x = cat, y = portfolio)
abline(reg = lm(cat ~ portfolio), col = "red", lwd = 2)

#
## Compare old and new portfolios
#
old.vs.new.portfolio <- read.csv("old.vs.new.portfolio.csv",header = TRUE, sep = ",")
old.vs.new.portfolio<- xts(old.vs.new.portfolio[,-1], order.by = as.POSIXct(old.vs.new.portfolio$Index)) ## convert to time-based

# Plot new and old portfolio values on same chart
plot(old.vs.new.portfolio$old.portfolio.value,col = "black")
lines(old.vs.new.portfolio$new.portfolio.value, col = "red")

# Plot density of the new and old portfolio returns on same chart
plot(density(old.vs.new.portfolio$old.portfolio.rtn),col = "black")
lines(density(old.vs.new.portfolio$new.portfolio.rtn),col = "red")

#
##A more accurate comparison of portfolios
#
library(PerformanceAnalytics)
# Draw value, return, drawdowns of old portfolio
charts.PerformanceSummary(old.vs.new.portfolio$old.portfolio.rtn)

# Draw value, return, drawdowns of new portfolio
charts.PerformanceSummary(old.vs.new.portfolio$new.portfolio.rtn)

# Draw both portfolios on same chart
charts.PerformanceSummary(old.vs.new.portfolio[, c(3, 4)])

#
## QUIZ
#
##Given that you already own a portfolio of stocks, on what grounds should you add a new stock to your portfolio?
 ##Correlation to your existing portfolio to assess diversification, return histogram to assess risk and box and whisker plot to assess average return


