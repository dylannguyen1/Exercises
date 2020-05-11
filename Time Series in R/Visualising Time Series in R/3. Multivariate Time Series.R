#
## Two time series grouped or stacked
#
stocka = c(0.1, 0.4, 0.5, 0.5, 0.2, 0.3, 0.7, 0.8, 0.7, 0.2, 0.1, 0.2)
stockb = 1 - stocka

## trying to create a dataframeportfolio <- data.frame("stocka" = stocka, "stockb" = stockb)
portfolio <- cbind(stocka, stockb)
# Plot stacked barplot
barplot(portfolio)

# Plot grouped barplot
barplot(portfolio, beside = TRUE)

#
## Visualizing bivariate relationships
#
data<-read.csv("Returns-for-XOM-C-MSFT-DOW-and-YHOO.csv",header = TRUE, sep = ",")
sp500 <- data$sp500
citi <- data$citigroup
# Draw the scatterplot
plot(x = sp500,y=citi)

# Draw a regression line
abline(reg = lm(citi~sp500),col = "red", lwd = 2)

#
## Multivariate time series
#
my_data<-read.csv("Returns-for-XOM-C-MSFT-DOW-and-YHOO.csv",header = TRUE, sep = ",")
my_data <- xts(my_data[,-1], order.by = as.POSIXct(data$Index)) ## convert to time-based

#
## Correlation matrix
#
# Create correlation matrix using Pearson method
cor(my_data,method = "pearson")

# Create correlation matrix using Spearman method
cor(my_data,method = "spearman")

#
## Scatterplot matrix
#
# Create scatterplot matrix
pairs(my_data)

# Create upper panel scatterplot matrix
pairs(my_data,lower.panel = NULL)

#
## Correlation plot
#
library(corrplot)
cor_mat <- cor(my_data[,2:6])
par(mfrow = c(1,1))
# Create correlation matrix
corrplot(cor_mat)

# Create correlation matrix with numbers
corrplot(cor_mat,method = "number")

# Create correlation matrix with colors
corrplot(cor_mat,method = "color")

# Create upper triangle correlation matrix
corrplot(cor_mat, method = "number", type = "upper")

#
## Heatmap
#
# Draw heatmap of cor_mat
corrplot(cor_mat,method = "color")

# Draw upper heatmap
corrplot(cor_mat,method = "color",type = "upper")

# Draw the upper heatmap with hclust
corrplot(cor_mat,method = "color",type = "upper",order = "hclust")
