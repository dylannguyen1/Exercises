#
## Dynamic regression
#
data("uschange")

#plot us personal consumption and income
autoplot(uschange[,1:2], facets = TRUE) + xlab("Year")

## relationship between income and consumption
ggplot(aes(x = Income, y = Consumption),
       data = as.data.frame(uschange)) +
  geom_point() +
  ggtitle("Quarterly changes in US consumption and personal income")

## Fitting dynamic regression, add xreg argument
fit <- auto.arima(uschange[,"Consumption"],
                  xreg = uschange[,"Income"])
fit
checkresiduals(fit) #p-value >0.05 -> white noise

fcast <- forecast(fit, xreg = rep(0.8,8)) ## assumefuture income change by 0.8 per quarter for the next 8 quarter
autoplot(fcast) + ylab("Percentage Change")

#
## Forecast sales following expenditure feature
#
library(fma)
data(advert,package = 'fma')
# Time plot of both variables
autoplot(advert, facets = TRUE)

# Fit ARIMA model
fit <- auto.arima(advert[, "sales"], xreg = advert[, "advert"], stationary = TRUE)

fit
# Check model. Increase in sales for each unit increase in advertising
salesincrease <- coefficients (fit)[3]

# Forecast fit as fc
fc <- forecast(fit, xreg = rep(10,6)) # next 6 months of advertising expenditure as 10 units per month

# Plot fc with x and y labels
autoplot(fc) + xlab("Month") + ylab("Sales")

#
## Forecasting electricity demand
#

# Time plots of demand and temperatures
autoplot(elec[, c("Demand", "Temperature")], facets = TRUE)

# Matrix of regressors
xreg <- cbind(MaxTemp = elec[, "Temperature"], 
              MaxTempSq = elec[,"Temperature"]^2, 
              Workday = elec[,"Workday"])

# Fit model
fit <- auto.arima(elec[,'Demand'], xreg = xreg)

# Forecast fit one day ahead
forecast(fit, xreg = cbind(20, 20^2, 1))




