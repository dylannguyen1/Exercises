#
## AR(Autoregressive) and MA(Moving Average) model
#
x<- arima.sim(list(order = c(1,0,0),ar = -0.7),n = 200) # AR
y<- arima.sim(list(order = c(0,0,1),ar = -0.7),n = 200) # MA
par(mfrow=c(1,2))
plot(x, main ="AR(1)")
plot(y, main = "MA(1)")

library(astsa)
## Simulate AR(2) with mean 50
x<- arima.sim(list(order = c(2,0,0),ar = c(1.5,-0.75)),n = 200) +50

## Estimation for time series using ASTSA
x_fit <-sarima(x, p =2, d=0, q=0)
x_fit$ttable ## show t table

## Simulate MA(1) with mean 0
y<- arima.sim(list(order = c(0,0,1),ar = -0.7),n = 200)

y_fit <-sarima(y, p=0, d=0, q= 1)
y_fit$ttable

#
## Fitting an AR(1) model
#
# Generate 100 observations from the AR(1) model
x <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 100) 

# Plot the generated data 
plot(x)

# Plot the sample P/ACF pair
plot(acf2(x))

# Fit an AR(1) to the data and examine the t-table
x_fit <- sarima(x,p=1,d=0,q=0)
x_fit$ttable
#
## Fitting an AR(2) model
#
x <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -.75)), n = 200)
plot(x)
# astsa is preloaded

# Plot x
plot(x)

# Plot the sample P/ACF of x
plot(acf2(x))

# Fit an AR(2) to the data and examine the t-table
x_fitted <- sarima(x,p=2,d=0,q=0)
x_fitted$ttable

#
## Fitting an MA(1) model
#
x <- arima.sim(model = list(order = c(0, 0, 1), ma = -.8), n = 100)

# Plot x
plot(x)

# Plot the sample P/ACF of x
plot(acf2(x))

# Fit an MA(1) to the data and examine the t-table
x_fit <- sarima(x,p=0,d=0,q=1)
x_fit$ttable

#
## ARMA model
#
x_arma <- arima.sim(list(order = c(1,0,1),
                    ar =0.9,
                    ma = -0.4),
                    n = 200)
plot(x_arma, main = "ARMA(1,1)")

x_arma_fit <- sarima(x_arma, p =1, d= 0, q=1)
x_arma_fit$ttable

#
##Fitting ARMA(2,1) model
#
# astsa is preloaded

# Plot x
plot(x)

# Plot the sample P/ACF of x
plot(acf2(x))

# Fit an ARMA(2,1) to the data and examine the t-table
x_arma_fit <- sarima(x,p =2,d=0,q=1)

dl_varve <- diff(log(varve))
acf(dl_varve)
pacf(dl_varve)

##
### Model choice
##
gnpgr <-diff(log(gnp))
x_ar<-sarima(gnpgr, p =1,d=0,q=0) ## fitting AR(1)
x_ar$AIC; x_ar$BIC

x_ma_2 <- sarima(gnpgr, p=0,d=0,q=2) ## fitting MA(2)
x_ma_2$AIC; x_ma_2$BIC
#
## Compare AIC and BIC between MA and ARMA
#
# Fit an MA(1) to dl_varve.   
ma_1_fit <- sarima(dl_varve,p=0,d=0,q=1)
ma_1_fit$AIC; ma_1_fit$BIC
# Fit an MA(2) to dl_varve. Improvement?
ma_2_fit <- sarima(dl_varve,p=0,d=0,q=2)
ma_2_fit$AIC; ma_2_fit$BIC

# Fit an ARMA(1,1) to dl_varve. Improvement?
arma_fit<-sarima(dl_varve,p=1,d=0,q=1)
arma_fit$AIC; arma_fit$BIC

#
##Residual analysis to see MA or ARMA fit better
#
# Fit an MA(1) to dl_varve. Examine the residuals  
sarima(dl_varve,0,0,1)

# Fit an ARMA(1,1) to dl_varve. Examine the residuals
sarima(dl_varve,1,0,1)

#
## ARMA get in, using ARMA in oil data
#
data(oil); attach(oil)
# Calculate approximate oil returns
oil_returns <- diff(log(oil))

# Plot oil_returns. Notice the outliers.
plot(oil_returns)

# Plot the P/ACF pair for oil_returns
plot(acf2(oil_returns))

# Assuming both P/ACF are tailing, fit a model to oil_returns
sarima(oil_returns,1,0,1)