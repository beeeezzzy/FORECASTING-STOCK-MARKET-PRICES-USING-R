#Loading the required libraries
library(quantmod)
library(forecast)
library(tseries)

#Loading the data into R
df <- getSymbols("AAPL",from = '2017-01-01', to = '2020-01-01', auto.assign = FALSE)
summary(df)

price <- df[,4]
price <- na.approx(price)
autoplot(price)

#Model Identification
adf.test(price)
temp <- diff(price,1)
diffprice = na.omit(temp)
autoplot(diffprice)

par(mfrow = c(1,2))
acf(diffprice, lag.max = 20, main = "ACF of Differenced Prices")
pacf(diffprice, lag.max = 20, main = "PACF of Differenced Prices")
adf.test(diffprice)

#Model Implementation and Fourier Implementation
pricearima = ts(price, start = c(2017,01,01), f = 365.25)
fit <- auto.arima(pricearima, xreg = fourier(pricearima, K = 2), allowdrift = TRUE, trace = TRUE, test = "adf", ic = "aic")
summary(fit)

#Forecast Evaluation
fcastvalues <- forecast(fit, h = 20, xreg = fourier(pricearima, K = 2, h = 20))
autoplot(fcastvalues, xlab = "Time", ylab = "AAPL Price", include = 300)
fcastvalues

#Accuracy of the model
accuracy(fcastvalues)

