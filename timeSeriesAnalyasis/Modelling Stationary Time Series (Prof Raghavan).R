#Loading the required libraries

require(graphics)
library(forecast)

#loading the file into R

exchange_rate_data <- read.csv("exchange-rate-twi.csv", header = T, sep = ',')

nrow(exchange_rate_data)

#Converting dataframe to time series

timeser <- ts(exchange_rate_data$Exchange.Rate.TWI)
plot(timeser)

#Building final model in R

timevals <- c(1:nrow(exchange_rate_data))

timeseriesdf <- as.data.frame(cbind(timevals, as.vector(timeser)))
colnames(timeseriesdf) <- c('Month', 'Exchange_Rate')

lmfit <- lm(Exchange_Rate ~ Month, data=timeseriesdf)
globalpred <- predict(lmfit, Month=timevals)
lines(timevals, globalpred, col='red', lwd=2)

#Now, let's ispect the local component of the time series

localpred <- timeser - globalpred
plot(localpred, col='red', type = "l")

#Now, let's make predictions for the local component of the time series

acf(localpred)
acf(localpred, type="partial")
armafit <- auto.arima(localpred)

tsdiag(armafit)
armafit

armapred <- fitted(armafit)

class_dec_pred <- ts(globalpred)
plot(timeser, col = "black")
lines(class_dec_pred, col = "red")

resi <- localpred - armapred
plot(resi)

library(tseries)
adf.test(resi,alternative = "stationary")
kpss.test(resi)
