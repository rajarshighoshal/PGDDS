#Loading the required libraries

library(graphics)
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
plot(timevals, globalpred, col='red', type = "l")

#Now, let's inspect the local component of the time series

localpred <- timeser - globalpred
plot(localpred, col='red', type = "l")